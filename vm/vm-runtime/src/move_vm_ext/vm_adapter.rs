// Copyright (c) The Starcoin Core Contributors
// SPDX-License-Identifier: Apache-2.0

use move_binary_format::{
    access::ModuleAccess, compatibility::Compatibility, errors::*, normalized, CompiledModule,
    IndexKind,
};
use move_core_types::{
    account_address::AccountAddress,
    ident_str,
    identifier::{IdentStr, Identifier},
    language_storage::{ModuleId, TypeTag},
    resolver::*,
    value::MoveValue,
    vm_status::StatusCode,
};
use move_vm_runtime::{
    loader::Function,
    session::{LoadedFunctionInstantiation, SerializedReturnValues, Session},
};
use move_vm_types::{gas::GasMeter, loaded_data::runtime_types::Type};
use once_cell::sync::Lazy;
use starcoin_logger::prelude::debug;
use starcoin_vm_types::language_storage::FunctionId;
use std::{
    borrow::Borrow,
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};
use tracing::warn;

type ConstructorMap = Lazy<BTreeMap<String, FunctionId>>;

static NEW_ALLOWED_STRUCTS: ConstructorMap = Lazy::new(|| {
    [
        (
            "0x1::string::String",
            FunctionId {
                module: ModuleId::new(AccountAddress::ONE, Identifier::from(ident_str!("string"))),
                function: Identifier::from(ident_str!("utf8")),
            },
        ),
        // not implemented in starcoin-framework
        (
            "0x1::object::Object",
            FunctionId {
                module: ModuleId::new(AccountAddress::ONE, Identifier::from(ident_str!("object"))),
                function: Identifier::from(ident_str!("address_to_object")),
            },
        ),
        // not implemented in starcoin-framework
        (
            "0x1::option::Option",
            FunctionId {
                module: ModuleId::new(AccountAddress::ONE, Identifier::from(ident_str!("option"))),
                function: Identifier::from(ident_str!("from_vec")),
            },
        ),
        (
            "0x1::fixed_point32::FixedPoint32",
            FunctionId {
                module: ModuleId::new(
                    AccountAddress::ONE,
                    Identifier::from(ident_str!("fixed_point32")),
                ),
                function: Identifier::from(ident_str!("create_from_raw_value")),
            },
        ),
        (
            "0x1::fixed_point64::FixedPoint64",
            FunctionId {
                module: ModuleId::new(
                    AccountAddress::ONE,
                    Identifier::from(ident_str!("fixed_point64")),
                ),
                function: Identifier::from(ident_str!("create_from_raw_value")),
            },
        ),
    ]
    .into_iter()
    .map(|(s, validator)| (s.to_string(), validator))
    .collect()
});

fn get_allowed_structs() -> &'static ConstructorMap {
    &NEW_ALLOWED_STRUCTS
}

/// Publish module bundle options
/// - force_publish: force publish without compatibility check.
/// - only_new_module: cannot only publish new module, update existing modules is not allowed.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
pub struct PublishModuleBundleOption {
    pub force_publish: bool,
    pub only_new_module: bool,
}

/// An adapter for wrap MoveVM Session
pub struct SessionAdapter<'r, 'l, R> {
    pub(crate) session: Session<'r, 'l, R>,
}

impl<'r, 'l, R> From<Session<'r, 'l, R>> for SessionAdapter<'r, 'l, R> {
    fn from(s: Session<'r, 'l, R>) -> Self {
        Self { session: s }
    }
}

#[allow(clippy::from_over_into)]
impl<'r, 'l, R> Into<Session<'r, 'l, R>> for SessionAdapter<'r, 'l, R> {
    fn into(self) -> Session<'r, 'l, R> {
        self.session
    }
}

impl<'r, 'l, R> AsRef<Session<'r, 'l, R>> for SessionAdapter<'r, 'l, R> {
    fn as_ref(&self) -> &Session<'r, 'l, R> {
        &self.session
    }
}

impl<'r, 'l, R> AsMut<Session<'r, 'l, R>> for SessionAdapter<'r, 'l, R> {
    fn as_mut(&mut self) -> &mut Session<'r, 'l, R> {
        &mut self.session
    }
}

impl<'r, 'l, R: MoveResolver> SessionAdapter<'r, 'l, R> {
    pub fn new(session: Session<'r, 'l, R>) -> Self {
        Self { session }
    }

    /// wrapper of Session, push signer as the first argument of function.
    pub fn validate_and_execute_entry_function(
        &mut self,
        module: &ModuleId,
        function_name: &IdentStr,
        ty_args: Vec<TypeTag>,
        args: Vec<impl Borrow<[u8]>>,
        gas_meter: &mut impl GasMeter,
        sender: AccountAddress,
    ) -> VMResult<SerializedReturnValues> {
        let (_, func, loaded_func) = self
            .session
            .load_function(module, function_name, &ty_args)?;

        self.validate_combine_singer_and_args(vec![sender], args.as_ref(), &loaded_func)?;

        let final_args = Self::check_and_rearrange_args_by_signer_position(
            func,
            args.into_iter().map(|b| b.borrow().to_vec()).collect(),
            sender,
        )?;
        self.session
            .execute_entry_function(module, function_name, ty_args, final_args, gas_meter)
    }

    /// wrapper of Session, push signer as the first argument of function.
    pub fn validate_and_execute_script(
        &mut self,
        script: impl Borrow<[u8]>,
        ty_args: Vec<TypeTag>,
        args: Vec<impl Borrow<[u8]>>,
        gas_meter: &mut impl GasMeter,
        sender: AccountAddress,
    ) -> VMResult<SerializedReturnValues> {
        let (main, loaded_func) = self.session.load_script(script.borrow(), ty_args.clone())?;

        self.validate_combine_singer_and_args(vec![sender], args.as_ref(), &loaded_func)?;

        let final_args = Self::check_and_rearrange_args_by_signer_position(
            main,
            args.into_iter().map(|b| b.borrow().to_vec()).collect(),
            sender,
        )?;
        self.session
            .execute_script(script, ty_args, final_args, gas_meter)
    }

    fn check_and_rearrange_args_by_signer_position(
        func: Arc<Function>,
        args: Vec<Vec<u8>>,
        sender: AccountAddress,
    ) -> VMResult<Vec<Vec<u8>>> {
        let has_signer = func
            .parameter_types()
            .iter()
            .position(|i| matches!(i, &Type::Signer))
            .map(|pos| {
                if pos != 0 {
                    Err(
                        PartialVMError::new(StatusCode::NUMBER_OF_SIGNER_ARGUMENTS_MISMATCH)
                            .with_message(format!(
                                "Expected signer arg is this first arg, but got it at {}",
                                pos + 1
                            ))
                            .finish(Location::Undefined),
                    )
                } else {
                    Ok(true)
                }
            })
            .unwrap_or(Ok(false))?;

        if has_signer {
            let signer = MoveValue::Signer(sender);
            let mut final_args = vec![signer
                .simple_serialize()
                .expect("serialize signer should success")];
            final_args.extend(args);
            Ok(final_args)
        } else {
            Ok(args)
        }
    }

    /// Publish module bundle with custom option.
    /// The code is copied from `VMRuntime::publish_module_bundle` with modification to support ModuleBundleVerifyOption.
    pub fn publish_module_bundle_with_option(
        &mut self,
        modules: Vec<Vec<u8>>,
        sender: AccountAddress,
        gas_meter: &mut impl GasMeter,
        option: PublishModuleBundleOption,
    ) -> VMResult<()> {
        let compiled_modules =
            self.verify_module_bundle(modules.clone(), sender, gas_meter, option)?;

        let mut clean_cache = false;
        // All modules verified, publish them to data cache
        for (module, blob) in compiled_modules.into_iter().zip(modules.into_iter()) {
            let republish = if self.session.exists_module(&module.self_id())? {
                clean_cache = true;
                true
            } else {
                false
            };
            self.session
                .publish_module_to_data_cache(&module.self_id(), blob, republish)?;
        }

        // Clear vm runtimer loader's cache to reload new modules from state cache
        if clean_cache {
            self.empty_loader_cache()?;
        }
        Ok(())
    }

    /// Verify module bundle.
    /// The code is copied from `move_vm:::VMRuntime::publish_module_bundle` with modification to support ModuleBundleVerifyOption.
    pub fn verify_module_bundle(
        &mut self,
        modules: Vec<Vec<u8>>,
        sender: AccountAddress,
        _gas_meter: &mut impl GasMeter,
        option: PublishModuleBundleOption,
    ) -> VMResult<Vec<CompiledModule>> {
        // deserialize the modules. Perform bounds check. After this indexes can be
        // used with the `[]` operator
        let compiled_modules = match modules
            .iter()
            .map(|blob| CompiledModule::deserialize(blob))
            .collect::<PartialVMResult<Vec<_>>>()
        {
            Ok(modules) => modules,
            Err(err) => {
                warn!("[VM] module deserialization failed {:?}", err);
                return Err(err.finish(Location::Undefined));
            }
        };

        // Make sure all modules' self addresses matches the transaction sender. The self address is
        // where the module will actually be published. If we did not check this, the sender could
        // publish a module under anyone's account.
        for module in &compiled_modules {
            if module.address() != &sender {
                return Err(verification_error(
                    StatusCode::MODULE_ADDRESS_DOES_NOT_MATCH_SENDER,
                    IndexKind::AddressIdentifier,
                    module.self_handle_idx().0,
                )
                .finish(Location::Undefined));
            }
        }

        // Collect ids for modules that are published together
        let mut bundle_unverified = BTreeSet::new();

        // For now, we assume that all modules can be republished, as long as the new module is
        // backward compatible with the old module.
        //
        // TODO: in the future, we may want to add restrictions on module republishing, possibly by
        // changing the bytecode format to include an `is_upgradable` flag in the CompiledModule.
        for module in &compiled_modules {
            let module_id = module.self_id();
            if self.session.exists_module(&module_id)? {
                if option.only_new_module {
                    warn!(
                        "[VM] module {:?} already exists. Only allow publish new modules",
                        module_id
                    );
                    return Err(PartialVMError::new(StatusCode::INVALID_MODULE_PUBLISHER)
                        .at_index(IndexKind::ModuleHandle, module.self_handle_idx().0)
                        .finish(Location::Undefined));
                }

                let old_module_ref = self.session.load_module(&module_id)?;
                let old_module = old_module_ref.module();
                let old_m = normalized::Module::new(old_module);
                let new_m = normalized::Module::new(&module);
                if Compatibility::new(true, true, false)
                    .check(&old_m, &new_m)
                    .is_err()
                    && !option.force_publish
                {
                    return Err(PartialVMError::new(
                        StatusCode::BACKWARD_INCOMPATIBLE_MODULE_UPDATE,
                    )
                    .finish(Location::Undefined));
                }
            }
            if !bundle_unverified.insert(module_id) {
                return Err(PartialVMError::new(StatusCode::DUPLICATE_MODULE_NAME)
                    .finish(Location::Undefined));
            }
        }

        // Perform bytecode and loading verification. Modules must be sorted in topological order.
        self.session
            .verify_module_bundle_for_publication(&compiled_modules)?;
        Ok(compiled_modules)
    }

    pub fn verify_script_args(
        &mut self,
        script: Vec<u8>,
        ty_args: Vec<TypeTag>,
        args: Vec<Vec<u8>>,
        sender: AccountAddress,
    ) -> VMResult<()> {
        //load the script, perform verification
        let (
            main,
            LoadedFunctionInstantiation {
                type_arguments: _,
                parameters,
                return_,
            },
        ) = self.session.load_script(script.borrow(), ty_args)?;

        Self::check_script_return(return_)?;

        self.check_script_signer_and_build_args(main, parameters, args, sender)?;

        Ok(())
    }

    pub fn verify_script_function_args(
        &mut self,
        module: &ModuleId,
        function_name: &IdentStr,
        ty_args: Vec<TypeTag>,
        args: Vec<Vec<u8>>,
        sender: AccountAddress,
    ) -> VMResult<()> {
        let (
            _module,
            func,
            LoadedFunctionInstantiation {
                type_arguments: _,
                parameters,
                return_,
            },
        ) = self
            .session
            .load_function(module, function_name, &ty_args)?;

        Self::check_script_return(return_)?;

        self.check_script_signer_and_build_args(func, parameters, args, sender)?;

        Ok(())
    }

    //ensure the script function not return value
    fn check_script_return(return_: Vec<Type>) -> VMResult<()> {
        return if !return_.is_empty() {
            Err(PartialVMError::new(StatusCode::RET_TYPE_MISMATCH_ERROR)
                .with_message(format!(
                    "Expected script function should not return value, but got {:?}",
                    return_
                ))
                .finish(Location::Undefined))
        } else {
            Ok(())
        };
    }

    fn check_script_signer_and_build_args(
        &self,
        func: Arc<Function>,
        arg_tys: Vec<Type>,
        args: Vec<Vec<u8>>,
        sender: AccountAddress,
    ) -> VMResult<()> {
        let final_args = Self::check_and_rearrange_args_by_signer_position(func, args, sender)?;
        let (_, _) = self.session.deserialize_args(arg_tys, final_args)?;

        Ok(())
    }

    /// Clear vm runtime loader's cache to reload new modules from state cache
    fn empty_loader_cache(&self) -> VMResult<()> {
        self.session.mark_loader_cache_as_invaliddated();
        self.session.flush_loader_cache_if_invalidated();
        Ok(())
    }

    // Return whether the argument is valid/allowed and whether it needs construction.
    fn is_valid_txn_arg(&self, typ: &Type, allowed_structs: &ConstructorMap) -> bool {
        use move_vm_types::loaded_data::runtime_types::Type::*;

        match typ {
            Bool | U8 | U16 | U32 | U64 | U128 | U256 | Address => true,
            Vector(inner) => self.is_valid_txn_arg(inner, allowed_structs),
            Struct(idx) | StructInstantiation(idx, _) => self
                .session
                .get_struct_type(*idx)
                .map(|st| {
                    let full_name = format!("{}::{}", st.module.short_str_lossless(), st.name);
                    allowed_structs.contains_key(&full_name)
                })
                .unwrap_or_default(),
            Signer | Reference(_) | MutableReference(_) | TyParam(_) => false,
        }
    }

    /// Validate and generate args for entry function
    /// validation includes:
    /// 1. return signature is empty
    /// 2. number of signers is same as the number of senders
    /// 3. check arg types are allowed after signers
    ///
    fn validate_combine_singer_and_args(
        &self,
        senders: Vec<AccountAddress>,
        args: &[impl Borrow<[u8]>],
        func: &LoadedFunctionInstantiation,
    ) -> VMResult<()> {
        debug!("validate script function");
        Self::check_script_return(func.return_.clone())?;

        let mut signer_param_cnt = 0;
        // find all signer params at the beginning
        for ty in func.parameters.iter() {
            match ty {
                Type::Signer => signer_param_cnt += 1,
                Type::Reference(inner_type) => {
                    if matches!(&**inner_type, Type::Signer) {
                        signer_param_cnt += 1;
                    }
                }
                _ => (),
            }
        }

        let allowed_structs = get_allowed_structs();
        // Need to keep this here to ensure we return the historic correct error code for replay
        for ty in func.parameters[signer_param_cnt..].iter() {
            let valid =
                self.is_valid_txn_arg(&ty.subst(&func.type_arguments).unwrap(), allowed_structs);
            if !valid {
                return Err(
                    PartialVMError::new(StatusCode::INVALID_MAIN_FUNCTION_SIGNATURE)
                        .finish(Location::Undefined),
                );
            }
        }

        if (signer_param_cnt + args.len()) != func.parameters.len() {
            return Err(
                PartialVMError::new(StatusCode::NUMBER_OF_ARGUMENTS_MISMATCH)
                    .finish(Location::Undefined),
            );
        }

        // If the invoked function expects one or more signers, we need to check that the number of
        // signers actually passed is matching first to maintain backward compatibility before
        // moving on to the validation of non-signer args.
        // the number of txn senders should be the same number of signers
        if signer_param_cnt > 0 && senders.len() != signer_param_cnt {
            return Err(
                PartialVMError::new(StatusCode::NUMBER_OF_SIGNER_ARGUMENTS_MISMATCH)
                    .finish(Location::Undefined),
            );
        }

        // This also validates that the args are valid. If they are structs, they have to be allowed
        // and must be constructed successfully. If construction fails, this would fail with a
        // FAILED_TO_DESERIALIZE_ARGUMENT error.
        let args = construct_args(
            &mut self.session,
            &func.parameters[signer_param_cnt..],
            args,
            &func.type_arguments,
            allowed_structs,
            false,
        )?;

        Ok(())
    }
}
