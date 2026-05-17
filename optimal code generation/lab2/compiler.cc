#include <memory>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

int main() {
    auto context = std::make_unique<llvm::LLVMContext>();
    auto builder = std::make_unique<llvm::IRBuilder<>>(*context);
    auto module = std::make_unique<llvm::Module>("a + b", *context);

    llvm::FunctionType* func_type =
        llvm::FunctionType::get(llvm::Type::getInt32Ty(*context), {}, false);
    llvm::Function* main_func =
        llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, "main", module.get());

    llvm::BasicBlock* entry = llvm::BasicBlock::Create(*context, "entrypoint", main_func);
    builder->SetInsertPoint(entry);

    llvm::Value* ret_val = builder->CreateAdd(builder->getInt32(353), builder->getInt32(48));
    builder->CreateRet(ret_val);

    module->dump();

    return 0;
}
