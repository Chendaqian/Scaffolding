# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 项目概述

ExceptionModelGenerator — 一个 F# 编写的代码生成工具，从 `.mt` 模板文件解析异常类层次结构，使用 Roslyn API 自动生成对应的 C# `partial class` 异常类继承体系。

## 构建与运行

```bash
# 构建（需要 .NET 8 SDK）
dotnet build Scaffolding.sln

# 运行
dotnet run --project Src/ExceptionModelGenerator -- <input.mt> --namespace <ns> --output <output.cs>

# 示例
dotnet run --project Src/ExceptionModelGenerator -- Src/ExceptionModelGenerator/Exceptions.mt --namespace MyNamespace --output ./Exceptions.cs
```

无测试项目，无 CI/CD 配置。

## 架构

三阶段管线：`Parser → CodeGen → 文件输出`

- **Parser.fs** — 读取 `.mt` 文件，按缩进层级（4 空格）构建 `ExceptionNode` 树。正则解析每行的类名、属性（含类型和默认值）、消息。
- **CodeGen.fs** — 递归遍历树，用 Roslyn `SyntaxFactory` 构建 C# 语法树，生成 `public partial class` + 构造函数 + 属性，通过 `NormalizeWhitespace()` 输出格式化代码。
- **Program.fs** — 入口，用 Argu 解析 CLI 参数（Input/Namespace/Output），串联 Parser 和 CodeGen。

F# 项目中 `.fs` 文件的编译顺序在 `.fsproj` 的 `<Compile>` 中显式声明，顺序敏感（Parser.fs → CodeGen.fs → Program.fs）。

## 模板语法（.mt 文件）

```text
ClassName("默认消息")                          # 顶层异常，继承 System.Exception
    ChildClass(Property: Type, "消息")         # 缩进 4 空格 = 继承父类
        GrandChild(Code=10010, "消息")         # 支持属性默认值
    SimpleException                            # 无消息、无属性的简写
```

- 属性格式：`Name: FullType`（如 `EntityId: System.Guid`）
- 默认值格式：`Name=value`（如 `Code=10010`）
- 消息用引号包裹，可中英文混用

## 关键依赖

| 包 | 用途 |
|---|---|
| Argu 3.7.0 | F# CLI 参数解析 |
| Microsoft.CodeAnalysis.CSharp 2.3.2 | Roslyn C# 语法树构建 |
| Microsoft.DotNet.ILCompiler 7.0.5 | NativeAOT 编译支持 |
