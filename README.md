# Scaffolding

> 一个异常类自动生成项目

## 目录结构

```java
├─ README.md
├─ Scaffolding.sln
└─ Src
       └─ ExceptionModelGenerator
              ├─ CodeGen.fs
              ├─ ExceptionModelGenerator.fsproj
              ├─ Exceptions.mt
              ├─ Parser.fs
              └─ Program.fs
```

## 使用方法

例如我们在项目生成时间中指定生成事件，项目右键 -> 属性 -> 生成事件 -> 生成前事件中填写。或者直接编辑项目的`csproj`文件添加如下代码

```xml
<PropertyGroup>
    <PreBuildEvent>$(ProjectDir)..\..\Generators\ExceptionModelGenerator.exe  $(ProjectDir)Exceptions\Exceptions.mt --namespace namespaceABC --output $(ProjectDir)Exceptions\Exceptions.cs</PreBuildEvent>
</PropertyGroup>
```

### 模板文件

使用据有层次的异常定义

```xml
FrameworkException("An exception from user framework")
    DevelopmentException("开发异常")
    DbUnhandledException("An unhandled exception caught from db")
    AttributeNotFoundException("The required attribute not found")
    BusinessException(Code: System.Int32)
        FieldException("字段值非法")
        BrokenRuleException(Code=10010, "An exception for broken rules")
        RequiredFieldMissingException
        BrokenTransactionException
    DataAccessException("An exception from DataAccess")
        EntityNotFoundException(EntityId: System.Guid, EntityType: System.Type, "指定的实体不存在")
        EntitySaveFailedException(EntityId: System.Int32, EntityType: System.Type, "保存实体状态时操作失败")
        EntityDeleteFailedException(EntityId: System.Int32, EntityType: System.Type, "删除实体时操作失败")
    CacheRequestResolverException
```

### 最终生成的Exception Class

```csharp

namespace namespaceABC
{
    using System;

    public partial class FrameworkException : System.Exception
    {
        public FrameworkException(string message = default (string), Exception innerException = default (Exception)): base (message: message ?? $"An exception from user framework", innerException: innerException)
        {
        }
    }

    public partial class DevelopmentException : FrameworkException
    {
        public DevelopmentException(string message = default (string), Exception innerException = default (Exception)): base (message: message ?? $"开发异常", innerException: innerException)
        {
        }
    }

    public partial class DbUnhandledException : FrameworkException
    {
        public DbUnhandledException(string message = default (string), Exception innerException = default (Exception)): base (message: message ?? $"An unhandled exception caught from db", innerException: innerException)
        {
        }
    }

    public partial class AttributeNotFoundException : FrameworkException
    {
        public AttributeNotFoundException(string message = default (string), Exception innerException = default (Exception)): base (message: message ?? $"The required attribute not found", innerException: innerException)
        {
        }
    }

    public partial class BusinessException : FrameworkException
    {
        public BusinessException(System.Int32 code, string message = default (string), Exception innerException = default (Exception)): base (message: message ?? $"", innerException: innerException)
        {
            this.Code = code;
        }

        public System.Int32 Code
        {
            get;
            private set;
        }
    }

    public partial class FieldException : BusinessException
    {
        public FieldException(string message = default (string), Exception innerException = default (Exception), System.Int32 code = default (System.Int32)): base (message: message ?? $"字段值非法", innerException: innerException, code: code)
        {
        }
    }

    public partial class BrokenRuleException : BusinessException
    {
        public BrokenRuleException(string message = default (string), Exception innerException = default (Exception), System.Int32 code = 10010): base (message: message ?? $"An exception for broken rules", innerException: innerException, code: code)
        {
        }
    }

    public partial class RequiredFieldMissingException : BusinessException
    {
        public RequiredFieldMissingException(string message = default (string), Exception innerException = default (Exception), System.Int32 code = default (System.Int32)): base (message: message ?? $"", innerException: innerException, code: code)
        {
        }
    }

    public partial class BrokenTransactionException : BusinessException
    {
        public BrokenTransactionException(string message = default (string), Exception innerException = default (Exception), System.Int32 code = default (System.Int32)): base (message: message ?? $"", innerException: innerException, code: code)
        {
        }
    }

    public partial class DataAccessException : FrameworkException
    {
        public DataAccessException(string message = default (string), Exception innerException = default (Exception)): base (message: message ?? $"An exception from DataAccess", innerException: innerException)
        {
        }
    }

    public partial class EntityNotFoundException : DataAccessException
    {
        public EntityNotFoundException(System.Guid entityId, System.Type entityType, string message = default (string), Exception innerException = default (Exception)): base (message: message ?? $"指定的实体不存在", innerException: innerException)
        {
            this.EntityId = entityId;
            this.EntityType = entityType;
        }

        public System.Guid EntityId
        {
            get;
            private set;
        }

        public System.Type EntityType
        {
            get;
            private set;
        }
    }

    public partial class EntitySaveFailedException : DataAccessException
    {
        public EntitySaveFailedException(System.Int32 entityId, System.Type entityType, string message = default (string), Exception innerException = default (Exception)): base (message: message ?? $"保存实体状态时操作失败", innerException: innerException)
        {
            this.EntityId = entityId;
            this.EntityType = entityType;
        }

        public System.Int32 EntityId
        {
            get;
            private set;
        }

        public System.Type EntityType
        {
            get;
            private set;
        }
    }

    public partial class EntityDeleteFailedException : DataAccessException
    {
        public EntityDeleteFailedException(System.Int32 entityId, System.Type entityType, string message = default (string), Exception innerException = default (Exception)): base (message: message ?? $"删除实体时操作失败", innerException: innerException)
        {
            this.EntityId = entityId;
            this.EntityType = entityType;
        }

        public System.Int32 EntityId
        {
            get;
            private set;
        }

        public System.Type EntityType
        {
            get;
            private set;
        }
    }

    public partial class CacheRequestResolverException : FrameworkException
    {
        public CacheRequestResolverException(string message = default (string), Exception innerException = default (Exception)): base (message: message ?? $"", innerException: innerException)
        {
        }
    }
}
```

