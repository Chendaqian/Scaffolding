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