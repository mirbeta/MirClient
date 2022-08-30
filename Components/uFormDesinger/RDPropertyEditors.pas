unit RDPropertyEditors;
{
  本单元将DesignEditors中定义的编辑器重载一次,
  当用户在非包中使用的时候才可以正常的引用
}
interface
  uses RDComponentEditors, DesignEditors;

  type
  TRDOrdinalProperty        = class(TOrdinalProperty);
  TRDIntegerProperty        = class(TIntegerProperty);
  TRDCharProperty           = class(TCharProperty);
  TRDEnumProperty           = class(TEnumProperty);
  TRDBoolProperty           = class(TBoolProperty);
  TRDInt64Property          = class(TInt64Property);
  TRDFloatProperty          = class(TFloatProperty);
  TRDStringProperty         = class(TStringProperty);
  TRDNestedProperty         = class(TNestedProperty);
  TRDSetElementProperty     = class(TSetElementProperty);
  TRDSetProperty            = class(TSetProperty);
  TRDClassProperty          = class(TClassProperty);
  TRDMethodProperty         = class(TMethodProperty);
  TRDWideStringProperty     = class(TWideStringProperty);
  TRDWideCharProperty       = class(TWideCharProperty);
  TRDComponentProperty      = class(TComponentProperty);
  TRDInterfaceProperty      = class(TInterfaceProperty);
  TRDComponentNameProperty  = class(TComponentNameProperty);
  TRDDateProperty           = class(TDateProperty);
  TRDTimeProperty           = class(TTimeProperty);
  TRDDateTimeProperty       = class(TDateTimeProperty);
  TRDVariantProperty        = class(TVariantProperty);

implementation

end.
