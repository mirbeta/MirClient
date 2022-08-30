unit RealtorWorld.Linq;

interface

uses
  dxEMF.Linq,
  dxEMF.Linq.Expressions,
  RealtorWorld.Entities;

type

  { IAgentsExpression }

  IAgentsExpression = interface(IdxEntityInfo)
  ['{9C1575FF-51ED-44E6-AB9F-5C1A6C97D802}']
    function ID: TdxLinqExpression;
    function FirstName: TdxLinqExpression;
    function LastName: TdxLinqExpression;
    function Phone: TdxLinqExpression;
    function Email: TdxLinqExpression;
    function Photo: TdxLinqExpression;
  end;

  { IHomesExpression }

  IHomesExpression = interface(IdxEntityInfo)
    function ID: TdxLinqExpression;
    function Address: TdxLinqExpression;
    function Beds: TdxLinqExpression;
    function Baths: TdxLinqExpression;
    function HouseSize: TdxLinqExpression;
    function LotSize: TdxLinqExpression;
    function Price: TdxLinqExpression;
    function Features: TdxLinqExpression;
    function YearBuilt: TdxLinqExpression;
    function &Type: TdxLinqExpression;
    function Status: TdxLinqExpression;
    function Photo: TdxLinqExpression;
  end;

  { IRealtorWorldContext }

  IRealtorWorldContext = interface(IdxDataContext)
  ['{14ADC308-F6CF-4228-8C99-CF252DA37C0A}']
    function Agents: IAgentsExpression;
    function Homes: IHomesExpression;
  end;

implementation

initialization
  TdxLinqExpressionFactory.Register<TAgents, IAgentsExpression>;
  TdxLinqExpressionFactory.Register<THomes, IHomesExpression>;

  TdxLinqExpressionFactory.Register<IRealtorWorldContext>;

finalization
  TdxLinqExpressionFactory.UnRegister([TAgents, THomes]);
  TdxLinqExpressionFactory.UnRegister<IRealtorWorldContext>;

end.
