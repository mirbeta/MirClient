unit EMFQueryEntity;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, StdCtrls, Menus, DB, DBClient, cxClasses, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit,
  dxLayoutLookAndFeels, dxUIAdorners, cxMemo, cxLabel, cxButtons, cxTextEdit, cxRichEdit, dxLayoutContainer,
  dxLayoutControl, EMFCustomTutorial;

type
  { TfrmEMFQueryEntity }

  TfrmEMFQueryEntity = class(TfrmEMFCustomTutorial)
    btnGetByKey: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    btnGetByCriteria: TcxButton;
    dxLayoutItem5: TdxLayoutItem;
    btnGetByPredicate: TcxButton;
    dxLayoutItem6: TdxLayoutItem;
    btnGetByLINQ: TcxButton;
    dxLayoutItem7: TdxLayoutItem;
    procedure btnGetByKeyClick(Sender: TObject);
    procedure btnGetByCriteriaClick(Sender: TObject);
    procedure btnGetByPredicateClick(Sender: TObject);
    procedure btnGetByLINQClick(Sender: TObject);
  protected
    function GetDescription: string; override;
    procedure GenerateContent; override;
    procedure RegisterActions; override;
    procedure UnregisterActions; override;
  public
    class function GetCaption: string; override;
    class function GetGroupID: Integer; override;
    class function GetID: Integer; override;
  end;

implementation

{$R *.dfm}

uses
  uMain, DateUtils, dxCore, dxEMF.DB.Criteria, dxEMF.Types, dxEMF.LINQ, EMFDemoClasses;

{ TfrmEMFQueryEntity }

class function TfrmEMFQueryEntity.GetCaption: string;
begin
  Result := 'Querying an Entity Object';
end;

function TfrmEMFQueryEntity.GetDescription: string;
begin
  Result := 'This tutorial shows how to query a single entity object using either ' +
    'the session component''s overloaded Find function or a LINQ expression.';
end;

class function TfrmEMFQueryEntity.GetID: Integer;
begin
  Result := 5;
end;

class function TfrmEMFQueryEntity.GetGroupID: Integer;
begin
  Result := EMFTreeViewTutorialEntityQueryingGroupID;
end;

procedure TfrmEMFQueryEntity.GenerateContent;
begin
  inherited GenerateContent;
  GenerateMails;
end;

procedure TfrmEMFQueryEntity.RegisterActions;
begin
  inherited RegisterActions;
  RegisterTutorialAction(btnGetByKey, 'This code retrieves a mail message (a TMail entity object) using the session component''s Find function and the object''s a key value passed as a parameter.', ['query object by key value', 'TMail|TMail']);
  RegisterTutorialAction(btnGetByCriteria, 'This code retrieves a TMail entity object using the session component''s Find function and a criteria operator passed as a parameter.', ['query object using criteria operator', 'TMail|TMail']);
  RegisterTutorialAction(btnGetByPredicate, 'This code retrieves a TMail entity object using the session component''s Find function and a predicate (anonymous Boolean function) passed as a parameter.', ['query object using predicate', 'TMail|TMail']);
  RegisterTutorialAction(btnGetByLINQ, 'This code retrieves a TMail entity object using a LINQ expression. The expression''s First method call can be omitted since the ID property is a primary key and all its values are unique.', ['query object using linq expression', 'IDemoContext|IDemoContext', 'IMailExpression|IMailExpression']);
end;

procedure TfrmEMFQueryEntity.UnregisterActions;
begin
  UnregisterTutorialAction(btnGetByLINQ);
  UnregisterTutorialAction(btnGetByPredicate);
  UnregisterTutorialAction(btnGetByCriteria);
  UnregisterTutorialAction(btnGetByKey);
  inherited UnregisterActions;
end;

//<query object using criteria operator
// SELECT * FROM MAIL WHERE ID = 2

procedure TfrmEMFQueryEntity.btnGetByCriteriaClick(Sender: TObject);
var
  AMail: TMail;
begin
  AMail := EMFSession.Find<TMail>(TdxBinaryOperator.Create('ID', '2'));

  ShowQueryResults<TMail>(AMail, 'Query criteria: (ID = 2)');
end;
//>query object using criteria operator

//<query object by key value
// SELECT * FROM MAIL WHERE ID = 1

procedure TfrmEMFQueryEntity.btnGetByKeyClick(Sender: TObject);
var
  AMail: TMail;
begin
  AMail := EMFSession.Find<TMail>(1);

  ShowQueryResults<TMail>(AMail, 'Query criteria: (ID = 1)');
end;
//>query object by key value

//<query object using predicate
// SELECT * FROM MAIL WHERE ID = 3

procedure TfrmEMFQueryEntity.btnGetByPredicateClick(Sender: TObject);
var
  AMail: TMail;
begin
  AMail := EMFSession.Find<TMail>(
    function (AMail: TMail): Boolean
    begin
      Result := AMail.ID = 3;
    end);

  ShowQueryResults<TMail>(AMail, 'Query criteria: (ID = 3)');
end;
//>query object using predicate

//<query object using linq expression
// SELECT * FROM MAIL WHERE ID = 4

procedure TfrmEMFQueryEntity.btnGetByLINQClick(Sender: TObject);
var
  AMailExpression: IMailExpression;
  AQuery: IdxQueryable;
  AMail: TObject;
begin
  AMailExpression := EMFSession.GetDataContext<IDemoContext>.Mail;

  AQuery := Linq.
    From(AMailExpression).
    Where(AMailExpression.ID = 4).
    Select.
    First;

  for AMail in AQuery do
    ShowQueryResults<TMail>(AMail as TMail, 'Query criteria: (ID = 4)');
end;
//>query object using linq expression

initialization
  TfrmEMFQueryEntity.Register;

end.
