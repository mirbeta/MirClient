unit EMFQueryEntitiesUsingLINQ;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, StdCtrls, Menus, DB, DBClient, cxClasses, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit,
  dxLayoutLookAndFeels, dxUIAdorners, cxMemo, cxLabel, cxButtons, cxTextEdit, cxRichEdit, dxLayoutContainer,
  dxLayoutControl, EMFCustomTutorial;

type
  { TfrmEMFQueryEntitiesUsingLINQ }

  TfrmEMFQueryEntitiesUsingLINQ = class(TfrmEMFCustomTutorial)
    btnSelect: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    btnComplexCondition: TcxButton;
    dxLayoutItem4: TdxLayoutItem;
    btnTake: TcxButton;
    dxLayoutItem5: TdxLayoutItem;
    btnTakeAndSkip: TcxButton;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    btnSorted: TcxButton;
    procedure btnSelectClick(Sender: TObject);
    procedure btnSortedClick(Sender: TObject);
    procedure btnComplexConditionClick(Sender: TObject);
    procedure btnTakeClick(Sender: TObject);
    procedure btnTakeAndSkipClick(Sender: TObject);
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
  uMain, dxCore, dxEMF.Types, dxEMF.Linq, dxEMF.Linq.Expressions, EMFDemoClasses;

{ TfrmEMFQueryEntitiesUsingLINQ }

procedure TfrmEMFQueryEntitiesUsingLINQ.GenerateContent;
begin
  inherited;
  GenerateMails;
end;

procedure TfrmEMFQueryEntitiesUsingLINQ.RegisterActions;
begin
  inherited RegisterActions;
  RegisterTutorialAction(btnSelect, 'This code retrieves all mail messages (all records from the Mail table) as TMail entity objects.', ['linq query all records', 'IDemoContext|IDemoContext', 'IMailExpression|IMailExpression']);
  RegisterTutorialAction(btnSorted, 'This code retrieves all TMail entity objects and sorts them in descending order by SENT and in ascending order by PRIORITY and FROM.', ['linq sorted query', 'IDemoContext|IDemoContext', 'IMailExpression|IMailExpression']);
  RegisterTutorialAction(btnComplexCondition, 'This code retrieves all high-priority mail messages (TMail entity objects) sent in 2017.', ['linq query criteria', 'IDemoContext|IDemoContext', 'IMailExpression|IMailExpression']);
  RegisterTutorialAction(btnTake, 'This code retrieves the five most recent TMail entity objects using the TAKE query function. Note that this function requires sorting entity objects.', ['linq query take function', 'IDemoContext|IDemoContext', 'IMailExpression|IMailExpression']);
  RegisterTutorialAction(btnTakeAndSkip, 'This code uses the TAKE and SKIP query functions to retrieve the next three TMail entity objects skipping the first two objects. Note that these functions require sorting entity objects.', ['linq query take and skip functions', 'IDemoContext|IDemoContext', 'IMailExpression|IMailExpression']);
end;

procedure TfrmEMFQueryEntitiesUsingLINQ.UnregisterActions;
begin
  UnregisterTutorialAction(btnTakeAndSkip);
  UnregisterTutorialAction(btnTake);
  UnregisterTutorialAction(btnComplexCondition);
  UnregisterTutorialAction(btnSorted);
  UnregisterTutorialAction(btnSelect);
  inherited UnregisterActions;
end;

class function TfrmEMFQueryEntitiesUsingLINQ.GetCaption: string;
begin
  Result := 'Querying Data Using LINQ Expressions';
end;

function TfrmEMFQueryEntitiesUsingLINQ.GetDescription: string;
begin
  Result := 'This tutorial shows how to query entity objects using LINQ ' +
    'expressions. Entity objects are retrieved by enumerating the expressions.';
end;

class function TfrmEMFQueryEntitiesUsingLINQ.GetGroupID: Integer;
begin
  Result := EMFTreeViewTutorialEntityQueryingGroupID
end;

class function TfrmEMFQueryEntitiesUsingLINQ.GetID: Integer;
begin
  Result := 17;
end;

//<linq query all records
// SELECT * FROM MAIL

procedure TfrmEMFQueryEntitiesUsingLINQ.btnSelectClick(Sender: TObject);
var
  AMailExpression: IMailExpression;
  AQuery: IdxQueryable;
begin
  AMailExpression := EMFSession.GetDataContext<IDemoContext>.Mail;

  AQuery := Linq.
    From(AMailExpression).
    Select;

  ShowQueryResults<TMail>(AQuery, 'Query criteria: (All records)');
end;
//>linq query all records

//<linq sorted query
// SELECT * FROM MAIL ORDER BY SENT DESC, PRIORITY ASC, FROM ASC

procedure TfrmEMFQueryEntitiesUsingLINQ.btnSortedClick(Sender: TObject);
var
  AMailExpression: IMailExpression;
  AQuery: IdxQueryable;
begin
  AMailExpression := EMFSession.GetDataContext<IDemoContext>.Mail;

  AQuery := Linq.
    From(AMailExpression).
    OrderBy(AMailExpression.Sent).Descending.
    OrderBy(AMailExpression.Priority, AMailExpression.From).
    Select;

  ShowQueryResults<TMail>(AQuery, 'Query criteria: (All records, ORDER BY SENT DESC and PRIORITY ASC and FROM ASC)');
end;
//>linq sorted query

//<linq query criteria
// SELECT * FROM MAIL WHERE SENT >= '2017/01/01' AND SENT < '2018/01/01' AND PRIORITY = 1

procedure TfrmEMFQueryEntitiesUsingLINQ.btnComplexConditionClick(Sender: TObject);
var
  AMailExpression: IMailExpression;
  AQuery: IdxQueryable;
begin
  AMailExpression := EMFSession.GetDataContext<IDemoContext>.Mail;

  AQuery := Linq.
    From(AMailExpression).
    Where(
      (AMailExpression.Sent >= EncodeDate(2017, 1, 1)) and
      (AMailExpression.Sent < EncodeDate(2018, 1, 1)) and
      (AMailExpression.Priority = 1)).
    Select;

  ShowQueryResults<TMail>(AQuery, 'Query criteria: (SENT >= ''2017/01/01'' and SENT < ''2018/01/01'' and PRIORITY = 1)');
end;
//>linq query criteria

//<linq query take function
// SELECT TOP 5 * FROM MAIL ORDER BY SENT DESC

procedure TfrmEMFQueryEntitiesUsingLINQ.btnTakeClick(Sender: TObject);
var
  AMailExpression: IMailExpression;
  AQuery: IdxQueryable;
begin
  AMailExpression := EMFSession.GetDataContext<IDemoContext>.Mail;

  AQuery := Linq.
    From(AMailExpression).
    OrderBy(AMailExpression.Sent).Descending.
    Select.
    Take(5);

  ShowQueryResults<TMail>(AQuery, 'Query criteria: (TOP 5 records, ORDER BY SENT DESC)');
end;
//>linq query take function

//<linq query take and skip functions
// SELECT TOP 3 * FROM MAIL ORDER BY SENT DESC OFFSET 2 ROWS

procedure TfrmEMFQueryEntitiesUsingLINQ.btnTakeAndSkipClick(Sender: TObject);
var
  AMailExpression: IMailExpression;
  AQuery: IdxQueryable;
begin
  AMailExpression := EMFSession.GetDataContext<IDemoContext>.Mail;

  AQuery := Linq.
    From(AMailExpression).
    OrderBy(AMailExpression.Sent).Descending.
    Select.
    Take(3).
    Skip(2);

  ShowQueryResults<TMail>(AQuery, 'Query criteria: (TOP 3 records, SKIP 2 records, ORDER BY SENT DESC)');
end;
//>linq query take and skip functions

initialization
  TfrmEMFQueryEntitiesUsingLINQ.Register;

end.
