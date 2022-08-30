unit EMFQueryEntitiesUsingCriteria;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, StdCtrls, Menus, DB, DBClient, cxClasses, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit,
  dxLayoutLookAndFeels, dxUIAdorners, cxMemo, cxLabel, cxButtons, cxTextEdit, cxRichEdit, dxLayoutContainer,
  dxLayoutControl, EMFCustomTutorial;

type
  { frmEMFQueryEntitiesUsingCriteria }

  TfrmEMFQueryEntitiesUsingCriteria = class(TfrmEMFCustomTutorial)
    btnSelectTop: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    btnSelectTopAndSkip: TcxButton;
    dxLayoutItem3: TdxLayoutItem;
    btnSelectOrderBy: TcxButton;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    btnCriteriaParser: TcxButton;
    dxLayoutItem6: TdxLayoutItem;
    btnBetweenOperator: TcxButton;
    dxLayoutItem7: TdxLayoutItem;
    btnInOperator: TcxButton;
    procedure btnSelectTopClick(Sender: TObject);
    procedure btnSelectTopAndSkipClick(Sender: TObject);
    procedure btnSelectOrderByClick(Sender: TObject);
    procedure btnCriteriaParserClick(Sender: TObject);
    procedure btnBetweenOperatorClick(Sender: TObject);
    procedure btnInOperatorClick(Sender: TObject);
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
  uMain, dxCore, dxEMF.Core, dxEMF.Types, dxEMF.DB.Criteria, EMFDemoClasses;

{ frmEMFQueryEntitiesUsingCriteria }

class function TfrmEMFQueryEntitiesUsingCriteria.GetCaption: string;
begin
  Result := 'Querying Data Using Criteria Operators';
end;

function TfrmEMFQueryEntitiesUsingCriteria.GetDescription: string;
begin
  Result := 'This tutorial shows how to query entity objects using criteria operators and sort query results. ' +
    'Criteria operators and query clauses are passed as parameters to the session component''s overloaded GetObjects function.';
end;

class function TfrmEMFQueryEntitiesUsingCriteria.GetGroupID: Integer;
begin
  Result := EMFTreeViewTutorialEntityQueryingGroupID;
end;

class function TfrmEMFQueryEntitiesUsingCriteria.GetID: Integer;
begin
  Result := 16;
end;

procedure TfrmEMFQueryEntitiesUsingCriteria.GenerateContent;
begin
  inherited GenerateContent;
  GenerateFilms;
end;

procedure TfrmEMFQueryEntitiesUsingCriteria.RegisterActions;
begin
  inherited RegisterActions;
  RegisterTutorialAction(btnCriteriaParser, 'This code retrieves a collection of films (TFilm entity objects) that match the criteria parsed from a string.', ['parse criteria string', 'TFilm|TFilm']);
  RegisterTutorialAction(btnSelectOrderBy, 'This code retrieves all TFilm entity objects and sorts them in ascending order by YEAR and in descending order by VIEWS.', ['sort query results', 'TFilm|TFilm']);
  RegisterTutorialAction(btnSelectTop, 'This code retrieves the first five TFilm entity objects using the TOP query clause. Note that this clause requires sorting entity objects.', ['top clause query', 'TFilm|TFilm']);
  RegisterTutorialAction(btnSelectTopAndSkip, 'This code uses the TOP and SKIP query clauses to retrieve the next three TFilm entity objects skipping the first two objects. Note that these clauses require sorting entity objects.', ['top and skip clauses query', 'TFilm|TFilm']);
  RegisterTutorialAction(btnBetweenOperator, 'This code retrieves TFilm entity objects whose RUNTIME ranges from 3600 to 5400.', ['between operator query', 'TFilm|TFilm']);
  RegisterTutorialAction(btnInOperator, 'This code retrieves TFilm entity objects whose YEAR is one of the three specified.', ['in operator query', 'TFilm|TFilm']);
end;

procedure TfrmEMFQueryEntitiesUsingCriteria.UnregisterActions;
begin
  UnregisterTutorialAction(btnSelectOrderBy);
  UnregisterTutorialAction(btnSelectTopAndSkip);
  UnregisterTutorialAction(btnSelectTop);
  UnregisterTutorialAction(btnInOperator);
  UnregisterTutorialAction(btnBetweenOperator);
  UnregisterTutorialAction(btnCriteriaParser);
  inherited UnregisterActions;
end;

//<top clause query
// SELECT TOP 5 * FROM FILM ORDER BY YEAR

procedure TfrmEMFQueryEntitiesUsingCriteria.btnSelectTopClick(Sender: TObject);
var
  ACollection: IdxEMFCollection<TFilm>;
  ASortByExpressions: TdxSortByExpressions;
  ASortByExpression1: TdxSortByExpression;
begin
  ASortByExpressions := TdxSortByExpressions.Create;
  ASortByExpression1 := TdxSortByExpression.Create(TdxOperandProperty.Create('YEAR'));
  ASortByExpressions.Add(ASortByExpression1);
  ACollection := EMFSession.GetObjects<TFilm>(nil, ASortByExpressions, 0, 5);

  ShowQueryResults<TFilm>(ACollection, 'Query criteria: (TOP 5 records, ORDER BY YEAR)');
end;
//>top clause query

//<top and skip clauses query
// SELECT TOP 3 * FROM FILM ORDER BY YEAR OFFSET 2 ROWS

procedure TfrmEMFQueryEntitiesUsingCriteria.btnSelectTopAndSkipClick(Sender: TObject);
var
  ACollection: IdxEMFCollection<TFilm>;
  ASortByExpressions: TdxSortByExpressions;
  ASortByExpression1: TdxSortByExpression;
begin
  ASortByExpressions := TdxSortByExpressions.Create;
  ASortByExpression1 := TdxSortByExpression.Create(TdxOperandProperty.Create('YEAR'));
  ASortByExpressions.Add(ASortByExpression1);
  ACollection := EMFSession.GetObjects<TFilm>(nil, ASortByExpressions, 2, 3);

  ShowQueryResults<TFilm>(ACollection, 'Query criteria: (TOP 3 records, SKIP 2 records, ORDER BY YEAR)');
end;
//>top and skip clauses query

//<sort query results
// SELECT * FROM FILM ORDER BY YEAR ASC, VIEWS DESC

procedure TfrmEMFQueryEntitiesUsingCriteria.btnSelectOrderByClick(Sender: TObject);
var
  ACollection: IdxEMFCollection<TFilm>;
  ASortByExpressions: TdxSortByExpressions;
  ASortByExpression1, ASortByExpression2: TdxSortByExpression;
begin
  ASortByExpressions := TdxSortByExpressions.Create;
  ASortByExpression1 := TdxSortByExpression.Create(TdxOperandProperty.Create('YEAR'), TdxSortDirection.Ascending);
  ASortByExpressions.Add(ASortByExpression1);
  ASortByExpression2 := TdxSortByExpression.Create(TdxOperandProperty.Create('VIEWS'), TdxSortDirection.Descending);
  ASortByExpressions.Add(ASortByExpression2);
  ACollection := EMFSession.GetObjects<TFilm>(nil, ASortByExpressions);

  ShowQueryResults<TFilm>(ACollection, 'Query criteria: (All records, ORDER BY YEAR ASC and VIEWS DESC)');
end;
//>sort query results

//<parse criteria string
procedure TfrmEMFQueryEntitiesUsingCriteria.btnCriteriaParserClick(Sender: TObject);
var
  ACollection: IdxEMFCollection<TFilm>;
begin
  ACollection := EMFSession.GetObjects<TFilm>(TdxCriteriaOperator.Parse('Contains(TAGLINE, ' + QuotedStr('horror') + ') and YEAR < 1960'));

  ShowQueryResults<TFilm>(ACollection, 'Query criteria: (TAGLINE contains ''horror'' and YEAR < 1960)');
end;
//>parse criteria string

//<between operator query
// SELECT * FROM FILM WHERE RUNTIME BETWEEN 3600 AND 5400

procedure TfrmEMFQueryEntitiesUsingCriteria.btnBetweenOperatorClick(Sender: TObject);
var
  ACollection: IdxEMFCollection<TFilm>;
  ACriteriaOperator: TdxCriteriaOperator;
begin
  ACriteriaOperator := TdxBetweenOperator.Create('RUNTIME', 3600, 5400);
  ACollection := EMFSession.GetObjects<TFilm>(ACriteriaOperator);

  ShowQueryResults<TFilm>(ACollection, 'Query criteria: (RUNTIME >= 3600 and RUNTIME <= 5400)');
end;
//>between operator query

//<in operator query
// SELECT * FROM FILM WHERE YEAR IN [1936, 1939, 1979]

procedure TfrmEMFQueryEntitiesUsingCriteria.btnInOperatorClick(Sender: TObject);
var
  ACollection: IdxEMFCollection<TFilm>;
  ACriteriaOperator: TdxCriteriaOperator;
begin
  ACriteriaOperator := TdxInOperator.Create('YEAR', [1936, 1939, 1979]);
  ACollection := EMFSession.GetObjects<TFilm>(ACriteriaOperator);

  ShowQueryResults<TFilm>(ACollection, 'Query criteria: (YEAR IN [1936, 1939, 1979])');
end;
//>in operator query

initialization
  TfrmEMFQueryEntitiesUsingCriteria.Register;

end.
