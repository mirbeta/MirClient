unit EMFQueryEntities;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, StdCtrls, Menus, DB, DBClient, cxClasses, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit,
  dxLayoutLookAndFeels, dxUIAdorners, cxMemo, cxLabel, cxButtons, cxTextEdit, cxRichEdit, dxLayoutContainer,
  dxLayoutControl, EMFCustomTutorial;

type
  { TfrmEMFQueryEntities }

  TfrmEMFQueryEntities = class(TfrmEMFCustomTutorial)
    btnGetCollection: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    btnGetByCriteria: TcxButton;
    dxLayoutItem3: TdxLayoutItem;
    btnGetByPredicate: TcxButton;
    dxLayoutItem5: TdxLayoutItem;
    btnGetByLINQ: TcxButton;
    dxLayoutItem6: TdxLayoutItem;
    procedure btnGetCollectionClick(Sender: TObject);
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
  uMain, dxCore, dxEMF.Types, dxEMF.DB.Criteria, dxEMF.Linq, EMFDemoClasses;

{ TfrmEMFQueryEntities }

class function TfrmEMFQueryEntities.GetCaption: string;
begin
  Result := 'Querying an Entity Object Collection';
end;

function TfrmEMFQueryEntities.GetDescription: string;
begin
  Result := 'This tutorial shows how to query a collection of entity objects using ' +
    'either the session component''s overloaded GetObjects function or a LINQ expression.';
end;

class function TfrmEMFQueryEntities.GetGroupID: Integer;
begin
  Result := EMFTreeViewTutorialEntityQueryingGroupID;
end;

class function TfrmEMFQueryEntities.GetID: Integer;
begin
  Result := 14;
end;

procedure TfrmEMFQueryEntities.GenerateContent;
begin
  inherited GenerateContent;
  GenerateFilms;
end;

procedure TfrmEMFQueryEntities.RegisterActions;
begin
  inherited RegisterActions;
  RegisterTutorialAction(btnGetCollection, 'This code retrieves all films (all records from the Film table) as TFilm entity objects using the session component''s GetObjects function. No criteria passed as a parameter indicates that this function retrieves all objects.', ['query all records', 'TFilm|TFilm']);
  RegisterTutorialAction(btnGetByCriteria, 'This code retrieves a collection of TFilm entity objects using the session component''s GetObjects function and a criteria operator passed as a parameter.', ['query objects using criteria operator', 'TFilm|TFilm']);
  RegisterTutorialAction(btnGetByPredicate, 'This code retrieves a collection of TFilm entity objects using the session component''s GetObjects function and a predicate (anonymous Boolean function) passed as a parameter.', ['query objects using predicate', 'TFilm|TFilm']);
  RegisterTutorialAction(btnGetByLINQ, 'This code retrieves a collection of TFilm entity objects using a LINQ expression.', ['query objects using linq expression', 'IDemoContext|IDemoContext', 'IFilmExpression|IFilmExpression']);
end;

procedure TfrmEMFQueryEntities.UnregisterActions;
begin
  UnregisterTutorialAction(btnGetByLINQ);
  UnregisterTutorialAction(btnGetByPredicate);
  UnregisterTutorialAction(btnGetByCriteria);
  UnregisterTutorialAction(btnGetCollection);
  inherited UnregisterActions;
end;

//<query all records
// SELECT * FROM FILM

procedure TfrmEMFQueryEntities.btnGetCollectionClick(Sender: TObject);
var
  ACollection: IdxEMFCollection<TFilm>;
begin
  ACollection := EMFSession.GetObjects<TFilm>;

  ShowQueryResults<TFilm>(ACollection, 'Query criteria: (All records)');
end;
//>query all records

//<query objects using criteria operator
// SELECT * FROM FILM WHERE YEAR <= 1950 AND RUNTIME > 6000

procedure TfrmEMFQueryEntities.btnGetByCriteriaClick(Sender: TObject);
var
  ACollection: IdxEMFCollection<TFilm>;
  ACriteriaOperator: IdxCriteriaOperator;
begin
  ACriteriaOperator := TdxGroupOperator.&And(
    TdxBinaryOperator.Create('YEAR', 1950, TdxBinaryOperatorType.LessOrEqual),
    TdxBinaryOperator.Create('RUNTIME', 6000, TdxBinaryOperatorType.Greater));
  ACollection := EMFSession.GetObjects<TFilm>(ACriteriaOperator);

  ShowQueryResults<TFilm>(ACollection, 'Query criteria: (YEAR <= 1950 and RUNTIME > 6000)');
end;
//>query objects using criteria operator

//<query objects using predicate
// SELECT * FROM FILM WHERE YEAR BETWEEN 1960 AND 1970

procedure TfrmEMFQueryEntities.btnGetByPredicateClick(Sender: TObject);
var
  ACollection: IdxEMFCollection<TFilm>;
begin
  ACollection := EMFSession.GetObjects<TFilm>(
    function (AFilm: TFilm): Boolean
    begin
      Result := (AFilm.Year >= 1960) and (AFilm.Year <= 1970);
    end);

  ShowQueryResults<TFilm>(ACollection, 'Query criteria: (YEAR >= 1960 and YEAR <= 1970)');
end;
//>query objects using predicate

//<query objects using linq expression
// SELECT * FROM FILM WHERE YEAR >= 1950 AND YEAR <= 1960

procedure TfrmEMFQueryEntities.btnGetByLINQClick(Sender: TObject);
var
  AFilmExpression: IFilmExpression;
  AQuery: IdxQueryable;
begin
  AFilmExpression := EMFSession.GetDataContext<IDemoContext>.Film;

  AQuery := Linq.
    From(AFilmExpression).
   	Where((AFilmExpression.Year >= 1950) and (AFilmExpression.Year <= 1960)).
  	Select;

  ShowQueryResults<TFilm>(AQuery, 'Query criteria: (YEAR >= 1950 and YEAR <= 1960)');
end;
//>query objects using linq expression

initialization
  TfrmEMFQueryEntities.Register;

end.
