unit ViewTableSimpleDemoData;

interface

uses
  {$IFDEF CLR}
  System.ComponentModel,
  {$ENDIF}
  SysUtils, Classes, DB, cxStyles, cxClasses, cxGridTableView,
  Forms, DBClient, MidasLib;

type
  TViewTableSimpleDemoMainDM = class(TDataModule)
    dsGENRES: TDataSource;
    dsFilms: TDataSource;
    cdsGenres: TClientDataSet;
    cdsGenresID: TAutoIncField;
    cdsGenresNAME: TStringField;
    cdsFilms: TClientDataSet;
    cdsFilmsGenres: TClientDataSet;
    cdsFilmsGenresID: TAutoIncField;
    cdsFilmsGenresFILMID: TIntegerField;
    cdsFilmsGenresGENREID: TIntegerField;
    cdsFilmsGenresPHOTO: TBlobField;
    cdsFilmsGenresICON: TBlobField;
    procedure cdsFilmsBeforePost(DataSet: TDataSet);
    procedure cdsFilmsAfterPost(DataSet: TDataSet);
    procedure cdsGenresAfterScroll(DataSet: TDataSet);
    procedure DataModuleCreate(Sender: TObject);
  private
    procedure SetFilter;
  end;

var
  ViewTableSimpleDemoMainDM: TViewTableSimpleDemoMainDM;

implementation

{$R *.dfm}

{$IFDEF CLR}
uses
  Variants;
{$ENDIF}

procedure TViewTableSimpleDemoMainDM.cdsFilmsBeforePost(DataSet: TDataSet);
begin
  if cdsFilms.State = dsInsert then
	  cdsFilmsGenres.Insert;
end;

procedure TViewTableSimpleDemoMainDM.cdsFilmsAfterPost(DataSet: TDataSet);
begin
  if cdsFilmsGenres.State = dsInsert then
  begin
	  cdsFilms.Filtered := False;
    cdsFilms.Last;
	  cdsFilmsGenres.FieldByName('GENREID').Value := cdsGenres.FieldByName('ID').Value;
	  cdsFilmsGenres.FieldByName('FILMID').Value := cdsFilms.FieldByName('ID').Value;
	  cdsFilmsGenres.FieldByName('PHOTO').Value := cdsFilms.FieldByName('PHOTO').Value;
	  cdsFilmsGenres.FieldByName('ICON').Value := cdsFilms.FieldByName('ICON').Value;
	  cdsFilmsGenres.Post;
	  cdsFilms.Filtered := True;
	  SetFilter;
  end;
end;

procedure TViewTableSimpleDemoMainDM.cdsGenresAfterScroll(
  DataSet: TDataSet);
begin
  if cdsFilms.State = dsInsert then
    cdsFilms.Cancel;
  SetFilter;
end;

procedure TViewTableSimpleDemoMainDM.SetFilter;
var
  AStr: string;
begin
  if cdsFilmsGenres.Active then
  begin
	  AStr := '';
	  cdsFilmsGenres.First;
	  while not cdsFilmsGenres.Eof do
	  begin
	    AStr := AStr + 'ID = ' + IntToStr(cdsFilmsGenres.FieldByName('FILMID').AsInteger) + ' or ';
	    cdsFilmsGenres.Next;
	  end;
	  cdsFilms.Filter := AStr + 'ID = 0';
  end;
end;

procedure TViewTableSimpleDemoMainDM.DataModuleCreate(Sender: TObject);
begin
  cdsGenres.LoadFromFile('..\..\Data\Genres.xml');
  cdsFilms.LoadFromFile('..\..\Data\Films.xml');
  cdsFilmsGenres.LoadFromFile('..\..\Data\Filmsgenres.xml');
  cdsFilms.Open;
  cdsFilmsGenres.Open;
  cdsGenres.Open;
end;

end.
