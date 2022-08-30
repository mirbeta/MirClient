unit EMFCustomTutorial;

interface

{$I cxVer.inc}

uses
  SysUtils, Graphics, Classes, Controls, Forms, DB, DBClient, StdCtrls, ExtCtrls, Generics.Defaults,
  Generics.Collections, Menus, dxCore, cxClasses, cxGraphics, cxControls, dxGDIPlusClasses, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxGroupBox, dxLayoutContainer, dxLayoutControl, cxImageList,
  dxLayoutLookAndFeels, dxLayoutControlAdapters, cxButtons, dxLayoutcxEditAdapters, cxCheckBox, cxTextEdit, cxMemo,
  cxRichEdit, dxCalloutPopup, cxLabel, uBaseDemoForm, dxDemoTutorial, EMFDemoClasses, dxEMF.Core,
  dxEMF.DataProvider.FireDAC, dxEMF.Types, dxEMF.DataSet, dxEMF.DB.Sqlite, dxUIAdorners,  dxToggleSwitch,
{$IFDEF DELPHI19}
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Dapt, FireDAC.Phys.SQLiteWrapper, FireDAC.Comp.Client,
  FireDAC.VCLUI.Wait, FireDAC.Comp.UI, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLite;
{$ELSE}
  uADStanIntf, uADStanOption, uADStanError, uADGUIxIntf, uADPhysIntf, uADStanDef, uADStanPool, uADStanAsync,
  uADPhysManager, uADGUIxFormsWait, uADStanParam, uADDatSManager, uADDAptIntf, uADDAptManager, uADCompDataSet,
  uADCompClient, uADPhysSQLiteWrapper, uADPhysODBCBase, uADPhysMSSQL, uADPhysSQLite, uADCompGUIx,
  dxEMF.Utils.FireDACAliases;
{$ENDIF}

const
  EntityMemoDelimiter = '********************';
  EntityObjectNotFound = 'The specified entity object cannot be found';

type
{$IFNDEF DELPHI19}
  TSQLiteFunctionInstance = TSQLiteFunctionData;
{$ENDIF}

  { TfrmEMFCustomTutorial }

  TfrmEMFCustomTutorial = class(TBaseDemoForm)
    pnlData: TPanel;
    liDataGrid: TdxLayoutItem;
    lgActions: TdxLayoutGroup;
    lgConnect: TdxLayoutGroup;
    cdsFilms: TClientDataSet;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    lcTutorialInfo: TdxLayoutControl;
    reStepSources: TcxRichEdit;
    btnCopy: TcxButton;
    cxLabel1: TcxLabel;
    lcTutorialInfoGroup_Root: TdxLayoutGroup;
    liActionSources: TdxLayoutItem;
    lgSources: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    liDescriptionLabel: TdxLayoutItem;
    lilActionDescription: TdxLayoutLabeledItem;
    dxUIAdornerManager1: TdxUIAdornerManager;
    cxLookAndFeelController1: TcxLookAndFeelController;
    mResults: TcxMemo;
    dxLayoutLookAndFeelList2: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel2: TdxLayoutCxLookAndFeel;
    cdsFilmsID: TAutoIncField;
    cdsFilmsCAPTION: TStringField;
    cdsFilmsYEAR: TIntegerField;
    cdsFilmsTAGLINE: TStringField;
    cdsFilmsPLOTOUTLINE: TStringField;
    cdsFilmsRUNTIME: TIntegerField;
    cdsFilmsCOLOR: TStringField;
    cdsFilmsPHOTO: TBlobField;
    cdsFilmsICON: TBlobField;
    cdsFilmsWEBSITE: TStringField;
    procedure dxUIAdornerManager1GuideGetCalloutPopupControl(AManager: TdxUIAdornerManager; AGuide: TdxGuide;
      out AControl: TWinControl);
    procedure dxUIAdornerManager1AdornerCustomDraw(AManager: TdxUIAdornerManager; AAdorner: TdxCustomAdorner;
      ACanvas: TdxGPCanvas; AViewInfo: TdxCustomAdornerViewInfo; var ADone: Boolean);
    procedure btnCopyClick(Sender: TObject);
    procedure dxUIAdornerManager1ActiveChanged(AManager: TdxUIAdornerManager; AAdorners: TdxCustomAdorners);
    procedure OnKeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  strict private
    FTutorialMode: Boolean;

    procedure SetConnected(const AValue: Boolean);
    procedure SetTutorialMode(const AValue: Boolean);

    function FindTutorialInfo(AGuide: TdxGuide; out ATutorialInfo: TTutorialInfo): Boolean; overload;
    function FindTutorialInfo(AControl: TWinControl; out ATutorialInfo: TTutorialInfo): Boolean; overload;
    procedure Connect;
    procedure DeleteEntities<T: class>;
    procedure DeleteLayoutItem(AControl: TControl);
    procedure SQLiteFunctionCharIndexCalculate(AFunc: TSQLiteFunctionInstance; AInputs: TSQLiteInputs; AOutput: TSQLiteOutput;
      var AUserData: TObject);
    procedure Output<T: class>(AEntity: T; AMemo: TcxMemo; AClear: Boolean = True);
    procedure OutputEntityDelimiter(AMemo: TcxMemo);
    procedure PrepareOutputLines(AClear: Boolean);
    procedure UpdateActionsButtons;
  protected const
    FilmFoundInDatabaseText = 'The following TFilm entity object is found in the database:';
    FilmMarkedAsDeletedText = 'The following TFilm entity object is marked as deleted:';
  protected
    FConnected: Boolean;
  {$IFDEF DELPHI19}
    FConnection: TFDConnection;
    FDSQLiteFunctionCharIndex2: TFDSQLiteFunction;
    FDSQLiteFunctionCharIndex3: TFDSQLiteFunction;
    FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink;
  {$ELSE}
    FConnection: TADConnection;
    FDSQLiteFunctionCharIndex2: TADSQLiteFunction;
    FDSQLiteFunctionCharIndex3: TADSQLiteFunction;
    FDPhysSQLiteDriverLink: TADPhysSQLiteDriverLink;
  {$ENDIF}
    FEMFDataProvider: TdxEMFFireDACDataProvider;
    FEMFSession: TdxEMFSession;

    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure Finalize; override;

    function GetActionButtonWidth: Integer; virtual;
    procedure Clear; overload; virtual;
    procedure GenerateContent; virtual;
    procedure RegisterActions; virtual;
    procedure UnregisterActions; virtual;
    procedure UpdateControls; virtual;

    function GetDefaultEMail(const AUserName: string): string;
    function GetRandomBoolean: Boolean;
    function GetRandomDateTime: TDateTime;
    function GetRandomInteger: Integer;
    function GetRandomIntegerAsString: string;
    function GetRandomProducts: string;
    function GetRandomRuntime: Integer;
    function GetRandomSubject: string;
    function GetRandomTitle: string;
    function GetRandomUser: string;
    function GetRandomViews: Integer;
    function GetRandomYear: Integer;
    procedure CreateDatabaseConnection;
    procedure CreateEMFDataProvider;
    procedure CreateEMFSession;
    procedure GenerateFilms;
    procedure GenerateMails;
    procedure GenerateTeams;
    procedure ShowLocateEntityText<T: class>(AID: Integer); overload;
    procedure ShowLocateEntityText(AEntityClass: TClass; AID: Integer; AClear: Boolean = False); overload;
    procedure ShowTeam(ATeam: TTeam);
    procedure ShowTeamMember(AMember: TMember; AUseDelimiter: Boolean);
    procedure ShowTeamMembers(const AMembers: IdxEMFCollection<TMember>; AUseMemberDelimiter: Boolean);
    procedure ShowTeams(const ATeams: IdxEMFCollection<TTeam>);
    procedure ShowText(const AText: string; AClear: Boolean = False);
    procedure ShowResults(const AText: string); overload;
    procedure ShowResults<T: class>(AEntity: T; AClear: Boolean = True); overload;
    procedure ShowResults<T: class>(AEntity: T; AMemo: TcxMemo; AClear: Boolean = True); overload;
    procedure ShowResults<T: class>(const ACollection: IEnumerable); overload;
    procedure ShowResults<T: class>(const ACollection: IEnumerable; AClear: Boolean); overload;
    procedure ShowQueryResults<T: class>(AEntity: T; const AQueryText: string = ''); overload;
    procedure ShowQueryResults<T: class>(ACollection: IEnumerable; const AQueryText: string = ''); overload;
    procedure ShowQueryResults<T: class>(ACollection: IEnumerable; const AText: string; AClear: Boolean); overload;

    procedure RegisterSQLiteFunctions;
    procedure RegisterTutorialAction(AControl: TWinControl; const ADescription: string;
      const ASourceTags: array of string); overload;
    procedure RegisterTutorialAction(AControl: TWinControl; const ADescription: string;
      const ASourceTags, AUnits: array of string); overload;
    procedure UnregisterTutorialAction(AControl: TWinControl);
  public
    class function GetGroupID: Integer; override;
    class function GetID: Integer; override;

    property Connected: Boolean read FConnected write SetConnected;
    property EMFDataProvider: TdxEMFFireDACDataProvider read FEMFDataProvider;
    property EMFSession: TdxEMFSession read FEMFSession;
    property DatabaseConnection: TFDConnection read FConnection;
    property TutorialMode: Boolean read FTutorialMode write SetTutorialMode;
  end;

implementation

{$R *.dfm}

uses
  Types, Windows, StrUtils, DateUtils, Math, dxCoreGraphics, dxEMF.MetaData, dxEMF.Linq, uMain;

const
  ActionButtonWidth = 200;
  Users: array[0..16] of string = (
    'Aaron Lewis Borrmann',
    'Alma D Dusek',
    'Alphonzo E Hively',
    'Bruce F Cambell',
    'Dailiah Austin Campbell',
    'Beverly Oneil',
    'Jimmy Lewis',
    'Jeffrey W McClain',
    'Calvin Liu',
    'Dave Murrel',
    'Chandler Bevington',
    'Edward Keck',
    'Clementine Luis Aguilar',
    'Paul Bailey',
    'Brad Barnes',
    'Carl Lucas',
    'Jerry Camnpbell'
  );
  Products: array[0..12] of string = (
    'ExpressQuantumGrid',
    'ExpressBars',
    'ExpressScheduler',
    'ExpressPivotGrid',
    'ExpressQuantumTreeList',
    'ExpressVerticalGrid',
    'ExpressNavBar',
    'ExpressSpellChecker',
    'ExpressSkins Library',
    'ExpressSpreadSheet',
    'ExpressDBTree',
    'ExpressOrgChart',
    'ExpressFlowChart');
  Subjects: array[0..20] of string = (
    'Integrating Developer Express MasterView control into an Accounting System.',
    'Web Edition: Data Entry Page. There is an issue with date validation.',
    'Payables Due Calculator is ready for testing.',
    'Web Edition: Search Page is ready for testing.',
    'Main Menu: Duplicate Items. Somebody has to review all menu items in the system.',
    'Receivables Calculator. Where can I find the complete specs?',
    'Ledger: Inconsistency. Please fix it.',
    'Receivables Printing module is ready for testing.',
    'Screen Redraw. Somebody has to look at it.',
    'Email System. What library are we going to use?',
    'Cannot add new vendor. This module doesn''''t work!',
    'History. Will we track sales history in our system?',
    'Main Menu: Add a File menu. File menu item is missing.',
    'Currency Mask. The current currency mask in completely unusable.',
    'Drag & Drop operations are not available in the scheduler module.',
    'Data Import. What database types will we support?',
    'Reports. The list of incomplete reports.',
    'Data Archiving. We still don''''t have this features in our application.',
    'Email Attachments. Is it possible to add multiple attachments? I haven''''t found a way to do this.',
    'Check Register. We are using different paths for different modules.',
    'Data Export. Our customers asked us for export to Microsoft Excel');
  Teams: array[0..2] of string = (
    'Management',
    'Support',
    'Documentation');

type
  TdxGuideAccess = class(TdxGuide);
  TdxGuidesAccess = class(TdxGuides);

{ TfrmEmptyUnit }

class function TfrmEMFCustomTutorial.GetGroupID: Integer;
begin
  Result := EMFTreeViewRootID;
end;

class function TfrmEMFCustomTutorial.GetID: Integer;
begin
  Result := 0;
end;

procedure TfrmEMFCustomTutorial.DestroySubClasses;
begin
  FreeAndNil(FConnection);
  FreeAndNil(FEMFDataProvider);
  FreeAndNil(FEMFSession);
  inherited DestroySubClasses;
end;

procedure TfrmEMFCustomTutorial.Initialize;
begin
  inherited Initialize;
  Connect;
  GenerateContent;
  Connected := False;
  Clear;
  RegisterActions;
  UpdateControls;
  UpdateActionsButtons;
end;

procedure TfrmEMFCustomTutorial.Finalize;
begin
  UnregisterActions;
  inherited Finalize;
end;

function TfrmEMFCustomTutorial.GetActionButtonWidth: Integer;
begin
  Result := ActionButtonWidth;
end;

procedure TfrmEMFCustomTutorial.Clear;
begin
  mResults.Clear;
end;

procedure TfrmEMFCustomTutorial.GenerateContent;
begin
// do nothing
end;

procedure TfrmEMFCustomTutorial.RegisterActions;
begin
// do nothing
end;

procedure TfrmEMFCustomTutorial.UnregisterActions;
begin
// do nothing
end;

procedure TfrmEMFCustomTutorial.UpdateControls;
begin
  liActionSources.Visible := False;
end;

function TfrmEMFCustomTutorial.GetDefaultEMail(const AUserName: string): string;
begin
  Result := StringReplace(AUserName, ' ', '.', [rfReplaceAll]) + '@dxcorpmail.com';
end;

function TfrmEMFCustomTutorial.GetRandomBoolean: Boolean;
begin
  Result := Boolean(Random(2));
end;

function TfrmEMFCustomTutorial.GetRandomDateTime: TDateTime;
begin
  Result := IncSecond(Now, - Random(315360000));
end;

function TfrmEMFCustomTutorial.GetRandomInteger: Integer;
begin
  Randomize;
  Result := Random(10000);
end;

function TfrmEMFCustomTutorial.GetRandomIntegerAsString: string;
begin
  Result := IntToStr(GetRandomInteger);
end;

function TfrmEMFCustomTutorial.GetRandomProducts: string;
begin
  Result := Products[Random(13)];
end;

function TfrmEMFCustomTutorial.GetRandomRuntime: Integer;
begin
  Result := RandomRange(90, 150);
end;

function TfrmEMFCustomTutorial.GetRandomSubject: string;
begin
  Result := Subjects[Random(21)];
end;

function TfrmEMFCustomTutorial.GetRandomTitle: string;
begin
  Result := 'Title' + IntToStr(GetRandomInteger);
end;

function TfrmEMFCustomTutorial.GetRandomUser: string;
begin
  Result := Users[Random(17)];
end;

function TfrmEMFCustomTutorial.GetRandomViews: Integer;
begin
  Result := RandomRange(2000, 5000);
end;

function TfrmEMFCustomTutorial.GetRandomYear: Integer;
begin
  Result := RandomRange(1940, 2017);
end;

//<create database connection
procedure TfrmEMFCustomTutorial.CreateDatabaseConnection;
begin
  FreeAndNil(FConnection); //<tutorial>
{$IFDEF DELPHI19}
  FConnection := TFDConnection.Create(Self);
{$ELSE}
  FConnection := TADConnection.Create(Self);
{$ENDIF}
  FConnection.Params.Add('Database=Data\demos.db');
  FConnection.Params.Add('DriverID=SQLite');
  FConnection.LoginPrompt := False;
end;
//>create database connection

//<create EMF data provider
procedure TfrmEMFCustomTutorial.CreateEMFDataProvider;
begin
  FreeAndNil(FEMFDataProvider); //<tutorial>
  FEMFDataProvider := TdxEMFFireDACDataProvider.Create(Self);
  FEMFDataProvider.Connection := FConnection;
  FEMFDataProvider.Options.AutoCreate := TdxAutoCreateOption.DatabaseAndSchema;
end;
//>create EMF data provider

//<create EMF session
procedure TfrmEMFCustomTutorial.CreateEMFSession;
begin
  FreeAndNil(FEMFSession); //<tutorial>
  FEMFSession := TdxEMFSession.Create(Self);
  FEMFSession.DataProvider := FEMFDataProvider;
  FEMFSession.CreateSchema([TFilm, TMail, TTeam, TMember]);
end;
//>create EMF session

procedure TfrmEMFCustomTutorial.GenerateFilms;
var
  AFilm: TFilm;
begin
  DeleteEntities<TFilm>;
  cdsFilms.LoadFromFile('Data\Films.xml');
  cdsFilms.Open;
  while not cdsFilms.Eof do
  begin
    AFilm := TFilm.Create;
    AFilm.Title := cdsFilms.FieldByName('Caption').AsString;
    AFilm.Year := cdsFilms.FieldByName('Year').AsInteger;
    AFilm.Runtime := cdsFilms.FieldByName('Runtime').AsInteger;
    AFilm.Plotoutline := cdsFilms.FieldByName('Plotoutline').AsString;
    AFilm.Tagline := cdsFilms.FieldByName('Tagline').AsString;
    AFilm.Color := cdsFilms.FieldByName('Color').AsBoolean;
    AFilm.Website := cdsFilms.FieldByName('Website').AsString;
    AFilm.Views := GetRandomViews;
    EMFSession.Save(AFilm);
    cdsFilms.Next;
  end;
  cdsFilms.Close;
  EMFSession.FlushChanges;
end;

procedure TfrmEMFCustomTutorial.GenerateMails;
var
  I: Integer;
  AMail: TMail;
begin
  EMFSession.CreateSchema(TMail);
  DeleteEntities<TMail>;
  for I := 0 to 99 do
  begin
    AMail := TMail.Create;
    AMail.ID := I + 1;
    AMail.Subject := GetRandomSubject;
    AMail.From := GetRandomUser;
    AMail.Sent := GetRandomDateTime;
    AMail.Size := GetRandomInteger;
    AMail.HasAttachment := GetRandomBoolean;
    AMail.Priority := Random(3) + 1;
    EMFSession.Save(AMail);
  end;
  EMFSession.FlushChanges;
end;

procedure TfrmEMFCustomTutorial.GenerateTeams;
var
  I, J, K, AUserIndex: Integer;
  AMember: TMember;
  AProduct: string;
  ATeam: TTeam;
begin
  EMFSession.CreateSchema([TTeam, TMember]);
  DeleteEntities<TTeam>;
  DeleteEntities<TMember>;
  AUserIndex := 0;
  for I := Low(Teams) to High(Teams) do
  begin
    ATeam := TTeam.Create;
    ATeam.Name := Teams[I];
    for K := 0 to 2 do
    begin
      AProduct := GetRandomProducts;
      while ATeam.Products.IndexOf(AProduct) <> -1 do
        AProduct := GetRandomProducts;
      ATeam.Products.Add(AProduct);
    end;
    for J := 0 to 2 do
    begin
      AMember := TMember.Create;
      AMember.Name := Users[AUserIndex + 1];
      AMember.EMail := GetDefaultEMail(AMember.Name);
      ATeam.Members.Add(AMember);
      Inc(AUserIndex);
    end;
    EMFSession.Save(ATeam);
  end;
  EMFSession.FlushChanges;
end;

procedure TfrmEMFCustomTutorial.OutputEntityDelimiter(AMemo: TcxMemo);
begin
  if AMemo.Lines[AMemo.Lines.Count - 1] <> EntityMemoDelimiter then
    AMemo.Lines.Add(EntityMemoDelimiter);
end;

procedure TfrmEMFCustomTutorial.ShowLocateEntityText<T>(AID: Integer);
var
  AEntity: T;
begin
  ShowLocateEntityText(TClass(T), AID);
  AEntity := EMFSession.Find<T>(AID);
  if AEntity <> nil then
  begin
    ShowResults(FilmFoundInDatabaseText);
    ShowResults<T>(AEntity, False);
  end
  else
    ShowResults(EntityObjectNotFound);
end;

procedure TfrmEMFCustomTutorial.ShowLocateEntityText(AEntityClass: TClass; AID: Integer; AClear: Boolean = False);
var
  AText: string;
begin
  AText := dxCRLF + Format('Attempting to locate %s entity object (ID = %d) in the database...', [AEntityClass.ClassName, AID]);
  ShowText(AText, AClear);
end;

procedure TfrmEMFCustomTutorial.ShowTeam(ATeam: TTeam);
var
  I: Integer;
begin
  if ATeam <> nil then
  begin
    OutputEntityDelimiter(mResults);
    mResults.Lines.Add('Team ID: ' + IntToStr(ATeam.ID));
    mResults.Lines.Add('Name: ' + ATeam.Name);
    mResults.Lines.Add('Products: ');
    for I := 0 to ATeam.Products.Count - 1 do
      mResults.Lines.Add('  ' + ATeam.Products[I]);
    mResults.Lines.Add('Members: ');
    ShowTeamMembers(ATeam.Members, False);
    OutputEntityDelimiter(mResults);
  end
  else
    mResults.Lines.Add(EntityObjectNotFound);
end;

procedure TfrmEMFCustomTutorial.ShowTeamMember(AMember: TMember; AUseDelimiter: Boolean);
begin
  if AMember <> nil then
  begin
    if AUseDelimiter then
      OutputEntityDelimiter(mResults);
    mResults.Lines.Add('  Member ID: ' + IntToStr(AMember.ID));
    mResults.Lines.Add('    E-Mail:' + AMember.EMail);
    if AUseDelimiter then
      OutputEntityDelimiter(mResults);
  end
  else
    mResults.Lines.Add(EntityObjectNotFound);
end;

procedure TfrmEMFCustomTutorial.ShowTeamMembers(const AMembers: IdxEMFCollection<TMember>; AUseMemberDelimiter: Boolean);
var
  AMember: TMember;
begin
  if AUseMemberDelimiter then
    PrepareOutputLines(False);
  for AMember in AMembers do
    ShowTeamMember(AMember, AUseMemberDelimiter);
end;

procedure TfrmEMFCustomTutorial.ShowTeams(const ATeams: IdxEMFCollection<TTeam>);
var
  ATeam: TTeam;
begin
  PrepareOutputLines(False);
  for ATeam in ATeams do
    ShowTeam(ATeam);
end;

procedure TfrmEMFCustomTutorial.ShowText(const AText: string; AClear: Boolean = False);
var
  ALastLineIndex: Integer;
begin
  PrepareOutputLines(AClear);
  ALastLineIndex := mResults.Lines.Count - 1;
  if (mResults.Lines[ALastLineIndex] = '') and (Pos(dxCRLF, AText) > 0) then
    mResults.Lines.Delete(ALastLineIndex);
  mResults.Lines.Add(AText);
end;

procedure TfrmEMFCustomTutorial.ShowResults(const AText: string);
begin
  mResults.Lines.Add(AText);
end;

procedure TfrmEMFCustomTutorial.ShowResults<T>(AEntity: T; AClear: Boolean = True);
begin
  Output<T>(AEntity, mResults, AClear);
end;

procedure TfrmEMFCustomTutorial.ShowResults<T>(AEntity: T; AMemo: TcxMemo; AClear: Boolean = True);
begin
  Output<T>(AEntity, AMemo, AClear);
end;

procedure TfrmEMFCustomTutorial.ShowResults<T>(const ACollection: IEnumerable);
begin
  ShowResults<T>(ACollection, True);
end;

procedure TfrmEMFCustomTutorial.ShowResults<T>(const ACollection: IEnumerable; AClear: Boolean);
var
  AEntity: TObject;
begin
  PrepareOutputLines(AClear);
  mResults.Lines.BeginUpdate;
  for AEntity in ACollection do
    Output<T>(AEntity, mResults, False);
  mResults.Lines.EndUpdate;
end;

procedure TfrmEMFCustomTutorial.ShowQueryResults<T>(AEntity: T; const AQueryText: string = '');
var
  AClass: TClass;
  AShowQueryText: Boolean;
begin
  AClass := T;
  AShowQueryText := AQueryText <> '';
  if AShowQueryText then
    ShowText(AQueryText, AShowQueryText);
  ShowText(Format('The following %s entity object matches the specified criteria:', [AClass.ClassName]), not AShowQueryText);
  ShowResults<T>(AEntity, False);
end;

procedure TfrmEMFCustomTutorial.ShowQueryResults<T>(ACollection: IEnumerable; const AQueryText: string = '');
var
  AClass: TClass;
  AShowQueryText: Boolean;
begin
  AClass := T;
  AShowQueryText := AQueryText <> '';
  if AShowQueryText then
    ShowText(AQueryText, AShowQueryText);
  ShowQueryResults<T>(ACollection,
    Format('The following %s entity objects match the specified criteria:', [AClass.ClassName]), not AShowQueryText);
end;

procedure TfrmEMFCustomTutorial.ShowQueryResults<T>(ACollection: IEnumerable; const AText: string; AClear: Boolean);
begin
  ShowText(AText, AClear);
  ShowResults<T>(ACollection, False);
end;

procedure TfrmEMFCustomTutorial.RegisterSQLiteFunctions;
begin
  if FDSQLiteFunctionCharIndex2 <> nil then
    Exit;
{$IFDEF DELPHI19}
  FDPhysSQLiteDriverLink := TFDPhysSQLiteDriverLink.Create(Self);
  FDSQLiteFunctionCharIndex2 := TFDSQLiteFunction.Create(Self);
  FDSQLiteFunctionCharIndex3 := TFDSQLiteFunction.Create(Self);
{$ELSE}
  FDPhysSQLiteDriverLink := TADPhysSQLiteDriverLink.Create(Self);
  FDSQLiteFunctionCharIndex2 := TADSQLiteFunction.Create(Self);
  FDSQLiteFunctionCharIndex3 := TADSQLiteFunction.Create(Self);
{$ENDIF}
  FDSQLiteFunctionCharIndex2.DriverLink := FDPhysSQLiteDriverLink;
  FDSQLiteFunctionCharIndex2.FunctionName := 'CharIndex';
  FDSQLiteFunctionCharIndex2.ArgumentsCount := 2;
  FDSQLiteFunctionCharIndex2.OnCalculate := SQLiteFunctionCharIndexCalculate;
  FDSQLiteFunctionCharIndex2.Active := True;
  FDSQLiteFunctionCharIndex3.DriverLink := FDPhysSQLiteDriverLink;
  FDSQLiteFunctionCharIndex3.FunctionName := 'CharIndex';
  FDSQLiteFunctionCharIndex3.ArgumentsCount := 3;
  FDSQLiteFunctionCharIndex3.OnCalculate := SQLiteFunctionCharIndexCalculate;
  FDSQLiteFunctionCharIndex3.Active := True;
end;

procedure TfrmEMFCustomTutorial.RegisterTutorialAction(AControl: TWinControl; const ADescription: string;
  const ASourceTags: array of string);
var
  I: Integer;
  ATutorialInfo: TTutorialInfo;
  AItem: TdxLayoutItem;
  ARichEdit: TcxRichEdit;
begin
  RegisterTutorialAction(AControl, ADescription, ASourceTags, ['TutorialUnits\EMFCustomTutorial.pas',
    'TutorialUnits\EMFDemoClasses.pas', 'TutorialUnits\' + UnitName + '.pas']);
  if FindTutorialInfo(AControl, ATutorialInfo) then
  begin
    lcTutorialInfo.BeginUpdate;
    try
      for I := ATutorialInfo.Entries.Count - 1 downto 0 do
      begin
        ARichEdit := ATutorialInfo.Entries[I].Sources;
        DeleteLayoutItem(ARichEdit);
        AItem := lgSources.CreateItemForControl(ARichEdit);
        AItem.AlignHorz := ahClient;
        AItem.AlignVert := avClient;
        AItem.Caption := ATutorialInfo.Entries[I].Caption;
        AItem.CaptionOptions.Visible := False;
        AItem.Visible := False;
        ARichEdit.Properties.ScrollBars := ssBoth;
        ARichEdit.Style.Color := clWhite;
        ARichEdit.Style.HotTrack := False;
        ARichEdit.Properties.ReadOnly := True;
      end;
    finally
      lcTutorialInfo.EndUpdate;
    end;
  end;
end;

procedure TfrmEMFCustomTutorial.RegisterTutorialAction(AControl: TWinControl; const ADescription: string;
  const ASourceTags, AUnits: array of string);
var
  AGuide: TdxGuide;
begin
  AGuide := dxUIAdornerManager1.Guides.Add;
  TdxAdornerTargetElementControl(AGuide.TargetElement).Control := AControl;
  TutorialManager.RegisterTutorial(AControl, ADescription, ASourceTags, AUnits);
end;

procedure TfrmEMFCustomTutorial.UnregisterTutorialAction(AControl: TWinControl);
var
  I: Integer;
  ATutorialInfo: TTutorialInfo;
begin
  if FindTutorialInfo(AControl, ATutorialInfo) then
  begin
    lcTutorialInfo.BeginUpdate;
    try
      for I := ATutorialInfo.Entries.Count - 1 downto 0 do
        DeleteLayoutItem(ATutorialInfo.Entries[I].Sources);
    finally
      lcTutorialInfo.EndUpdate;
    end;
  end;
  TutorialManager.UnregisterTutorial(AControl);
end;

function TfrmEMFCustomTutorial.FindTutorialInfo(AGuide: TdxGuide; out ATutorialInfo: TTutorialInfo): Boolean;
var
  ATargetControl: TWinControl;
begin
  Result := False;
  if AGuide.TargetElement is TdxAdornerTargetElementControl then
  begin
    ATargetControl := TdxAdornerTargetElementControl(AGuide.TargetElement).Control;
    Result := ATargetControl <> nil;
    if Result then
      Result := FindTutorialInfo(ATargetControl, ATutorialInfo);
  end;
end;

function TfrmEMFCustomTutorial.FindTutorialInfo(AControl: TWinControl; out ATutorialInfo: TTutorialInfo): Boolean;
begin
  Result := TutorialManager.TryGetValue(AControl, ATutorialInfo);
end;

procedure TfrmEMFCustomTutorial.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  inherited;
  for I := 0 to lgConnect.Count - 1 do
    lgConnect.Items[I].AlignHorz := ahLeft;
end;

procedure TfrmEMFCustomTutorial.Connect;
begin
  CreateDatabaseConnection;
  RegisterSQLiteFunctions;
  CreateEMFDataProvider;
  CreateEMFSession;
  Connected := True;
end;

procedure TfrmEMFCustomTutorial.DeleteEntities<T>;
var
  ACollection: IEnumerable;
  AObject: TObject;
begin
  ACollection := EMFSession.GetObjects<T>;
  for AObject in ACollection do
    EMFSession.Delete(AObject);
  EMFSession.FlushChanges;
end;

procedure TfrmEMFCustomTutorial.DeleteLayoutItem(AControl: TControl);
var
  AItem: TdxLayoutItem;
begin
  AItem := lcTutorialInfo.FindItem(AControl);
  if AItem <> nil then
    AItem.Free;
  AControl.Parent := nil;
end;

procedure TfrmEMFCustomTutorial.SQLiteFunctionCharIndexCalculate(AFunc: TSQLiteFunctionInstance;
  AInputs: TSQLiteInputs; AOutput: TSQLiteOutput; var AUserData: TObject);
var
  AStartPos: integer;
begin
  case AInputs.Count of
    2:
      AStartPos := 1;
    3:
      begin
        AStartPos := AInputs[2].AsInteger;
        if AStartPos<=0 then
          AStartPos := 1;
      end
    else
      raise Exception.Create('ErrorWrongNumberOfArgs');
  end;
  if AInputs[0].IsNull or AInputs[1].IsNull then
    AOutput.AsInteger := 0
  else
    AOutput.AsInteger := PosEx(AInputs[0].AsString, AInputs[1].AsString, AStartPos);
end;

procedure TfrmEMFCustomTutorial.Output<T>(AEntity: T; AMemo: TcxMemo; AClear: Boolean = True);
var
  I, APrevCount: Integer;
  AEntityInfo: TdxEntityInfo;
  AField: string;
begin
  PrepareOutputLines(AClear);
  if AEntity <> nil then
  begin
    APrevCount := AMemo.Lines.Count;
    AEntityInfo := EntityManager.GetEntityInfo(TClass(T));
    for I := 0 to AEntityInfo.MemberAttributes.Count - 1 do
    begin
      AField := AEntityInfo.MemberAttributes[I].MemberName + ': ' + AEntityInfo.MemberAttributes[I].GetValue(AEntity).ToString;
      if AEntityInfo.MemberAttributes[I].IsKey then
        AMemo.Lines.Insert(APrevCount, AField)
      else
        AMemo.Lines.Add(AField);
    end;
    if AMemo.Lines[Max(APrevCount - 1, 0)] <> EntityMemoDelimiter then
      AMemo.Lines.Insert(APrevCount, EntityMemoDelimiter);
    OutputEntityDelimiter(AMemo);
  end
  else
    AMemo.Lines.Add(EntityObjectNotFound);
end;

procedure TfrmEMFCustomTutorial.PrepareOutputLines(AClear: Boolean);
begin
  if AClear then
    mResults.Clear;
  if not AClear and (mResults.Lines.Count > 0) and (mResults.Lines[mResults.Lines.Count - 1] <> dxCRLF) and
    (mResults.Lines[mResults.Lines.Count - 1] <> '') and
    (mResults.Lines[mResults.Lines.Count - 1] <> EntityMemoDelimiter) then
    mResults.Lines.Add('');
end;

procedure TfrmEMFCustomTutorial.UpdateActionsButtons;
var
  I: Integer;
  AItem: TdxCustomLayoutItem;
begin
  lcMain.BeginUpdate;
  try
    for I := 0 to lgContent.Container.AbsoluteItemCount - 1 do
    begin
      AItem := lgContent.Container.AbsoluteItems[I];
      if (AItem is TdxLayoutItem) and (TdxLayoutItem(AItem).Control <> nil) and (TdxLayoutItem(AItem).Control is TcxButton) then
        TdxLayoutItem(AItem).Control.Width := GetActionButtonWidth;
    end;
  finally
    lcMain.EndUpdate;
  end;
end;

procedure TfrmEMFCustomTutorial.SetConnected(const AValue: Boolean);
begin
  if FConnected <> AValue then
  begin
    FConnected := AValue;
    UpdateControls;
  end;
end;

procedure TfrmEMFCustomTutorial.SetTutorialMode(const AValue: Boolean);
begin
  if FTutorialMode <> AValue then
  begin
    FTutorialMode := AValue;
    dxUIAdornerManager1.Guides.Active := FTutorialMode;
  end;
end;

procedure TfrmEMFCustomTutorial.dxUIAdornerManager1ActiveChanged(AManager: TdxUIAdornerManager;
  AAdorners: TdxCustomAdorners);
begin
  if dxUIAdornerManager1.Guides.Active and (dxUIAdornerManager1.Guides.Count > 0) then
    TdxGuideAccess(dxUIAdornerManager1.Guides[0]).ToggleCalloutPopup;
  frmMain.tsShowCode.Checked := dxUIAdornerManager1.Guides.Active;
end;

procedure TfrmEMFCustomTutorial.OnKeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    TdxGuidesAccess(dxUIAdornerManager1.Guides).ToggleCalloutPopup;
end;

procedure TfrmEMFCustomTutorial.dxUIAdornerManager1AdornerCustomDraw(AManager: TdxUIAdornerManager;
  AAdorner: TdxCustomAdorner; ACanvas: TdxGPCanvas; AViewInfo: TdxCustomAdornerViewInfo; var ADone: Boolean);
var
  AGuideViewInfo: TdxGuideViewInfo;
begin
  if AViewInfo is TdxGuideViewInfo then
  begin
    AGuideViewInfo := TdxGuideViewInfo(AViewInfo);
    ADone := (AGuideViewInfo.State = astNone) and not TdxGuide(AAdorner).Selected;
    if ADone then
      ACanvas.Rectangle(AGuideViewInfo.Bounds, dxacNone, dxacNone);
  end;
end;

procedure TfrmEMFCustomTutorial.dxUIAdornerManager1GuideGetCalloutPopupControl(AManager: TdxUIAdornerManager;
  AGuide: TdxGuide; out AControl: TWinControl);

  procedure DeleteItem(AControl: TControl);
  var
    AItem: TdxLayoutItem;
  begin
    AItem := lcTutorialInfo.FindItem(AControl);
    if AItem <> nil then
      AItem.Free;
  end;

var
  I: Integer;
  ATutorialInfo: TTutorialInfo;
  AItem: TdxLayoutItem;
  ASources: TcxRichEdit;
begin
  if FindTutorialInfo(AGuide, ATutorialInfo) then
  begin
    lcTutorialInfo.BeginUpdate;
    try
      liDescriptionLabel.Visible := ATutorialInfo.Description <> '';
      lilActionDescription.Visible := liDescriptionLabel.Visible;
      lilActionDescription.CaptionOptions.Text := ATutorialInfo.Description;
      for I := 0 to lgSources.Count - 1 do
        lgSources.Items[I].Visible := False;
      for I := ATutorialInfo.Entries.Count - 1 downto 0 do
      begin
        ASources := ATutorialInfo.Entries[I].Sources;
        AItem := lcTutorialInfo.FindItem(ASources);
        if AItem <> nil then
        begin
          AItem.Visible := True;
          AItem.Index := 0;
        end;
        ASources.OnKeyDown := OnKeyDownHandler;
        ASources.Properties.ScrollBars := ssVertical;
        ASources.Properties.WordWrap := True;
      end;
    finally
      lcTutorialInfo.EndUpdate;
    end;
    lcTutorialInfo.Width := Max(Width div 2, 450);
    lcTutorialInfo.Height := Max(Height div 2, 550);
    AControl := lcTutorialInfo;
  end;
end;

procedure TfrmEMFCustomTutorial.btnCopyClick(Sender: TObject);
var
  ARichEdit: TcxRichEdit;
  AActiveControl: TWinControl;
begin
  AActiveControl := (lgSources.Items[lgSources.ItemIndex] as TdxLayoutItem).Control as TWinControl;
  if (AActiveControl <> nil) and (AActiveControl is TcxRichEdit) then
  begin
    ARichEdit := TcxRichEdit(AActiveControl);
    ARichEdit.SelectAll;
    ARichEdit.CopyToClipboard;
  end;
end;

end.

