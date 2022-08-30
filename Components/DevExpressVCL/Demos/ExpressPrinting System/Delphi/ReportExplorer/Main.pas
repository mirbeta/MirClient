unit Main;

interface

{$I cxVer.inc}

uses
  Classes, Forms, Graphics, dxPSGlbl, dxPSUtl, dxPSEngn, dxPrnPg, dxBkgnd, dxWrap, 
  dxPrnDev, dxPSCore, dxPSCompsProvider, dxPSFillPatterns, dxPSEdgePatterns, 
  dxPSFileBasedXplorer, 
  dxPSShapes, dxPSContainerLnk, dxPSPrVwStd, dxPSPDFExportCore,
  dxPSPDFExport, cxDrawTextUtils, dxPScxEditorProducers,
  dxPScxExtEditorProducers, dxPScxPageControlProducer, dxPScxCommon, dxPScxExtCommon
  {The following units have to be added if you want to support all types of saved reports,
   i.e. reports that were created from all types of ReportLinks.
   These units contain registration information for all item types used to create them

   You must own appropriate Developer Express Inc. Control Libraries }

{$IFDEF EXPRESSSPREADSHEET2}
  , dxPSdxSpreadSheetLnk                                   { cxSpreadSheet }
{$ENDIF}
{$IFDEF EXPRESSLAYOUTCONTROL}
  , dxPSdxLCLnk                                  { dxLayoutControl }
{$ENDIF}
{$IFDEF EXPRESSGRID}
  , dxPScxGridLnk { cxGrid, cxTreeList, cxVerticalGrid and any others cx-family products }
{$ENDIF}
  ;

const
  cConfigFile: string = 'ReportExplorer.config';
  cFileName: string = 'FileName';
  cRootDirectory: string = 'RootDirectory';
  cRegistryPath = 'Software\Developer Express\ReportExplorer 1.0';

type
  TfmLauncher = class(TForm)
    dxPSFileBasedExplorer1: TdxPSFileBasedExplorer;
    dxComponentPrinter1: TdxComponentPrinter;
    dxPSEngineController1: TdxPSEngineController;
    procedure dxPSFileBasedExplorer1ItemDataLoadError(
      Sender: TCustomdxPSExplorer; AnItem: TdxPSExplorerItem;
      var AShowErrorMessage: Boolean; var AText: String);
  private
    FFileName: string;
    FRootDirectory: string;
    function GetConfigFileName: string;
  protected
    procedure ProcessCommandLine;
    procedure ProcessConfigFile;
    procedure ProcessStartupParams;
    
    procedure InitializeExplorer; virtual;
    procedure RunExplorer;

    procedure SaveCustomSettings;

    property ConfigFileName: string read GetConfigFileName;
    property FileName: string read FFileName write FFileName;
    property RootDirectory: string read FRootDirectory write FRootDirectory;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

var
  fmLauncher: TfmLauncher;

implementation

{$R *.dfm}

uses
  IniFiles, SysUtils;
  
{ Helpers }  

function TrimChars(const Source: string; ALeadingChar, ATrailingChar: Char): string;
begin
  Result := Source;
  if (Result <> '') and (ALeadingChar <> #0) then 
    while Result[1] = ALeadingChar do 
      Delete(Result, 1, 1);

  if (Result <> '') and (ATrailingChar <> #0) then     
    while Result[Length(Result)] = ATrailingChar do 
      Delete(Result, Length(Result), 1);
end;

function RemoveQuotes(const Source: string): string;
begin
  Result := TrimChars(Source, '''', '''');
end;

function RemoveLeadingSlash(const Source: string): string;
begin
  Result := TrimChars(Source, '\', #0);
end;

function RemoveTrailingSlash(const Source: string): string;
begin
  Result := TrimChars(Source, #0, '\');
end;

{ TfmLauncher }

constructor TfmLauncher.Create(AOwner: TComponent);
begin
  inherited;
  dxComponentPrinter1.ExplorerStubLink := dxComponentPrinter1.AddEmptyLink(TBasedxReportLink);
end;
  
destructor TfmLauncher.Destroy;
begin
  SaveCustomSettings;
  inherited;
end;

procedure TfmLauncher.AfterConstruction;
begin
  inherited;
  SetBounds(-100, -100, 50, 50); // move main window outside screen
  RunExplorer;
end;

procedure TfmLauncher.ProcessCommandLine;
begin
  if ParamCount > 0 then 
  begin
    FileName := ParamStr(1);
    if FileExists(FileName) then
      RootDirectory := ExtractFileDrive(FileName);
  end;  
end;

procedure TfmLauncher.ProcessConfigFile;
var
  ConfigFile: TStringList;
begin
  if FileExists(ConfigFileName) then
  begin
    ConfigFile := TStringList.Create;
    try
      ConfigFile.LoadFromFile(ConfigFileName);

      RootDirectory := ConfigFile.Values[cRootDirectory];
      FileName := ConfigFile.Values[cFileName];
      
      FileName := RemoveTrailingSlash(RootDirectory) + '\' + RemoveLeadingSlash(FileName);
    finally
      ConfigFile.Free;
    end;
  end;  
end;

procedure TfmLauncher.ProcessStartupParams;
begin
  ProcessCommandLine;
  if not FileExists(FileName) then 
    ProcessConfigFile;

  if not DirectoryExists(RootDirectory) then
    RootDirectory := ExtractFilePath(Application.ExeName) + '..\..\Data';
  if not FileExists(FileName) then
    FileName := '';
  if (RootDirectory = '') and (FileName <> '') then 
    RootDirectory := ExtractFileDir(FileName);
end;

procedure TfmLauncher.InitializeExplorer;
begin
  ProcessStartupParams;
  
  with dxPSFileBasedExplorer1 do 
  begin
    RootPath := RootDirectory;
    if RootPath = '' then 
      RootPath := 'C:\'; 
    if FileName <> '' then
      LoadItemData(FileName, dxComponentPrinter1.ExplorerStubLink);
  end;

  with dxComponentPrinter1.PreviewOptions do 
  begin
    ShowExplorer := True;
    VisibleOptions := VisibleOptions - [pvoReportDesign];
  end;   

  dxPSEngineController1.DialogsLookAndFeel.NativeStyle := True;
  dxPSEngineController1.OptionsStoring.RegistryPath := cRegistryPath;
end;

procedure TfmLauncher.RunExplorer;
begin
  Hide;
  try
    try
      InitializeExplorer;
    except
      on E: Exception do
        dxPSUtl.MessageError(E.Message);
    end;
    dxComponentPrinter1.Explore;
  finally
    Application.Terminate;
  end;  
end;

procedure TfmLauncher.SaveCustomSettings;

  function PreparedFileName(const Source: string): string;
  var
    P: Integer;
  begin
    Result := Source;
    P := Pos(dxPSFileBasedExplorer1.RealRootPath, Result);
    if P = 1 then 
      Delete(Result, 1, P + Length(dxPSFileBasedExplorer1.RealRootPath));
  end;
  
var
  ConfigFile: TStringList;
  S: string;
begin
  ConfigFile := TStringList.Create;
  try
    S := cFileName;
    if dxPSFileBasedExplorer1.LoadedItem <> nil then 
      S := S + '=' + PreparedFileName(TdxPSFileBasedExplorerItem(dxPSFileBasedExplorer1.LoadedItem).FullQualifiedFileName);

    ConfigFile.Add(S);  
    ConfigFile.Add(cRootDirectory + '=' + dxPSFileBasedExplorer1.RealRootPath);
       
    ConfigFile.SaveToFile(ConfigFileName);
  finally
    ConfigFile.Free;
  end;
end;

function TfmLauncher.GetConfigFileName: string;
begin
  Result := ExtractFilePath(Application.ExeName) + cConfigFile;
end;

procedure TfmLauncher.dxPSFileBasedExplorer1ItemDataLoadError(
  Sender: TCustomdxPSExplorer; AnItem: TdxPSExplorerItem;
  var AShowErrorMessage: Boolean; var AText: String);
const
  CRLF = #13#10;
  ErrorText: string = 'Cannot Load File "%s".' + CRLF + 
    'You should uncomment appropriate units in "uses" clause.' + CRLF + 
    CRLF +
    'Please read ReadMe.txt.';
begin
  AShowErrorMessage := True;
  AText := Format(ErrorText, [TdxPSFileBasedExplorerItem(AnItem).FileName]);
end;

end.
