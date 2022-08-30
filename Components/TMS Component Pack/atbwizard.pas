{*************************************************************************}
{ TMS ToolBars component                                                  }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2006 - 2013                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage caused by the use of this code.                 }
{ The licensed user can use the source code royalty free for building any }
{ compiled application. The complete source code remains property of the  }
{ author and may not be distributed, published, given or sold in any form }
{ as such. No parts of the source code can be included in any other       }
{ component or application without                                        }
{ written authorization of the author.                                    }
{*************************************************************************}

unit ATBWizard;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, ToolsAPI, Dialogs;

type
  TATBApplicationWizard = class(TNotifierObject, IOTAWizard, IOTAProjectWizard, IOTARepositoryWizard, IUnknown
    {$IFDEF DELPHI2006_LVL}, IOTARepositoryWizard80{$ENDIF})
  private
    FUnitIdent: string;
    FClassName: string;
    FFileName: string;
    FProjectName: string;
  public
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;

  { IOTAProjectWizard }
    function GetAuthor : string;
    function GetComment : string;
    function GetPage : string;
    function GetGlyph: {$IFDEF DELPHI6_LVL}Cardinal{$ELSE}HICON{$ENDIF};
    procedure Execute;

    // IOTARepositoryWizard80
    {$IFDEF DELPHI2006_LVL}
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;
    function GetDesigner: string;
    {$ENDIF}
  protected
  end;

  TATBProjectCreator = class(TNotifierObject, IOTACreator, IOTAProjectCreator, IOTAProjectCreator50
  {$IFDEF DELPHI2006_LVL},IOTAProjectCreator80{$ENDIF})
  private
    FProjectFile: string;
    FProjectDirectory: string;
    FUnitName: string;
    FFormClass: string;
    FFileName: string;
  protected
    //IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;

    //IOTAProjectCreator
    function GetFileName: string;
    function GetOptionFileName: string;
    function GetShowSource: Boolean;
    procedure NewDefaultModule;
    function NewOptionSource(const ProjectName: string): IOTAFile;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;
    //IOTAProjectCreator50
    procedure NewDefaultProjectModule(const Project: IOTAProject);
    {$IFDEF DELPHI2006_LVL}
    function GetProjectPersonality: string;
    {$ENDIF}
  public
    constructor Create(ProjFile, ProjectDir, UnitName, FormClass, aFileName: string);
  end;


  TATBFrmWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard, IOTAFormWizard
    {$IFDEF VER180}, IOTAFormWizard100 {$ENDIF}
    {$IFDEF DELPHI2006_LVL}, IOTARepositoryWizard80{$ENDIF},IUnknown)
  private
    FUnitIdent: string;
    FClassName: string;
    FFileName: string;
  public
    // IOTAWizard methods
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard / IOTAFormWizard methods
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    //function GetGlyph: HICON;
    function GetGlyph: Cardinal;

    {$IFDEF DELPHI2006_LVL}
    function GetDesigner: string;
    property Designer: string read GetDesigner;
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;
    property GalleryCategory: IOTAGalleryCategory read GetGalleryCategory;
    property Personality: string read GetPersonality;
    {$ENDIF}
    {$IFDEF VER180}
    function IsVisible(Project: IOTAProject): Boolean;
    {$ENDIF}
  end;

  TATBFrmStyleWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard, IOTAFormWizard
    {$IFDEF VER180}, IOTAFormWizard100{$ENDIF}
    {$IFDEF DELPHI2006_LVL}, IOTARepositoryWizard80{$ENDIF},IUnknown)
  private
    FUnitIdent: string;
    FClassName: string;
    FFileName: string;
  public
    // IOTAWizard methods
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard / IOTAFormWizard methods
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    //function GetGlyph: HICON;
    function GetGlyph: Cardinal;

    {$IFDEF DELPHI2006_LVL}
    // 60
    function GetDesigner: string;
    property Designer: string read GetDesigner;
    // 80
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;
    property GalleryCategory: IOTAGalleryCategory read GetGalleryCategory;
    property Personality: string read GetPersonality;
    {$ENDIF}
    {$IFDEF VER180}
    function IsVisible(Project: IOTAProject): Boolean;
    {$ENDIF}
  end;

  TATBUnitCreator = class (TNotifierObject, IOTACreator, IOTAModuleCreator)
  private
    FIsStyleForm: Boolean;
    FUnitIdent: string;
    FClassName: string;
    FFileName: string;
    FIsMainForm: Boolean;
    FOwner : IOTAModule;
  public
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
    constructor Create(AOwner : IOTAModule; UnitIdent, ClassName, aFileName: string; AIsStyleForm: Boolean; AIsMainForm: Boolean = False);
  end;

  TBaseFile = class(TInterfacedObject)
  private
    FIsStyleForm: Boolean;
    FModuleName: string;
    FFormName: string;
    FAncestorName: string;
  public
    constructor Create(const ModuleName, FormName, AncestorName: string; AIsStyleForm: Boolean);
  end;

  TUnitFile = class(TBaseFile, IOTAFile)
  protected
    function GetSource: string;
    function GetAge: TDateTime;
  end;

  TFormFile = class(TBaseFile, IOTAFile)
  protected
    function GetSource: string;
    function GetAge: TDateTime;
  end;

  TATBProjectFile = class(TNotifierObject, IOTAFile)
  private
    FProjectName: string;
    FUnitName: string;
    FFormClass: string;
  public
    function GetSource : string;
    function GetAge : TDateTime;
    constructor Create(ProjName, UnitName, FormClass: string);
  end;

{$IFDEF DELPHI2006_LVL}
var
  EasyDelphiCategory: IOTAGalleryCategory = nil;
{$ENDIF}

implementation

uses
  Forms, SysUtils, DesignIntf, Registry, ShlObj;

const
  sAuthor = 'tmssoftware.com';
  sPage = 'TMS Forms';

{$R CodeGen.res}
{$R CodeGenFrm.res}
{$R CodeGenStyle.res}
{$R CodeGenStyleFrm.res}

//------------------------------------------------------------------------------

function GetCurrentProject: IOTAProject;
var
  LServices: IOTAModuleServices;
  LModule: IOTAModule;
  LProject: IOTAProject;
  LProjectGroup: IOTAProjectGroup;
  LMultipleProjects: Boolean;
  i: Integer;
begin
  Result := nil;
  LMultipleProjects := False;
  LServices := BorlandIDEServices as IOTAModuleServices;
  for i := 0 to LServices.ModuleCount - 1 do
  begin
    LModule := LServices.Modules[I];
    if LModule.QueryInterface(IOTAProjectGroup, LProjectGroup) = S_OK then
    begin
      Result := LProjectGroup.ActiveProject;
      Exit;
    end
    else if LModule.QueryInterface(IOTAProject, LProject) = S_OK then
    begin
      if Result = nil then
        Result := LProject
      else
      begin
        LMultipleProjects := True;
      end;
    end;
  end;

  if LMultipleProjects then
    Result := nil;
end;

//------------------------------------------------------------------------------

function IncludeTrailingPathDelim(const asPath: string): string;
begin
  Result := asPath;
  if Length(Result) > 0 then begin
    if Result[Length(Result)] <> PATHDELIM then begin
      Result := Result + PATHDELIM;
    end;
  end;
end;

//------------------------------------------------------------------------------

function GetMyDocuments: string;
var
  r: Bool;
  path: array[0..Max_Path] of Char;
begin
  r := ShGetSpecialFolderPath(0, path, CSIDL_Personal, False) ;
  if not r then
    raise Exception.Create('Could not find MyDocuments folder location.') ;
  Result := Path;
end;

//------------------------------------------------------------------------------

function GetIDEProjectPath: string;
var
  LPath: string;

begin
  {$IFNDEF DELPHI2006_LVL}
  LPAth := ExtractFileDir(ParamStr(0));
  if Pos('BIN', UpperCase(LPAth)) > 0 then
  begin
    Delete(LPath, Pos('BIN', UpperCase(LPath)), 3);
  end;

  LPath := IncludeTrailingPathDelim(LPath) + 'Projects' + PATHDELIM;
  Result := LPAth;
  {$ELSE}
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    {$IFNDEF VER185}
    {$IFDEF VER180} // Delphi 2006
    if OpenKey('\Software\Borland\BDS\4.0\Globals', False) then begin
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VER185} // Delphi 2007
    if OpenKey('\Software\Borland\BDS\5.0\Globals', False) then begin
    {$ENDIF}
    {$IFDEF VER200} // Delphi 2009
    if OpenKey('\Software\CodeGear\BDS\6.0\Globals', False) then begin
    {$ENDIF}
    {$IFDEF VER210} // Delphi 2010
    if OpenKey('\Software\CodeGear\BDS\7.0\Globals', False) then begin
    {$ENDIF}
    {$IFDEF VER220} // Delphi XE
    if OpenKey('\Software\Embarcadero\BDS\8.0\Globals', False) then begin
    {$ENDIF}
    {$IFDEF VER230} // Delphi XE2
    if OpenKey('\Software\Embarcadero\BDS\9.0\Globals', False) then begin
    {$ENDIF}
    {$IFDEF VER240} // Delphi XE3
    if OpenKey('\Software\Embarcadero\BDS\10.0\Globals', False) then begin
    {$ENDIF}
    {$IFDEF VER250} // Delphi XE4
    if OpenKey('\Software\Embarcadero\BDS\11.0\Globals', False) then begin
    {$ENDIF}
    {$IFDEF VER260} // Delphi XE5
    if OpenKey('\Software\Embarcadero\BDS\12.0\Globals', False) then begin
    {$ENDIF}
    {$IFDEF VER270} // Delphi XE6
    if OpenKey('\Software\Embarcadero\BDS\14.0\Globals', False) then begin
    {$ENDIF}
    {$IFDEF VER280} // Delphi XE7
    if OpenKey('\Software\Embarcadero\BDS\15.0\Globals', False) then begin
    {$ENDIF}
    {$IFDEF VER290} // Delphi XE8
    if OpenKey('\Software\Embarcadero\BDS\16.0\Globals', False) then begin
    {$ENDIF}
    {$IFDEF VER300} // Delphi XE9
    if OpenKey('\Software\Embarcadero\BDS\17.0\Globals', False) then begin
    {$ENDIF}
      LPath := ReadString('DefaultProjectsDirectory');
      CloseKey;
    end;
    if LPath = '' then
    begin
      LPath := GetMyDocuments;

      {$IFNDEF DELPHI2007_LVL}
      LPath := IncludeTrailingPathDelim(LPath) + 'Borland Studio Projects' + PATHDELIM;
      {$ENDIF}

      {$IFDEF DELPHI2007_LVL}
      LPath := IncludeTrailingPathDelim(LPath) + 'RAD Studio\Projects' + PATHDELIM;
      {$ENDIF}

      if not DirectoryExists(LPath) then
        ForceDirectories(LPath);

    end
    else
    begin
      LPath := IncludeTrailingPathDelim(LPath);
    end;
    Result := LPath;
  finally
    Free;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function MakeFileName(const ProjectDirectory, ABaseFilename: string; const AExt: string): string;
begin
  if AExt <> '' then begin
    Result := ProjectDirectory + ABaseFilename + '.' + AExt;
  end else begin
  	Result := ProjectDirectory + ABaseFilename;
  end;
end;

//------------------------------------------------------------------------------

function GetActiveProjectGroup: IOTAProjectGroup;
var
  ModuleServices: IOTAModuleServices;
  i: Integer;
begin
  Result := nil;
  if Assigned(BorlandIDEServices) then
  begin
    ModuleServices := BorlandIDEServices as IOTAModuleServices;
    for i := 0 to ModuleServices.ModuleCount - 1 do
      if Supports(ModuleServices.Modules[i], IOTAProjectGroup, Result) then
        Break;
  end;
end;

//------------------------------------------------------------------------------

function ProjectExists(const AProjectGroup:IOTAProjectGroup; AProject:string):boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to AProjectGroup.ProjectCount - 1 do
  begin
    if UpperCase(ChangeFileExt(ExtractFileName(AProjectGroup.Projects[i].FileName),'')) = UpperCase(AProject) then
    begin
      Result := true;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function FindNewProjectName(const AProjectGroup: IOTAProjectGroup): string;
var
  i: Integer;
begin
  i := 1;
  if Assigned(AProjectGroup) then
  begin
    while ProjectExists(AProjectGroup, Format('Project%d',[i])) do
      inc(i);
  end;

  Result := Format('Project%d',[i]);
end;

//------------------------------------------------------------------------------

{ TBaseFile }
constructor TBaseFile.Create(const ModuleName, FormName, AncestorName: string; AIsStyleForm: Boolean);
begin
  FIsStyleForm := AIsStyleForm;
  inherited Create;
  FModuleName := ModuleName;
  FFormName := FormName;
  FAncestorName := AncestorName;
end;

//------------------------------------------------------------------------------

{ TUnitFile }
function TUnitFile.GetSource: string;
var
  Text: ansistring;
  ResInstance: THandle;
  HRes: HRSRC;
  resname: ansistring;
begin
  if FIsStyleForm then
  begin
    resname := 'CODEGENSTYLE';
    ResInstance := FindResourceHInstance(HInstance);
    HRes := FindResourceA(ResInstance, PAnsiChar(resname), PAnsiChar(10));
    Text := PAnsiChar(LockResource(LoadResource(ResInstance, HRes)));
    SetLength(Text, SizeOfResource(ResInstance, HRes));
    Result := Format(string(Text), [FModuleName, FFormName, FAncestorName]);
  end
  else
  begin
    resname := 'CODEGEN';
    ResInstance := FindResourceHInstance(HInstance);
    HRes := FindResourceA(ResInstance, PAnsiChar(resname), PAnsiChar(10));
    Text := PAnsiChar(LockResource(LoadResource(ResInstance, HRes)));
    SetLength(Text, SizeOfResource(ResInstance, HRes));
    Result := Format(string(Text), [FModuleName, FFormName, FAncestorName]);
  end;
end;

//------------------------------------------------------------------------------

function TUnitFile.GetAge: TDateTime;
begin
  Result := -1;
end;

//------------------------------------------------------------------------------

{ TFormFile }
function TFormFile.GetSource: string;
var
  Text: ansistring;
  ResInstance: THandle;
  HRes: HRSRC;
  resname: ansistring;
begin
  if FIsStyleForm then
  begin
    resname := 'CODEGENSTYLEFRM';
    ResInstance := FindResourceHInstance(HInstance);
    HRes := FindResourceA(ResInstance, PAnsiChar(resname), PAnsiChar(10));
    Text := PAnsiChar(LockResource(LoadResource(ResInstance, HRes)));
    SetLength(Text, SizeOfResource(ResInstance, HRes));
    Result := Format(string(Text), [FFormName]);
  end
  else
  begin
    resname := 'CODEGENFRM';
    ResInstance := FindResourceHInstance(HInstance);
    HRes := FindResourceA(ResInstance, PAnsiChar(resname), PAnsiChar(10));
    Text := PAnsiChar(LockResource(LoadResource(ResInstance, HRes)));
    SetLength(Text, SizeOfResource(ResInstance, HRes));
    Result := Format(string(Text), [FFormName]);
  end;
end;

//------------------------------------------------------------------------------

function TFormFile.GetAge: TDateTime;
begin
  Result := -1;
end;

//------------------------------------------------------------------------------

{ TATBFrmWizard }
{ TATBFrmWizard.IOTAWizard }
function TATBFrmWizard.GetIDString: string;
begin
  Result := 'TMS.ToolBarWizard';
end;

//------------------------------------------------------------------------------

function TATBFrmWizard.GetName: string;
begin
  Result := 'TMS Office 2007 ribbon form';
end;

//------------------------------------------------------------------------------

function TATBFrmWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

//------------------------------------------------------------------------------

procedure TATBFrmWizard.Execute;
var
  LProj : IOTAProject;
begin
  {$IFDEF DELPHI2006_LVL}
  (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName('', FUnitIdent, FClassName, FFileName);
  FClassName := 'TMSForm' + Copy(FUnitIdent, 5, Length(FUnitIdent));
  {$ELSE}
  (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName('TMSForm', FUnitIdent, FClassName, FFileName);
  {$ENDIF}

  LProj := GetCurrentProject;
  if LProj <> nil then
  begin
    (BorlandIDEServices as IOTAModuleServices).CreateModule(TATBUnitCreator.Create(LProj, FUnitIdent, FClassName, FFileName, false));
  end;
end;

//------------------------------------------------------------------------------

{$IFDEF DELPHI2006_LVL}
function TATBFrmWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := nil;
  {
  if (EasyDelphiCategory = nil) then
      EasyDelphiCategory := IOTAGalleryCategoryManager(BorlandIDEServices as IOTAGalleryCategoryManager).AddCategory(
        IOTAGalleryCategoryManager(BorlandIDEServices as IOTAGalleryCategoryManager).FindCategory(sCategoryDelphiNew),
        'TMS.AdvToolBarWizard', sCategory, 0);
  Result := EasyDelphiCategory;
  }
end;

//------------------------------------------------------------------------------

function TATBFrmWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

//------------------------------------------------------------------------------

function TATBFrmWizard.GetDesigner: string;
begin
  Result := dVCL;
end;
{$ENDIF}

//------------------------------------------------------------------------------
{$IFDEF VER180}
function TATBFrmWizard.IsVisible(Project: IOTAProject): Boolean;
begin
  Result := True;
end;
{$ENDIF}

//------------------------------------------------------------------------------

function TATBFrmWizard.GetGlyph: Cardinal;
begin
  Result := 0;  // use standard icon
end;

//------------------------------------------------------------------------------

function TATBFrmWizard.GetPage: string;
begin
  Result := sPage;
end;

//------------------------------------------------------------------------------

function TATBFrmWizard.GetAuthor: string;
begin
  Result := sAuthor;
end;

//------------------------------------------------------------------------------

function TATBFrmWizard.GetComment: string;
begin
  Result := 'Creates a new ToolBar form.'
end;

{ TATBFrmWizard.IOTAWizard }
function TATBFrmStyleWizard.GetIDString: string;
begin
  Result := 'TMS.ToolBarStyleFrom';
end;

//------------------------------------------------------------------------------

function TATBFrmStyleWizard.GetName: string;
begin
  Result := 'TMS ToolBar Style Form';
end;

//------------------------------------------------------------------------------

function TATBFrmStyleWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

//------------------------------------------------------------------------------

procedure TATBFrmStyleWizard.Execute;
var
  LProj : IOTAProject;
begin
  {$IFDEF DELPHI2006_LVL}
  (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName('', FUnitIdent, FClassName, FFileName);
  FClassName := 'TMSForm' + Copy(FUnitIdent, 5, Length(FUnitIdent));
  {$ELSE}
  (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName('TMSForm', FUnitIdent, FClassName, FFileName);
  {$ENDIF}
  //(BorlandIDEServices as IOTAModuleServices).CreateModule(Self);
  LProj := GetCurrentProject;
  if LProj <> nil then
  begin
    (BorlandIDEServices as IOTAModuleServices).CreateModule(TATBUnitCreator.Create(LProj, FUnitIdent, FClassName, FFileName, true));
  end;
end;

//------------------------------------------------------------------------------

{$IFDEF DELPHI2006_LVL}
{ TATBFrmWizard.IOTARepositoryWizard / TATBFrmWizard.IOTAFormWizard }
function TATBFrmStyleWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := nil;
  {
  if (EasyDelphiCategory = nil) then
      EasyDelphiCategory := IOTAGalleryCategoryManager(BorlandIDEServices as IOTAGalleryCategoryManager).AddCategory(
        IOTAGalleryCategoryManager(BorlandIDEServices as IOTAGalleryCategoryManager).FindCategory(sCategoryDelphiNew),
        'TMS.AdvToolBarWizard', sCategory, 0);
  Result := EasyDelphiCategory;
  }
end;

//------------------------------------------------------------------------------

function TATBFrmStyleWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

//------------------------------------------------------------------------------

function TATBFrmStyleWizard.GetDesigner: string;
begin
  Result := dVCL;
end;
{$ENDIF}

//------------------------------------------------------------------------------
{$IFDEF VER180}
function TATBFrmStyleWizard.IsVisible(Project: IOTAProject): Boolean;
begin
  Result := True;
end;
{$ENDIF}

//------------------------------------------------------------------------------

function TATBFrmStyleWizard.GetGlyph: Cardinal;
begin
  Result := 0;  // use standard icon
end;

//------------------------------------------------------------------------------

function TATBFrmStyleWizard.GetPage: string;
begin
  Result := sPage;
end;

//------------------------------------------------------------------------------

function TATBFrmStyleWizard.GetAuthor: string;
begin
  Result := sAuthor;
end;

//------------------------------------------------------------------------------

function TATBFrmStyleWizard.GetComment: string;
begin
  Result := 'Creates a new Office 2007 ribbon form.'
end;

//------------------------------------------------------------------------------
{
function TATBFrmWizard.GetOwner: IOTAModule;
var
  I: Integer;
  ModServ: IOTAModuleServices;
  Module: IOTAModule;
  ProjGrp: IOTAProjectGroup;
begin
  Result := nil;
  ModServ := BorlandIDEServices as IOTAModuleServices;
  for I := 0 to ModServ.ModuleCount - 1 do
  begin
    Module := ModSErv.Modules[I];
    // find current project group
    if CompareText(ExtractFileExt(Module.FileName), '.bpg') = 0 then
      if Module.QueryInterface(IOTAProjectGroup, ProjGrp) = S_OK then
      begin
        // return active project of group
        Result := ProjGrp.GetActiveProject;
        Exit;
      end;
  end;
end;

//------------------------------------------------------------------------------

function TATBFrmWizard.GetImplFileName: string;
var
  CurrDir: array[0..MAX_PATH] of char;
begin
  // Note: full path name required!
  GetCurrentDirectory(SizeOf(CurrDir), CurrDir);
  Result := Format('%s\%s.pas', [CurrDir, FUnitIdent, '.pas']);
end;
}
//------------------------------------------------------------------------------

{ TATBUnitCreator }

constructor TATBUnitCreator.Create(AOwner: IOTAModule; UnitIdent, ClassName, aFileName: string; AIsStyleForm: Boolean; AIsMainForm: Boolean);
begin
  FIsStyleForm := AIsStyleForm;
  FUnitIdent := UnitIdent;
  FClassName := ClassName;
  FFileName := aFileName;
  inherited Create;
  FOwner := AOwner;
  FIsMainForm := AIsMainForm;
end;

//------------------------------------------------------------------------------

procedure TATBUnitCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
//
end;

//------------------------------------------------------------------------------

function TATBUnitCreator.GetAncestorName: string;
begin
  if not FIsStyleForm then
    Result := 'TAdvToolBarForm'
  else
    Result := 'TStyleForm';
end;

//------------------------------------------------------------------------------

function TATBUnitCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

//------------------------------------------------------------------------------

function TATBUnitCreator.GetExisting: Boolean;
begin
  Result := False;
end;

//------------------------------------------------------------------------------

function TATBUnitCreator.GetFileSystem: string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------

function TATBUnitCreator.GetFormName: string;
begin
  Result := FClassName;
end;

//------------------------------------------------------------------------------

function TATBUnitCreator.GetImplFileName: string;
var
  ProjectDir: string;
begin
  ProjectDir := GetIDEProjectPath;
  ProjectDir := IncludeTrailingPathDelim(ProjectDir);

{$IFDEF DELPHI9_LVL}
  if not FIsMainForm then
  begin
    //Result := ProjectOptions.FormFile;

    //Result := FFileName;
  end
  else
  begin
    Result := MakeFileName(ProjectDir, FUnitIdent, 'pas');
  end;
{$ELSE}
  Result := MakeFileName(ProjectDir, FUnitIdent, 'pas');
{$ENDIF}
end;

//------------------------------------------------------------------------------

function TATBUnitCreator.GetIntfFileName: string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------

function TATBUnitCreator.GetMainForm: Boolean;
begin
  Result := FIsMainForm;
end;

//------------------------------------------------------------------------------

function TATBUnitCreator.GetOwner: IOTAModule;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

function TATBUnitCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

//------------------------------------------------------------------------------

function TATBUnitCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

//------------------------------------------------------------------------------

function TATBUnitCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

//------------------------------------------------------------------------------

function TATBUnitCreator.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TFormFile.Create('', FormIdent, AncestorIdent, FIsStyleForm);
end;

//------------------------------------------------------------------------------

function TATBUnitCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TUnitFile.Create(ModuleIdent, FormIdent, AncestorIdent, FIsStyleForm);
end;

//------------------------------------------------------------------------------

function TATBUnitCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

//------------------------------------------------------------------------------

{ TATBApplicationWizard }
procedure TATBApplicationWizard.Execute;
var
  //LProj : IOTAProject;
  LModuleServices : IOTAModuleServices;
  ProjectDir: string;
begin
  LModuleServices := (BorlandIDEServices as IOTAModuleServices);
  FProjectName := FindNewProjectName(GetActiveProjectGroup);
  ProjectDir := GetIDEProjectPath;
  ProjectDir := IncludeTrailingPathDelim(ProjectDir);

  {$IFDEF DELPHI2006_LVL}
  LModuleServices.GetNewModuleAndClassName('', FUnitIdent, FClassName, FFileName);
  FClassName := 'TMSForm' + Copy(FUnitIdent, 5, Length(FUnitIdent));
  {$ELSE}
  LModuleServices.GetNewModuleAndClassName('TMSForm', FUnitIdent, FClassName, FFileName);
  {$ENDIF}

  LModuleServices.CreateModule(TATBProjectCreator.Create(FProjectName, ProjectDir, FUnitIdent, FClassName, FFileName));
end;

//------------------------------------------------------------------------------

function TATBApplicationWizard.GetAuthor: string;
begin
  Result := sAuthor;
end;

//------------------------------------------------------------------------------

function TATBApplicationWizard.GetComment: string;
begin
  Result := 'AdvToolBar Application Wizard';
end;

//------------------------------------------------------------------------------

{$IFDEF DELPHI2006_LVL}
function TATBApplicationWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := nil;
  {
  if (EasyDelphiCategory = nil) then
      EasyDelphiCategory := IOTAGalleryCategoryManager(BorlandIDEServices as IOTAGalleryCategoryManager).AddCategory(
        IOTAGalleryCategoryManager(BorlandIDEServices as IOTAGalleryCategoryManager).FindCategory(sCategoryDelphiNew),
        'TMS.ATBApplicationWizard', sCategory, 0);
  Result := EasyDelphiCategory;
  }
end;

//------------------------------------------------------------------------------

function TATBApplicationWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

//------------------------------------------------------------------------------

function TATBApplicationWizard.GetDesigner: string;
begin
  Result := dVCL;
end;
{$ENDIF}

//------------------------------------------------------------------------------

function TATBApplicationWizard.GetGlyph: {$IFDEF DELPHI6_LVL}Cardinal{$ELSE}HICON{$ENDIF};
begin
  result := 0;
end;

//------------------------------------------------------------------------------

function TATBApplicationWizard.GetIDString: string;
begin
  Result := 'TMS.ATBApplicationWizard';
end;

//------------------------------------------------------------------------------

function TATBApplicationWizard.GetName: string;
begin
  Result := 'TMS Office 2007 ribbon application';
end;

//------------------------------------------------------------------------------

function TATBApplicationWizard.GetPage: string;
begin
  Result := sPage;
end;

//------------------------------------------------------------------------------

function TATBApplicationWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

//------------------------------------------------------------------------------

{ TATBProjectCreator }

constructor TATBProjectCreator.Create(ProjFile, ProjectDir, UnitName, FormClass, aFileName: string);
begin
  inherited Create;
  FProjectFile := ProjFile;
  FProjectDirectory := ProjectDir;
  FUnitName := UnitName;
  FFormClass := FormClass;
  FFileName := aFileName;
end;

//------------------------------------------------------------------------------

function TATBProjectCreator.GetCreatorType: string;
begin
  Result := sApplication;
end;

//------------------------------------------------------------------------------

function TATBProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

//------------------------------------------------------------------------------

function TATBProjectCreator.GetFileName: string;
begin
  Result := FProjectDirectory + FProjectFile + '.dpr';
end;

//------------------------------------------------------------------------------

function TATBProjectCreator.GetFileSystem: string;
begin
  result := '';
end;

//------------------------------------------------------------------------------

function TATBProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------

function TATBProjectCreator.GetOwner: IOTAModule;
begin
  result := nil;
end;

//------------------------------------------------------------------------------

{$IFDEF DELPHI2006_LVL}
function TATBProjectCreator.GetProjectPersonality: string;
begin
{$IFDEF DELPHI2006_LVL}
   Result := sDelphiPersonality;
{$ELSE}
  Result := 'Delphi.Personality';
{$ENDIF}
end;
{$ENDIF}

//------------------------------------------------------------------------------

function TATBProjectCreator.GetShowSource: Boolean;
begin
  Result := True; //not FIsBCB;
end;

//------------------------------------------------------------------------------

function TATBProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

//------------------------------------------------------------------------------

procedure TATBProjectCreator.NewDefaultModule;
begin
//
end;

//------------------------------------------------------------------------------

procedure TATBProjectCreator.NewDefaultProjectModule( const Project: IOTAProject);
var
  LModuleServices : IOTAModuleServices;
begin
  LModuleServices := (BorlandIDEServices as IOTAModuleServices);
  LModuleServices.CreateModule(TATBUnitCreator.Create(Project, FUnitName, FFormClass, FFileName, false));
end;

//------------------------------------------------------------------------------

function TATBProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

//------------------------------------------------------------------------------

procedure TATBProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
//
end;

//------------------------------------------------------------------------------

function TATBProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  result := TATBProjectFile.Create(ProjectName, FUnitName, FFormClass);
end;

//------------------------------------------------------------------------------

{ TATBProjectFile }

constructor TATBProjectFile.Create(ProjName, UnitName, FormClass: string);
begin
  inherited Create;
  FProjectName := ProjName;
  FUnitName := UnitName;
  FFormClass := FormClass;
end;

//------------------------------------------------------------------------------

function TATBProjectFile.GetAge: TDateTime;
begin
  Result := -1;
end;

//------------------------------------------------------------------------------

function TATBProjectFile.GetSource: string;
begin
  Result := 'program ' + FProjectName + ';' + #13#10 + #13#10 +
            {$IFDEF DELPHI2006_LVL}
            'uses Forms;'+#13#10+
            {$ELSE}
            'uses Forms,'+#13#10+
            ' '+FUnitName+' in '''+FUnitName+'.pas'' {'+FFormClass+'};' + #13#10 +
            {$ENDIF}
            '{$R *.res}' + #13#10 + #13#10 +
            'begin' + #13#10 +
            '  Application.Initialize;'+ #13#10 +
            {$IFDEF DELPHI2006_LVL}
            {$ELSE}
            '  Application.CreateForm(T'+FFormClass+', '+FFormClass+');' + #13#10 +
            {$ENDIF}
            '  Application.Run;' + #13#10 +
            'end.';
end;

//------------------------------------------------------------------------------

initialization

finalization

end.
