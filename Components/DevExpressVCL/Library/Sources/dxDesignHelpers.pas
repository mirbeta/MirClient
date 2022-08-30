{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCommonLibrary                                     }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSCOMMONLIBRARY AND ALL          }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxDesignHelpers;

{$I cxVer.inc}

interface

uses
  Classes, Windows, DesignIntf, ToolsApi, DesignEditors, dxCoreReg;

type
  TdxOTARepositoryCategory = (dxcNewProject, dxcNewFiles);
  TdxOTAPersonality = (dxopDelphi, dxopCBuilder);

  { TdxOTACustomRepositoryWizard }

  TdxOTACustomRepositoryWizard = class(TNotifierObject,
    IOTARepositoryWizard,
    IOTARepositoryWizard60,
    IOTARepositoryWizard80,
    IOTAWizard)
  protected
    // IOTAWizard
    function GetIDString: string; virtual;
    function GetName: string; virtual; abstract;
    function GetState: TWizardState;
    procedure Execute; virtual; abstract;
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string; virtual;
    function GetGlyph: Cardinal; virtual;
    function GetPage: string;
    // IOTARepositoryWizard60
    function GetDesigner: string;
    // IOTARepositoryWizard80
    function IOTARepositoryWizard80.GetGalleryCategory = Wizard80GetGalleryCategory;
    function IOTARepositoryWizard80.GetPersonality = Wizard80GetPersonality;
    function Wizard80GetGalleryCategory: IOTAGalleryCategory;
    function Wizard80GetPersonality: string;
    function GetCategory: TdxOTARepositoryCategory; virtual;
    function GetPersonality: TdxOTAPersonality; virtual; abstract;
    //
    property Category: TdxOTARepositoryCategory read GetCategory;
    property Personality: TdxOTAPersonality read GetPersonality;
  end;

  { TdxOTACustomCreator }

  TdxOTACustomCreator = class(TInterfacedObject, IOTACreator)
  protected
    // IOTACreator
    function GetCreatorType: string; virtual; abstract;
    function GetExisting: Boolean; virtual;
    function GetFileSystem: string; virtual;
    function GetOwner: IOTAModule; virtual;
    function GetUnnamed: Boolean; virtual;
  end;

  { TdxOTAFormCreator }

  TdxOTAFormCreator = class(TdxOTACustomCreator, IOTAModuleCreator)
  private
    FFormTemplate: string;
    FImplTemplate: string;
    FIntfTemplate: string;
    function CreateOTAFile(const ATemplate, AModuleIdent, AFormIdent, AAncestorIdent: string): IOTAFile;
  protected
    function ExpandTemplate(const ATemplate, AModuleIdent, AFormIdent, AAncestorIdent: string): string; virtual;
    // IOTACreator
    function GetCreatorType: string; override;
    // IOTAModuleCreator
    function GetAncestorName: string; virtual;
    function GetFormName: string; virtual;
    function GetImplFileName: string; virtual;
    function GetIntfFileName: string; virtual;
    function GetMainForm: Boolean; virtual;
    function GetShowForm: Boolean; virtual;
    function GetShowSource: Boolean; virtual;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile; virtual;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; virtual;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile; virtual;
    procedure FormCreated(const FormEditor: IOTAFormEditor); virtual;
  public
    constructor Create(const AFormTemplate, AImplTemplate, AIntfTemplate: string);
    //
    property FormTemplate: string read FFormTemplate;
    property ImplTemplate: string read FImplTemplate;
    property IntfTemplate: string read FIntfTemplate;
  end;

  { TdxOTACustomProjectCreator }

  TdxOTACustomProjectCreator = class(TdxOTACustomCreator,
    IOTAProjectCreator50,
    IOTAProjectCreator80,
  {$IFDEF DELPHI16}
    IOTAProjectCreator160,
  {$ENDIF}
    IOTAProjectCreator)
  protected
    // IOTACreator
    function GetOwner: IOTAModule; override;
    // IOTAProjectCreator
    function GetFileName: string; virtual;
    function GetOptionFileName: string; // deprecated;
    function GetShowSource: Boolean; virtual;
    function NewOptionSource(const ProjectName: string): IOTAFile; // deprecated;
    function NewProjectSource(const ProjectName: string): IOTAFile; virtual;
    procedure NewDefaultModule; // deprecated;
    procedure NewProjectResource(const Project: IOTAProject); virtual;
    // IOTAProjectCreator50
    procedure NewDefaultProjectModule(const Project: IOTAProject); virtual;
    // IOTAProjectCreator80
    function GetProjectPersonality: string;
  {$IFDEF DELPHI16}
    // IOTAProjectCreator160
    function GetFrameworkType: string;
    function GetPlatforms: TArray<string>;
    function GetPreferredPlatform: string;
    procedure SetInitialOptions(const NewProject: IOTAProject);
  {$ENDIF}
    function GetPersonality: TdxOTAPersonality; virtual; abstract;
  end;

  { TdxOTAApplicationCreator }

  TdxOTAApplicationCreator = class(TdxOTACustomProjectCreator)
  strict private
    FPersonality: TdxOTAPersonality;
  protected
    function GetCreatorType: string; override;
    function GetPersonality: TdxOTAPersonality; override;
  public
    constructor Create(APersonality: TdxOTAPersonality); virtual;
  end;


function dxGetActiveProject: IOTAProject;
function dxGetActiveProjectFileName: string;
procedure dxRegisterPackageWizard(AWizard: TdxOTACustomRepositoryWizard);
implementation

uses
{$IFDEF DELPHI16}
  PlatformAPI,
{$ENDIF}
  SysUtils;

function dxGetActiveProject: IOTAProject;
begin
  Result := GetActiveProject;
end;

function dxGetActiveProjectFileName: string;
var
  AProject: IOTAProject;
begin
  AProject := dxGetActiveProject;
  if AProject <> nil then
    Result := AProject.FileName
  else
    Result := '';
end;

procedure dxRegisterPackageWizard(AWizard: TdxOTACustomRepositoryWizard);
begin
  RegisterPackageWizard(AWizard);
end;

{ TdxOTACustomRepositoryWizard }

function TdxOTACustomRepositoryWizard.GetIDString: string;
begin
  Result := ClassName;
end;

function TdxOTACustomRepositoryWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

function TdxOTACustomRepositoryWizard.GetAuthor: string;
begin
  Result := dxCompanyName;
end;

function TdxOTACustomRepositoryWizard.GetComment: string;
begin
  Result := '';
end;

function TdxOTACustomRepositoryWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

function TdxOTACustomRepositoryWizard.GetPage: string;
const
  CategoriesMap: array[TdxOTARepositoryCategory] of string = ('Projects', 'New');
begin
  Result := CategoriesMap[Category];
end;

function TdxOTACustomRepositoryWizard.GetDesigner: string;
begin
  Result := dVCL;
end;

function TdxOTACustomRepositoryWizard.Wizard80GetGalleryCategory: IOTAGalleryCategory;
const
  CategoriesMap: array[TdxOTAPersonality, TdxOTARepositoryCategory] of string = (
    (sCategoryDelphiNew, sCategoryDelphiNewFiles),
    (sCategoryCBuilderNew, sCategoryCBuilderNewFiles)
  );
var
  ACategoryManager: IOTAGalleryCategoryManager;
begin
  if Supports(BorlandIDEServices, IOTAGalleryCategoryManager, ACategoryManager) then
    Result := ACategoryManager.FindCategory(CategoriesMap[Personality, Category])
  else
    Result := nil;
end;

function TdxOTACustomRepositoryWizard.Wizard80GetPersonality: string;
const
  PersonalitiesMap: array[TdxOTAPersonality] of string = (
    sDelphiPersonality, sCBuilderPersonality
  );
begin
  Result := PersonalitiesMap[Personality];
end;

function TdxOTACustomRepositoryWizard.GetCategory: TdxOTARepositoryCategory;
begin
  Result := dxcNewFiles;
end;

{ TdxOTACustomCreator }

function TdxOTACustomCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TdxOTACustomCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TdxOTACustomCreator.GetOwner: IOTAModule;
begin
  Result := dxGetActiveProject;
end;

function TdxOTACustomCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

{ TdxOTAFormCreator }

constructor TdxOTAFormCreator.Create(const AFormTemplate, AImplTemplate, AIntfTemplate: string);
begin
  inherited Create;
  FFormTemplate := AFormTemplate;
  FImplTemplate := AImplTemplate;
  FIntfTemplate := AIntfTemplate;
end;

function TdxOTAFormCreator.ExpandTemplate(
  const ATemplate, AModuleIdent, AFormIdent, AAncestorIdent: string): string;
begin
  Result := ATemplate;
  Result := StringReplace(Result, '%FormIdent%', AFormIdent, [rfReplaceAll]);
  Result := StringReplace(Result, '%AncestorIdent%', AAncestorIdent, [rfReplaceAll]);
  Result := StringReplace(Result, '%ModuleIdent%', AModuleIdent, [rfReplaceAll]);
end;

function TdxOTAFormCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TdxOTAFormCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TdxOTAFormCreator.GetFormName: string;
begin
  Result := '';
end;

function TdxOTAFormCreator.GetImplFileName: string;
begin
  Result := '';
end;

function TdxOTAFormCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TdxOTAFormCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TdxOTAFormCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TdxOTAFormCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TdxOTAFormCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := CreateOTAFile(FormTemplate, '', FormIdent, AncestorIdent);
end;

function TdxOTAFormCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := CreateOTAFile(ImplTemplate, ModuleIdent, FormIdent, AncestorIdent);
end;

function TdxOTAFormCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := CreateOTAFile(IntfTemplate, ModuleIdent, FormIdent, AncestorIdent);
end;

procedure TdxOTAFormCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

function TdxOTAFormCreator.CreateOTAFile(const ATemplate, AModuleIdent, AFormIdent, AAncestorIdent: string): IOTAFile;
begin
  if ATemplate <> '' then
    Result := TOTAFile.Create(ExpandTemplate(ATemplate, AModuleIdent, AFormIdent, AAncestorIdent))
  else
    Result := nil;
end;

{ TdxOTACustomProjectCreator }

function TdxOTACustomProjectCreator.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TdxOTACustomProjectCreator.GetFileName: string;
begin
  Result := '';
end;

function TdxOTACustomProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TdxOTACustomProjectCreator.GetShowSource: Boolean;
begin
  Result := False;
end;

function TdxOTACustomProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

function TdxOTACustomProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  Result := nil; // Default project source
end;

procedure TdxOTACustomProjectCreator.NewDefaultModule;
begin
  // do nothing
end;

procedure TdxOTACustomProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
  // do nothing
end;

procedure TdxOTACustomProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
begin
  // do nothing
end;

function TdxOTACustomProjectCreator.GetProjectPersonality: string;
const
  PersonalitiesMap: array[TdxOTAPersonality] of string = (
    sDelphiPersonality, sCBuilderPersonality
  );
begin
  Result := PersonalitiesMap[GetPersonality];
end;

{$IFDEF DELPHI16}
function TdxOTACustomProjectCreator.GetFrameworkType: string;
begin
  Result := sFrameworkTypeVCL;
end;

function TdxOTACustomProjectCreator.GetPlatforms: TArray<string>;
begin
  SetLength(Result, 2);
  Result[0] := cWin32Platform;
  Result[1] := cWin64Platform;
end;

function TdxOTACustomProjectCreator.GetPreferredPlatform: string;
begin
  Result := cWin32Platform;
end;

procedure TdxOTACustomProjectCreator.SetInitialOptions(const NewProject: IOTAProject);
begin
  // do nothing
end;
{$ENDIF}

{ TdxOTAApplicationCreator }

constructor TdxOTAApplicationCreator.Create(APersonality: TdxOTAPersonality);
begin
  inherited Create;
  FPersonality := APersonality;
end;

function TdxOTAApplicationCreator.GetCreatorType: string;
begin
  Result := sApplication;
end;

function TdxOTAApplicationCreator.GetPersonality: TdxOTAPersonality;
begin
  Result := FPersonality;
end;

end.
