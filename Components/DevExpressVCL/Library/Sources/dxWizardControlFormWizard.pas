{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressWizardControl                                     }
{                                                                    }
{           Copyright (c) 2012-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSWIZARDCONTROL AND ALL          }
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

unit dxWizardControlFormWizard;

{$I cxVer.inc}

interface

uses
  Classes, Windows, DesignIntf, ToolsApi, DesignEditors,
  dxWizardControlReg, dxCoreReg, dxWizardControlForm, dxDesignHelpers;

const
  sdxWizardControlApplicationWizardComment = 'Create a Wizard VCL application';
  sdxWizardControlApplicationWizardName = 'DevExpress VCL %version% Wizard Application';
  sdxWizardControlFormWizardComment = 'Create a Wizard VCL form';
  sdxWizardControlFormWizardName = 'DevExpress VCL %version% Wizard Form';

type

  { TdxOTACustomWizardControlWizard }

  TdxOTACustomWizardControlWizard = class(TdxOTACustomRepositoryWizard, IOTAFormWizard)
  private
    FPersonality: TdxOTAPersonality;
  protected
    function ExpandWizardControlName(const AFormat: string): string;
    function GetGlyph: Cardinal; override;
    function GetIDString: string; override;
    function GetPersonality: TdxOTAPersonality; override;
  public
    constructor Create(APersonality: TdxOTAPersonality); virtual;
    //
    property Personality: TdxOTAPersonality read GetPersonality;
  end;

  { TdxOTAWizardControlApplicationCreator }

  TdxOTAWizardControlApplicationCreator = class(TdxOTACustomProjectCreator)
  private
    FPersonality: TdxOTAPersonality;
  protected
    function GetCreatorType: string; override;
    function GetPersonality: TdxOTAPersonality; override;
  public
    constructor Create(APersonality: TdxOTAPersonality); virtual;
  end;

  { TdxOTAWizardControlApplicationWizard }

  TdxOTAWizardControlApplicationWizard = class(TdxOTACustomWizardControlWizard, IOTAProjectWizard)
  protected
    procedure Execute; override;
    function GetCategory: TdxOTARepositoryCategory; override;
    function GetComment: string; override;
    function GetName: string; override;
  end;

  { TdxOTAWizardControlFormCreator }

  TdxOTAWizardControlFormCreator = class(TdxOTAFormCreator)
  private
    function LoadTemplate(const AName: string): string;
    procedure LoadTemplates(APersonality: TdxOTAPersonality;
      out AFormTemplate, AImplTemplate, AIntfTemplate: string);
  public
    constructor Create(APersonality: TdxOTAPersonality);
  end;

  { TdxOTAWizardControlFormWizard }

  TdxOTAWizardControlFormWizard = class(TdxOTACustomWizardControlWizard, IOTAProjectWizard)
  protected
    procedure Execute; override;
    function GetComment: string; override;
    function GetName: string; override;
  end;

implementation

uses
  SysUtils, dxCore;

{$R dxWizardControlFormWizard.res}

{ TdxOTACustomWizardControlWizard }

constructor TdxOTACustomWizardControlWizard.Create(APersonality: TdxOTAPersonality);
begin
  inherited Create;
  FPersonality := APersonality;
end;

function TdxOTACustomWizardControlWizard.ExpandWizardControlName(const AFormat: string): string;
begin
  Result := StringReplace(AFormat, '%version%', dxGetShortBuildNumberAsString, [rfReplaceAll]);
end;

function TdxOTACustomWizardControlWizard.GetGlyph: Cardinal;
begin
  Result := LoadIcon(HInstance, 'DXWIZARDCONTROLFORMWIZARD');
end;

function TdxOTACustomWizardControlWizard.GetIDString: string;
const
  PersonalitiesMap: array[TdxOTAPersonality] of string = ('Delphi', 'CBuilder');
begin
  Result := inherited GetIDString + '.' + PersonalitiesMap[Personality];
end;

function TdxOTACustomWizardControlWizard.GetPersonality: TdxOTAPersonality;
begin
  Result := FPersonality;
end;

{ TdxOTAWizardControlApplicationCreator }

constructor TdxOTAWizardControlApplicationCreator.Create(APersonality: TdxOTAPersonality);
begin
  inherited Create;
  FPersonality := APersonality;
end;

function TdxOTAWizardControlApplicationCreator.GetCreatorType: string;
begin
  Result := sApplication;
end;

function TdxOTAWizardControlApplicationCreator.GetPersonality: TdxOTAPersonality;
begin
  Result := FPersonality;
end;

{ TdxOTAWizardControlApplicationWizard }

procedure TdxOTAWizardControlApplicationWizard.Execute;
var
  AModuleServices: IOTAModuleServices;
begin
  if Supports(BorlandIDEServices, IOTAModuleServices, AModuleServices) then
  begin
    AModuleServices.CreateModule(TdxOTAWizardControlApplicationCreator.Create(Personality));
    AModuleServices.CreateModule(TdxOTAWizardControlFormCreator.Create(Personality));
  end;
end;

function TdxOTAWizardControlApplicationWizard.GetCategory: TdxOTARepositoryCategory;
begin
  Result := dxcNewProject;
end;

function TdxOTAWizardControlApplicationWizard.GetComment: string;
begin
  Result := ExpandWizardControlName(sdxWizardControlApplicationWizardComment);
end;

function TdxOTAWizardControlApplicationWizard.GetName: string;
begin
  Result := ExpandWizardControlName(sdxWizardControlApplicationWizardName);
end;

{ TdxOTAWizardControlFormCreator }

constructor TdxOTAWizardControlFormCreator.Create(APersonality: TdxOTAPersonality);
var
  AFormTemplate, AImplTemplate, AIntfTemplate: string;
begin
  LoadTemplates(APersonality, AFormTemplate, AImplTemplate, AIntfTemplate);
  inherited Create(AFormTemplate, AImplTemplate, AIntfTemplate);
end;

function TdxOTAWizardControlFormCreator.LoadTemplate(const AName: string): string;
var
  AList: TStringList;
  AStream: TResourceStream;
begin
  AStream := TResourceStream.Create(HInstance, AName, 'DXWIZARDTEMPLATES');
  try
    AList := TStringList.Create;
    try
      AList.LoadFromStream(AStream);
      Result := AList.Text;
    finally
      AList.Free;
    end;
  finally
    AStream.Free;
  end;
end;

procedure TdxOTAWizardControlFormCreator.LoadTemplates(APersonality: TdxOTAPersonality;
  out AFormTemplate, AImplTemplate, AIntfTemplate: string);
begin
  AFormTemplate := LoadTemplate('WIZARDCONTROLFORM');
  if APersonality = dxopCBuilder then
  begin
    AIntfTemplate := LoadTemplate('WIZARDCONTROLCBUILDERHEADER');
    AImplTemplate := LoadTemplate('WIZARDCONTROLCBUILDERUNIT');
  end
  else
  begin
    AImplTemplate := LoadTemplate('WIZARDCONTROLDELPHIUNIT');
    AIntfTemplate := '';
  end;
end;

{ TdxOTAWizardControlFormWizard }

procedure TdxOTAWizardControlFormWizard.Execute;
var
  AModuleServices: IOTAModuleServices;
begin
  if Supports(BorlandIDEServices, IOTAModuleServices, AModuleServices) then
    AModuleServices.CreateModule(TdxOTAWizardControlFormCreator.Create(Personality));
end;

function TdxOTAWizardControlFormWizard.GetComment: string;
begin
  Result := ExpandWizardControlName(sdxWizardControlFormWizardComment);
end;

function TdxOTAWizardControlFormWizard.GetName: string;
begin
  Result := ExpandWizardControlName(sdxWizardControlFormWizardName);
end;

end.
