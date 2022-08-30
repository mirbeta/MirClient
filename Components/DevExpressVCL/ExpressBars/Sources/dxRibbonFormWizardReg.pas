{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars components                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxRibbonFormWizardReg;

{$I cxVer.inc}

interface

uses
  Classes, Windows, DesignIntf, ToolsApi, DesignEditors,
  dxBar, dxRibbon, dxRibbonReg, dxCoreReg, dxRibbonForm, dxRibbonFormWizard, dxDesignHelpers, dxRibbonSkins;

const
  sdxRibbonApplicationSelectStyleText = 'Select the application style:';
  sdxRibbonApplicationWizardComment = 'Create a VCL application with an empty form featuring the Ribbon UI';
  sdxRibbonApplicationWizardName = 'DevExpress VCL %version% Ribbon Based Application';
  sdxRibbonFormSelectStyleText = 'Select the form style:';
  sdxRibbonFormWizardComment = 'Create an empty VCL form featuring the Ribbon UI';
  sdxRibbonFormWizardName = 'DevExpress VCL %version% Ribbon Based Form';

type

  { TdxOTACustomRibbonWizard }

  TdxOTACustomRibbonWizard = class(TdxOTACustomRepositoryWizard, IOTAFormWizard)
  strict private
    FPersonality: TdxOTAPersonality;
  protected
    function ExpandText(const AText: string): string;
    function GetGlyph: Cardinal; override;
    function GetIDString: string; override;
    function GetPersonality: TdxOTAPersonality; override;
  public
    constructor Create(APersonality: TdxOTAPersonality);
    //
    property Personality: TdxOTAPersonality read GetPersonality;
  end;

  { TdxOTARibbonApplicationWizard }

  TdxOTARibbonApplicationWizard = class(TdxOTACustomRibbonWizard, IOTAProjectWizard)
  protected
    procedure Execute; override;
    function GetCategory: TdxOTARepositoryCategory; override;
    function GetComment: string; override;
    function GetName: string; override;
  end;

  { TdxOTARibbonFormWizard }

  TdxOTARibbonFormWizard = class(TdxOTACustomRibbonWizard, IOTAProjectWizard)
  protected
    procedure Execute; override;
    function GetComment: string; override;
    function GetDefaultRibbonStyle: TdxRibbonStyle;
    function GetName: string; override;
  end;

  { TdxOTARibbonFormCreator }

  TdxOTARibbonFormCreator = class(TdxOTAFormCreator)
  strict private
    FPersonality: TdxOTAPersonality;
    FRibbonStyle: TdxRibbonStyle;
  protected
    function ExpandTemplate(const ATemplate, AModuleIdent, AFormIdent, AAncestorIdent: string): string; override;
    function LoadTemplate(const AName: string): string;
    procedure LoadTemplates(out AFormTemplate, AImplTemplate, AIntfTemplate: string);
  public
    constructor Create(ARibbonStyle: TdxRibbonStyle; APersonality: TdxOTAPersonality);
    //
    property Personality: TdxOTAPersonality read FPersonality;
    property RibbonStyle: TdxRibbonStyle read FRibbonStyle;
  end;

procedure Register;
implementation

uses
  StrUtils, SysUtils, dxCore;

procedure Register;
var
  APersonality: TdxOTAPersonality;
begin
  ForceDemandLoadState(dlDisable);
  for APersonality := Low(TdxOTAPersonality) to High(TdxOTAPersonality) do
  begin
  {$IF DEFINED(DELPHI14) AND NOT DEFINED(DELPHIXE)}
    if APersonality = dxopCBuilder then
      Continue;
  {$IFEND}
    dxRegisterPackageWizard(TdxOTARibbonApplicationWizard.Create(APersonality));
    dxRegisterPackageWizard(TdxOTARibbonFormWizard.Create(APersonality));
  end;
end;

{ TdxOTACustomRibbonWizard }

constructor TdxOTACustomRibbonWizard.Create(APersonality: TdxOTAPersonality);
begin
  inherited Create;
  FPersonality := APersonality;
end;

function TdxOTACustomRibbonWizard.ExpandText(const AText: string): string;
begin
  Result := StringReplace(AText, '%version%', dxGetShortBuildNumberAsString, [rfReplaceAll]);
end;

function TdxOTACustomRibbonWizard.GetGlyph: Cardinal;
begin
  Result := LoadIcon(HInstance, 'DXRIBBONFORMWIZARD');
end;

function TdxOTACustomRibbonWizard.GetIDString: string;
const
  PersonalitiesMap: array[TdxOTAPersonality] of string = ('Delphi', 'CBuilder');
begin
  Result := inherited GetIDString + '.' + PersonalitiesMap[Personality];
end;

function TdxOTACustomRibbonWizard.GetPersonality: TdxOTAPersonality;
begin
  Result := FPersonality;
end;

{ TdxOTARibbonApplicationWizard }

procedure TdxOTARibbonApplicationWizard.Execute;
var
  AModuleServices: IOTAModuleServices;
  ARibbonStyle: TdxRibbonStyle;
begin
  if Supports(BorlandIDEServices, IOTAModuleServices, AModuleServices) then
  begin
    ARibbonStyle := High(TdxRibbonStyle);
    if ExecuteSelectRibbonStyleDialog(ARibbonStyle, GetName, sdxRibbonApplicationSelectStyleText) then
    begin
      AModuleServices.CreateModule(TdxOTAApplicationCreator.Create(Personality));
      AModuleServices.CreateModule(TdxOTARibbonFormCreator.Create(ARibbonStyle, Personality));
    end;
  end;
end;

function TdxOTARibbonApplicationWizard.GetCategory: TdxOTARepositoryCategory;
begin
  Result := dxcNewProject;
end;

function TdxOTARibbonApplicationWizard.GetComment: string;
begin
  Result := ExpandText(sdxRibbonApplicationWizardComment);
end;

function TdxOTARibbonApplicationWizard.GetName: string;
begin
  Result := ExpandText(sdxRibbonApplicationWizardName);
end;

{ TdxOTARibbonFormWizard }

procedure TdxOTARibbonFormWizard.Execute;
var
  AModuleServices: IOTAModuleServices;
  ARibbonStyle: TdxRibbonStyle;
begin
  if Supports(BorlandIDEServices, IOTAModuleServices, AModuleServices) then
  begin
    ARibbonStyle := GetDefaultRibbonStyle;
    if ExecuteSelectRibbonStyleDialog(ARibbonStyle, GetName, sdxRibbonFormSelectStyleText) then
      AModuleServices.CreateModule(TdxOTARibbonFormCreator.Create(ARibbonStyle, Personality));
  end;
end;

function TdxOTARibbonFormWizard.GetComment: string;
begin
  Result := ExpandText(sdxRibbonFormWizardComment);
end;

function TdxOTARibbonFormWizard.GetDefaultRibbonStyle: TdxRibbonStyle;
var
  ARibbon: TdxCustomRibbon;
  I: Integer;
begin
  if dxBarManagerList <> nil then
    for I := 0 to dxBarManagerList.Count - 1 do
    begin
      ARibbon := FindRibbonForComponent(dxBarManagerList[I]);
      if ARibbon <> nil then
        Exit(ARibbon.Style);
    end;

  Result := High(TdxRibbonStyle);
end;

function TdxOTARibbonFormWizard.GetName: string;
begin
  Result := ExpandText(sdxRibbonFormWizardName);
end;

{ TdxOTARibbonFormCreator }

constructor TdxOTARibbonFormCreator.Create(ARibbonStyle: TdxRibbonStyle; APersonality: TdxOTAPersonality);
var
  AFormTemplate, AImplTemplate, AIntfTemplate: string;
begin
  FRibbonStyle := ARibbonStyle;
  FPersonality := APersonality;
  LoadTemplates(AFormTemplate, AImplTemplate, AIntfTemplate);
  inherited Create(AFormTemplate, AImplTemplate, AIntfTemplate);
end;

function TdxOTARibbonFormCreator.ExpandTemplate(const ATemplate, AModuleIdent, AFormIdent, AAncestorIdent: string): string;
const
  SkinNameMap: array[TdxRibbonStyle] of string = (
    'Office2007Blue',
    'Office2010Blue',
    'Office2013White',
    'Office2013LightGray',
    'Office2013LightGray',
    'Office2019Colorful'
  );
begin
  Result := inherited ExpandTemplate(ATemplate, AModuleIdent, AFormIdent, AAncestorIdent);
  Result := StringReplace(Result, '%SkinName%', '''' + SkinNameMap[RibbonStyle] + '''', [rfReplaceAll]);
end;

function TdxOTARibbonFormCreator.LoadTemplate(const AName: string): string;
var
  AList: TStringList;
  AStream: TResourceStream;
begin
  AList := TStringList.Create;
  try
    AStream := TResourceStream.Create(HInstance, AName, dxRibbonFormWizardResType);
    try
      AList.LoadFromStream(AStream);
    finally
      AStream.Free;
    end;
    Result := AList.Text;
  finally
    AList.Free;
  end;
end;

procedure TdxOTARibbonFormCreator.LoadTemplates(out AFormTemplate, AImplTemplate, AIntfTemplate: string);
begin
  AFormTemplate := LoadTemplate('RIBBON' + dxRibbonStyleNamePrefixMap[RibbonStyle] + 'FORM');
  if Personality = dxopCBuilder then
  begin
    AIntfTemplate := LoadTemplate('RIBBON' + dxRibbonStyleNamePrefixMap[RibbonStyle] + 'CBUILDERHEADER');
    AImplTemplate := LoadTemplate('RIBBON' + dxRibbonStyleNamePrefixMap[RibbonStyle] + 'CBUILDERUNIT');
  end
  else
  begin
    AImplTemplate := LoadTemplate('RIBBON' + dxRibbonStyleNamePrefixMap[RibbonStyle] + 'DELPHIUNIT');
    AIntfTemplate := '';
  end;
end;

end.
