{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxUIGeneratorDesignHelpers;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHIXE3}
  Actions,
{$ENDIF}
  SysUtils, Classes, Controls, ActnList, dxCoreReg, dxUIGenerator, DesignIntf, Generics.Defaults, Generics.Collections;

type

  { TdxUIGeneratorComponentEditor }

  TdxUIGeneratorComponentEditor = class(TdxComponentEditor)
  strict private
    FHasGenerator: Boolean;

    procedure Generate;
  public
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure RegisterActionsForComponent(AComponentClass: TComponentClass);
implementation

uses
  dxUIGeneratorWizard;

const
  sdxGenerate = 'Generate Ribbon/Toolbar UI...';

function ToArray(AActions: TList<TBasicActionClass>): TArray<TBasicActionClass>;
{$IFDEF DELPHIXE}
begin
  Result := AActions.ToArray;
end;
{$ELSE}
var
  I: Integer;
begin
  SetLength(Result, AActions.Count);
  for I := 0 to AActions.Count - 1 do
    Result[I] := AActions[I];
end;
{$ENDIF}

procedure RegisterActionsForComponent(AComponentClass: TComponentClass);
var
  AActions: TList<TBasicActionClass>;
  ACategory: TdxUIGeneratorCategoryInfo;
  AInfo: TdxUIGeneratorComponentInfo;
  I: Integer;
begin
  if TdxUIGenerator.GetComponentInfo(AComponentClass, AInfo) then
  begin
    AActions := TList<TBasicActionClass>.Create;
    try
      for I := 0 to AInfo.Categories.Count - 1 do
      begin
        AActions.Clear;
        ACategory := AInfo.Categories[I];
        ACategory.Enum(
          procedure (AInfo: TdxUIGeneratorCommandInfo)
          begin
            if AInfo.Action <> nil then
              AActions.Add(AInfo.Action);
          end);

        if AActions.Count > 0 then
          RegisterActions(TdxUIGeneratorHelper.GenerateCategoryName(ACategory), ToArray(AActions), nil);
      end;
    finally
      AActions.Free;
    end;
  end;
end;

{ TdxUIGeneratorComponentEditor }

constructor TdxUIGeneratorComponentEditor.Create(AComponent: TComponent; ADesigner: IDesigner);
var
  AInfo: TdxUIGeneratorComponentInfo;
begin
  inherited Create(AComponent, ADesigner);
  FHasGenerator := TdxUIGenerator.GetComponentInfo(Component, AInfo) and (TdxUIGenerator.AdapterCount > 0);
end;

procedure TdxUIGeneratorComponentEditor.ExecuteVerb(Index: Integer);
begin
  if (Index < InternalGetVerbCount) or not FHasGenerator then
    inherited ExecuteVerb(Index)
  else
    case Index - InternalGetVerbCount of
      0: { do nothing };
      1: Generate;
    else
      inherited ExecuteVerb(Index - 2);
    end;
end;

function TdxUIGeneratorComponentEditor.GetVerb(Index: Integer): string;
begin
  if (Index < InternalGetVerbCount) or not FHasGenerator then
    Result := inherited GetVerb(Index)
  else
    case Index - InternalGetVerbCount of
      0: Result := '-';
      1: Result := sdxGenerate;
    else
      Result := inherited GetVerb(Index - 2);
    end;
end;

function TdxUIGeneratorComponentEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount;
  if FHasGenerator then
    Inc(Result, 2);
end;

procedure TdxUIGeneratorComponentEditor.Generate;
begin
  TfrmUIGenerator.Execute(Component);
end;

end.
