{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSkins Library                                     }
{                                                                    }
{           Copyright (c) 2006-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSKINS AND ALL ACCOMPANYING     }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit dxSkinsDesignHelperReg;

{$I cxVer.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, cxControls, cxLookAndFeels, Dialogs,
  Types, cxLookAndFeelPainters, dxSkinsLookAndFeelPainter, DesignIntf, Menus,
  DesignEditors, dxSkinsStrs, dxSkinsForm, ToolsApi, cxClasses;

type
  { TdxSkinsBaseSelectionEditor }

  TdxSkinsBaseSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresProductsUnits(Proc: TGetStrProc); virtual;
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TdxSkinsAdditionalUnits }

  TdxSkinsAdditionalUnits = class(TStringList)
  private
    FControlClass: TClass;
  public
    constructor Create(AControlClass: TClass); virtual;
    procedure RemoveUnit(const AName: string);
    //
    property ControlClass: TClass read FControlClass;
  end;

  { TdxSkinsAdditionalUnitsList }

  TdxSkinsAdditionalUnitsList = class(TObject)
  private
    FItems: TcxObjectList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TdxSkinsAdditionalUnits;
  protected
    function Find(AControlClass: TClass; out AItem: TdxSkinsAdditionalUnits): Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(AControlClass: TClass; const ASkinUnitName: string);
    procedure Remove(AControlClass: TClass; const ASkinUnitName: string);
    procedure RequiresUnits(AControlClass: TClass; AProc: TGetStrProc);
    //
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxSkinsAdditionalUnits read GetItem; default;
  end;

function dxSkinsAdditionalUnits: TdxSkinsAdditionalUnitsList;
procedure Register;
implementation

uses
  dxSkinsDesignHelper, dxSkinsReg, cxLibraryReg, cxScrollBar, cxButtons, cxRadioGroup;

var
  FAdditionalSkinsUnits: TdxSkinsAdditionalUnitsList;

function dxSkinsAdditionalUnits: TdxSkinsAdditionalUnitsList;
begin
  if FAdditionalSkinsUnits = nil then
    FAdditionalSkinsUnits := TdxSkinsAdditionalUnitsList.Create;
  Result := FAdditionalSkinsUnits;
end;

procedure RequiresSkinsUnits(Proc: TGetStrProc);
var
  AItem: TdxSkinsUnitStateListItem;
  I: Integer;
begin
  Proc('dxSkinsCore');
  for I := 0 to dxSkinsProjectSettings.UnitStateList.Count - 1 do
  begin
    AItem := dxSkinsProjectSettings.UnitStateList.Item[I];
    if AItem.Enabled then
      Proc(AItem.SkinUnitName);
  end;
end;

procedure dxSkinsRequiresAdditionalUnits(AControlClass: TClass; AProc: TGetStrProc);
begin
  dxSkinsProjectSettings.UpdateActiveProjectSettings;
  if dxSkinsProjectSettings.Enabled then
  begin
    RequiresSkinsUnits(AProc);
    dxSkinsAdditionalUnits.RequiresUnits(AControlClass, AProc);
  end;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterSelectionEditor(TcxControl, TdxSkinsBaseSelectionEditor);
  RegisterSelectionEditor(TcxScrollBar, TdxSkinsBaseSelectionEditor);
  RegisterSelectionEditor(TcxLookAndFeelController, TdxSkinsBaseSelectionEditor);
  RegisterSelectionEditor(TcxCustomButton, TdxSkinsBaseSelectionEditor);
  RegisterSelectionEditor(TcxRadioButton, TdxSkinsBaseSelectionEditor);
end;

{ TdxSkinsBaseSelectionEditor }

procedure TdxSkinsBaseSelectionEditor.RequiresProductsUnits(Proc: TGetStrProc);
begin
end;

procedure TdxSkinsBaseSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);
  dxSkinsProjectSettings.UpdateActiveProjectSettings;
  if dxSkinsProjectSettings.Enabled then
  begin
    RequiresSkinsUnits(Proc);
    RequiresProductsUnits(Proc);
  end;
end;

{ TdxSkinsAdditionalUnits }

constructor TdxSkinsAdditionalUnits.Create(AControlClass: TClass);
begin
  inherited Create;
  FControlClass := AControlClass
end;

procedure TdxSkinsAdditionalUnits.RemoveUnit(const AName: string);
var
  AIndex: Integer;
begin
  AIndex := IndexOf(AName);
  if AIndex >= 0 then
    Delete(AIndex);
end;

{ TdxSkinsAdditionalUnitsList }

constructor TdxSkinsAdditionalUnitsList.Create;
begin
  inherited Create;
  FItems := TcxObjectList.Create;
end;

destructor TdxSkinsAdditionalUnitsList.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TdxSkinsAdditionalUnitsList.Add(AControlClass: TClass; const ASkinUnitName: string);
var
  AItem: TdxSkinsAdditionalUnits;
begin
  if not Find(AControlClass, AItem) then
  begin
    AItem := TdxSkinsAdditionalUnits.Create(AControlClass);
    FItems.Add(AItem);
  end;
  AItem.Add(ASkinUnitName);
end;

function TdxSkinsAdditionalUnitsList.Find(
  AControlClass: TClass; out AItem: TdxSkinsAdditionalUnits): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].ControlClass = AControlClass;
    if Result then
    begin
      AItem := Items[I];
      Break;
    end;
  end;
end;

procedure TdxSkinsAdditionalUnitsList.Remove(AControlClass: TClass; const ASkinUnitName: string);
var
  AItem: TdxSkinsAdditionalUnits;
begin
  if Find(AControlClass, AItem) then
  begin
    AItem.RemoveUnit(ASkinUnitName);
    if AItem.Count = 0 then
      FItems.FreeAndRemove(AItem);
  end;
end;

procedure TdxSkinsAdditionalUnitsList.RequiresUnits(AControlClass: TClass; AProc: TGetStrProc);

  procedure PopulateUnits(AProc: TGetStrProc; AItem: TdxSkinsAdditionalUnits);
  var
    I: Integer;
  begin
    for I := 0 to AItem.Count - 1 do
      AProc(AItem[I]);
  end;

var
  I: Integer;
begin
  if AControlClass <> nil then
    for I := 0 to Count - 1 do
    begin
      if AControlClass.InheritsFrom(Items[I].ControlClass) then
        PopulateUnits(AProc, Items[I]);
    end;
end;

function TdxSkinsAdditionalUnitsList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxSkinsAdditionalUnitsList.GetItem(Index: Integer): TdxSkinsAdditionalUnits;
begin
  Result := TdxSkinsAdditionalUnits(FItems[Index]);
end;

initialization
  FdxSkinsListFilterProc := dxSkinsListFilter;
  FdxSkinModifyProjectOptionsProc := dxSkinsShowProjectOptionsDialog;
  FdxSkinsRequiresAdditionalUnits := dxSkinsRequiresAdditionalUnits;

finalization
  FreeAndNil(FAdditionalSkinsUnits);
  FdxSkinsRequiresAdditionalUnits := nil;
  FdxSkinModifyProjectOptionsProc := nil;
  FdxSkinsListFilterProc := nil;
end.
