{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressGaugeControl                                      }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSGAUGECONTROL AND ALL           }
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

unit dxGaugeControlSelection;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Windows, cxControls, cxGeometry, dxGaugeCustomScale;

type
  TdxGaugeControlCustomSelectionHelper = class;
  TdxGaugeControlCustomSelectionHelperClass = class of TdxGaugeControlCustomSelectionHelper;
  TdxGaugeControlMoveSelectionEvent = procedure(const ADelta: TdxPointF) of object;

  { TdxGaugeControlCustomSelectionHelper }

  TdxGaugeControlCustomSelectionHelper = class(TPersistent)
  private
    FControl: TcxControl;
    FIsActive: Boolean;
    FSelections: TList;

    function IsCaption(AScale: TdxGaugeCustomScale; AComponent: TComponent): Boolean;
    function IsEmptySelection: Boolean;
    function IsGaugeElement(AComponent: TComponent): Boolean;
    function IsMultiSelection: Boolean;
    function IsRange(AScale: TdxGaugeCustomScale; AComponent: TComponent): Boolean;
    function IsScale(AComponent: TComponent): Boolean;
    function GetScale(AScaleIndex: Integer): TdxGaugeCustomScale;
    function GetFirstScaleIndex: Integer;
    procedure AddSelection(AComponent: TComponent);
    procedure InternalSelect(AComponent: TComponent; AIsSelectionKeyPressed: Boolean);
    procedure SelectLastScale;
    procedure SelectNextScale;
    procedure SelectPreviousScale;
  protected
    function GetIsControlSelected: Boolean; virtual;
    procedure SetSelection; virtual;
    procedure SelectComponent(AComponent: TComponent); virtual;
    procedure ShowScalesEditor; virtual;

    function GetActive(ASelection: TList): Boolean;
    function KeyDown(AKey: Integer): Boolean;
    procedure Changed;
    procedure DeleteSelections;
    procedure PopulateSelections(ASelection: TList);
    procedure Select(AComponent: TComponent; AIsShiftPressed: Boolean);

    property Control: TcxControl read FControl;
    property IsActive: Boolean read FIsActive write FIsActive;
    property IsControlSelected: Boolean read GetIsControlSelected;
    property Selections: TList read FSelections;
  public
    constructor Create(AControl: TcxControl); virtual;
    destructor Destroy; override;
  end;

  { TdxGaugeControlCustomSelectionHelpers }

  TdxGaugeControlSelectionHelpers = class(TList)
  protected
    function FindActiveHelper(out AHelper: TdxGaugeControlCustomSelectionHelper): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegistrySelectionHelper(AHelper: TdxGaugeControlCustomSelectionHelper);
    procedure UnregistrySelectionHelper(AHelper: TdxGaugeControlCustomSelectionHelper);
  end;

function dxGaugeControlSelectionHelpers: TdxGaugeControlSelectionHelpers;

implementation

uses
  Types, dxHooks, dxCore, dxGaugeControl, dxGaugeQuantitativeScale;

const
  dxCenterPositionDelta = 0.5;

type
  TdxCustomGaugeControlAccess = class(TdxCustomGaugeControl);
  TdxCustomGaugeControlControllerAccess = class(TdxCustomGaugeControlController);
  TdxGaugeCustomScaleAccess = class(TdxGaugeCustomScale);
  TdxGaugeQuantitativeScaleAccess = class(TdxGaugeQuantitativeScale);

var
  FGaugeControlSelectionHelpers: TdxGaugeControlSelectionHelpers;

function dxGaugeControlSelectionHelpers: TdxGaugeControlSelectionHelpers;
begin
  if FGaugeControlSelectionHelpers = nil then
    FGaugeControlSelectionHelpers := TdxGaugeControlSelectionHelpers.Create;
  Result := FGaugeControlSelectionHelpers;
end;

function SortSelections(Item1, Item2: Pointer): Integer;
var
  ADelta1, ADelta2: Integer;
begin
  if TObject(Item1) is TdxGaugeCustomScale then
    ADelta1 := 1
  else
    ADelta1 := 2;
  if TObject(Item2) is TdxGaugeCustomScale then
    ADelta2 := 1
  else
    ADelta2 := 2;
  Result := dxCompareValues(ADelta2, ADelta1);
end;

{ TdxGaugeControlCustomSelectionHelper }

constructor TdxGaugeControlCustomSelectionHelper.Create(AControl: TcxControl);
begin
  inherited Create;
  FControl := AControl as TdxCustomGaugeControl;
  FSelections := TList.Create;
end;

destructor TdxGaugeControlCustomSelectionHelper.Destroy;
begin
  FreeAndNil(FSelections);
  inherited Destroy;
end;

function TdxGaugeControlCustomSelectionHelper.GetIsControlSelected: Boolean;
begin
  Result := False;
end;

procedure TdxGaugeControlCustomSelectionHelper.SetSelection;
begin
//do nothing
end;

procedure TdxGaugeControlCustomSelectionHelper.SelectComponent(AComponent: TComponent);
begin
//do nothing
end;

procedure TdxGaugeControlCustomSelectionHelper.ShowScalesEditor;
begin
//do nothing
end;

function TdxGaugeControlCustomSelectionHelper.GetActive(ASelection: TList): Boolean;
begin
  Result := (ASelection.Count > 0) and IsGaugeElement(TComponent(ASelection.Last));
end;

function TdxGaugeControlCustomSelectionHelper.KeyDown(AKey: Integer): Boolean;

  function IsScalePopupMenuShowing: Boolean;
  begin
    Result := TdxCustomGaugeControlControllerAccess(TdxCustomGaugeControlAccess(Control).Controller).IsScalePopupMenuShowing;
  end;

begin
  Result := False;
  case AKey of
    VK_DELETE:
      begin
        DeleteSelections;
        Result := TdxCustomGaugeControl(Control).Scales.Count = 0;
      end;
    VK_ESCAPE:
      begin
        Result := not IsScalePopupMenuShowing and not (Control.DragAndDropState <> ddsNone);
        if Result then
        begin
          if Control.DragAndDropState = ddsInProcess then
            Control.EndDrag(False);
          SelectComponent(Control);
        end;
      end;
    VK_RIGHT:
      SelectNextScale;
    VK_LEFT:
      SelectPreviousScale
  end;
end;

procedure TdxGaugeControlCustomSelectionHelper.Changed;
begin
  TdxCustomGaugeControl(Control).InvalidateRect(Control.ClientRect, False);
end;

procedure TdxGaugeControlCustomSelectionHelper.DeleteSelections;
begin
  Selections.Sort(SortSelections);
  while Selections.Count > 0 do
    TdxGaugeCustomScale(Selections[0]).Free;
  TdxCustomGaugeControlAccess(Control).Modified;
  if TdxCustomGaugeControlAccess(Control).Scales.Count > 0 then
    SelectLastScale
  else
    SelectComponent(Control);
end;

procedure TdxGaugeControlCustomSelectionHelper.PopulateSelections(ASelection: TList);
var
  I: Integer;
begin
  for I := 0 to ASelection.Count - 1 do
    if IsGaugeElement(ASelection[I]) then
      AddSelection(ASelection[I]);
end;

procedure TdxGaugeControlCustomSelectionHelper.Select(AComponent: TComponent; AIsShiftPressed: Boolean);
begin
  if AComponent <> nil then
  begin
    InternalSelect(AComponent, AIsShiftPressed);
    SetSelection;
  end
  else
    Selections.Clear;
end;

function TdxGaugeControlCustomSelectionHelper.IsCaption(AScale: TdxGaugeCustomScale; AComponent: TComponent): Boolean;
var
  I: Integer;
begin
  Result := AComponent is TdxGaugeCustomCaption;
  if Result then
  begin
    Result := False;
    for I := 0 to TdxGaugeCustomScaleAccess(AScale).Captions.Count - 1 do
    begin
      Result := TdxGaugeCustomScaleAccess(AScale).Captions.IndexOf(TdxGaugeCustomCaption(AComponent)) <> -1;
      if Result then
        Break;
    end;
  end;
end;

function TdxGaugeControlCustomSelectionHelper.IsEmptySelection: Boolean;
begin
  Result := Selections.Count = 0;
end;

function TdxGaugeControlCustomSelectionHelper.IsGaugeElement(AComponent: TComponent): Boolean;
var
  I: Integer;
  AElement: IdxGaugeSelectableElement;
begin
  Result := Supports(AComponent, IdxGaugeSelectableElement, AElement);
  if Result then
  begin
    Result := IsScale(AComponent);
    if not Result then
    begin
      Result := False;
      for I := 0 to TdxCustomGaugeControl(Control).Scales.Count - 1 do
      begin
        Result := IsRange(TdxCustomGaugeControl(Control).Scales[I], AComponent) or
          IsCaption(TdxCustomGaugeControl(Control).Scales[I], AComponent);
        if Result then
          Break;
      end;
    end;
  end;
end;

function TdxGaugeControlCustomSelectionHelper.IsMultiSelection: Boolean;
begin
  Result := Selections.Count > 1;
end;

function TdxGaugeControlCustomSelectionHelper.IsRange(AScale: TdxGaugeCustomScale; AComponent: TComponent): Boolean;
begin
  Result := (AComponent is TdxGaugeCustomRange) and (AScale is TdxGaugeQuantitativeScale) and
    (TdxGaugeQuantitativeScaleAccess(AScale).Ranges.IndexOf(TdxGaugeCustomRange(AComponent)) <> -1);
end;

function TdxGaugeControlCustomSelectionHelper.IsScale(AComponent: TComponent): Boolean;
begin
  Result := (AComponent is TdxGaugeCustomScale) and
    (TdxCustomGaugeControl(Control).Scales.IndexOf(TdxGaugeCustomScale(AComponent)) <> -1);
end;

function TdxGaugeControlCustomSelectionHelper.GetScale(AScaleIndex: Integer): TdxGaugeCustomScale;
begin
  Result := TdxGaugeCustomScale(TdxCustomGaugeControl(Control).Scales[AScaleIndex]);
end;

function TdxGaugeControlCustomSelectionHelper.GetFirstScaleIndex: Integer;
begin
  Result := TdxGaugeCustomScale(Selections[0]).Index;
end;

procedure TdxGaugeControlCustomSelectionHelper.AddSelection(AComponent: TComponent);
begin
  Selections.Add(AComponent);
end;

procedure TdxGaugeControlCustomSelectionHelper.InternalSelect(AComponent: TComponent; AIsSelectionKeyPressed: Boolean);
begin
  if (Selections.Count = 1) and not AIsSelectionKeyPressed or
    (IsMultiSelection and (Selections.IndexOf(AComponent) = -1) and not AIsSelectionKeyPressed) then
    Selections.Clear;
  if (Selections.IndexOf(AComponent) = -1) and (IsEmptySelection or ((Selections.Count >= 1) and AIsSelectionKeyPressed)) then
    AddSelection(AComponent)
  else
    if AIsSelectionKeyPressed then
      Selections.Remove(AComponent);
end;

procedure TdxGaugeControlCustomSelectionHelper.SelectLastScale;
begin
  if TdxCustomGaugeControl(Control).Scales.Count > 0 then
    Select(GetScale(TdxGaugeControl(Control).Scales.Count - 1), False);
end;

procedure TdxGaugeControlCustomSelectionHelper.SelectNextScale;
begin
  if not (IsEmptySelection or IsMultiSelection) and
    (GetFirstScaleIndex < TdxCustomGaugeControl(Control).Scales.Count - 1) then
    Select(GetScale(GetFirstScaleIndex + 1), False);
end;

procedure TdxGaugeControlCustomSelectionHelper.SelectPreviousScale;
begin
  if not (IsEmptySelection or IsMultiSelection) and (GetFirstScaleIndex > 0) then
    Select(GetScale(GetFirstScaleIndex - 1), False);
end;

{ TdxGaugeControlCustomSelectionHelpers }

function ProcessKeyboardMessage(AKey: WPARAM; AFlags: LPARAM): Boolean;

  function KeyPressed: Boolean;
  begin
    Result := (AFlags shr 31) and 1 = 0;
  end;

var
  AHelper: TdxGaugeControlCustomSelectionHelper;
begin
  Result := False;
  if KeyPressed and dxGaugeControlSelectionHelpers.FindActiveHelper(AHelper) then
  begin
    Result := AHelper.Selections.Count > 0;
    if Result then
      Result := AHelper.KeyDown(AKey);
  end
end;

procedure dxGaugeControlKeyboardHook(ACode: Integer; wParam: WPARAM; lParam: LPARAM; var AHookResult: LRESULT);
begin
  if (ACode = HC_ACTION) then
    AHookResult := Integer(ProcessKeyboardMessage(wParam, lParam));
end;

constructor TdxGaugeControlSelectionHelpers.Create;
begin
  inherited Create;
  dxSetHook(htKeyboard, dxGaugeControlKeyboardHook);
end;

destructor TdxGaugeControlSelectionHelpers.Destroy;
begin
  dxReleaseHook(dxGaugeControlKeyboardHook);
  inherited Destroy;
end;

procedure TdxGaugeControlSelectionHelpers.RegistrySelectionHelper(AHelper: TdxGaugeControlCustomSelectionHelper);
begin
  if IndexOf(AHelper) = -1 then
    Add(AHelper);
end;

procedure TdxGaugeControlSelectionHelpers.UnregistrySelectionHelper(AHelper: TdxGaugeControlCustomSelectionHelper);
begin
  Remove(AHelper);
end;

function TdxGaugeControlSelectionHelpers.FindActiveHelper(out AHelper: TdxGaugeControlCustomSelectionHelper): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    AHelper := Items[I];
    Result := AHelper.IsActive and AHelper.Control.HandleAllocated and
      ((AHelper.Control.Handle = GetFocus) or ((AHelper.Control.Parent <> nil) and
        AHelper.Control.Parent.HandleAllocated and cxIsParentFocused(AHelper.Control.Parent.Handle)));
    if Result then
      Break;
  end;
end;

initialization

finalization
  FreeAndNil(FGaugeControlSelectionHelpers);

end.
