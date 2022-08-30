{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressTileControl                                       }
{                                                                    }
{           Copyright (c) 2011-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSTILECONTROL AND ALL            }
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

unit dxTileControlReg;

{$I cxVer.inc}

interface

uses
  Forms, DesignIntf, DesignEditors, DesignMenus, Controls,
  cxClasses, dxCoreReg, dxCoreClasses, cxDesignWindows;

const
  dxTileBarProductName  = 'ExpressTileBar Suite';
  dxTileControlProductName  = 'ExpressTileControl Suite';

procedure Register;

implementation

uses
  SysUtils, Windows, Classes, Menus, Graphics, TypInfo, Math, ImgList,
  dxCustomTileControl, dxTileControl, dxTileBar, cxLibraryReg, cxPropEditors, cxGraphics;

const
  dxTileBarEditorVerbs: array [0..2] of string = ('A&dd Regular Item', 'Add &Large Item', 'Add Group');
  dxTileControlEditorVerbs: array [0..4] of string = ('Add Small Item', 'Add Regular Item',
    'Add Large Item', 'Add Extra-Large Item', 'Add Group');

type
  { TdxTileControlItemCustomGlyphImageIndexProperty }

  TdxTileControlItemCustomGlyphImageIndexProperty = class(TImageIndexProperty)
  private
    function GetGlyph: TdxTileControlItemCustomGlyph;
  protected
    property Glyph: TdxTileControlItemCustomGlyph read GetGlyph;
  public
    function GetImages: TCustomImageList; override;
  end;

  { TdxTileControlItemDetailProperty }

  TdxTileControlItemDetailProperty = class(TComponentProperty)
  private
    FProc: TGetStrProc;

    function GetCandidate(const AName: string): TComponent;
  protected
    function CheckCandidate(ACandidate, ATileControl: TComponent): Boolean; virtual;
    procedure GetStrProc(const AName: string);
    function GetTileControl: TComponent; virtual;
    function GetTileItem: TComponent; virtual;
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxTileControlComponentEditor }

  TdxTileControlComponentEditor = class(TdxComponentEditor)
  private
    procedure CreateTileControlItem(const ASize: TdxTileControlItemSize = tcisRegular);
    function GetTileControl: TdxCustomTileControl;
  protected
    procedure Edit; override;
    function GetProductName: string; override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  public
    property TileControl: TdxCustomTileControl read GetTileControl;
  end;

  { TdxTileBarComponentEditor }

  TdxTileBarComponentEditor = class(TdxTileControlComponentEditor)
  private
    procedure CreateTileBarItem(const ASize: TdxTileBarItemSize = tbisRegular);
    function GetTileBar: TdxCustomTileBar;
  protected
    function GetProductName: string; override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  public
    property TileBar: TdxCustomTileBar read GetTileBar;
  end;

  { TdxTileBarFocusedItemProperty }

  TdxTileBarFocusedItemProperty = class(TdxTileControlItemDetailProperty)
  protected
    function CheckCandidate(ACandidate, ATileControl: TComponent): Boolean; override;
    function GetTileControl: TComponent; override;
  end;

  { TdxTileBarItemDetailProperty }

  TdxTileBarItemDetailProperty = class(TdxTileControlItemDetailProperty)
  protected
    function CheckCandidate(ACandidate, ATileControl: TComponent): Boolean; override;
    function GetTileControl: TComponent; override;
    function GetTileItem: TComponent; override;
  end;

  { TdxTileBarItemPopupControlProperty }

  TdxTileBarItemPopupControlProperty = class(TdxTileControlItemDetailProperty)
  protected
    function CheckCandidate(ACandidate, ATileControl: TComponent): Boolean; override;
    function GetTileControl: TComponent; override;
    function GetTileItem: TComponent; override;
  end;

  { TdxTileControlDesignHelper }

  TdxTileControlDesignHelper = class(TdxTileControlCustomDesignHelper, IUnknown, IcxDesignSelectionChanged)
  private
    FDesignHelper: TcxDesignHelper;
  protected
    function GetControl: TdxCustomTileControl; override;
    // IUnknown
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    // IDesignNotification
    procedure DesignSelectionChanged(AList: TList);
  public
    constructor Create(AControl: TdxCustomTileControl); override;
    destructor Destroy; override;
    procedure CreateItemClickHandler(AItem: TdxTileControlItem); override;
    function IsObjectSelected(AObject: TPersistent): Boolean; override;
    procedure Select(AObject: TPersistent; AShift: TShiftState); override;
    procedure SetSelection(AList: TList); override;
    procedure UnselectObject(AObject: TPersistent); override;
  end;

  { TdxTileBarDesignHelper }

  TdxTileBarDesignHelper = class(TdxTileControlDesignHelper)
  protected
    function GetControl: TdxCustomTileBar; reintroduce; virtual;
  public
    property Control: TdxCustomTileBar read GetControl;
  end;

{ TdxTileControlItemCustomGlyphImageIndexProperty }

TdxTileControlItemGlyphAccess = class(TdxTileControlItemCustomGlyph);

function TdxTileControlItemCustomGlyphImageIndexProperty.GetGlyph: TdxTileControlItemCustomGlyph;
begin
  Result := GetComponent(0) as TdxTileControlItemCustomGlyph;
end;

function TdxTileControlItemCustomGlyphImageIndexProperty.GetImages: TCustomImageList;
begin
  Result := TdxTileControlItemGlyphAccess(Glyph).GetImages;
end;

{ TdxTileControlItemDetailProperty }

function TdxTileControlItemDetailProperty.CheckCandidate(ACandidate, ATileControl: TComponent): Boolean;
begin
  Result := (ACandidate is TWinControl) and (ATileControl <> ACandidate) and
    ((TWinControl(ACandidate).Parent is TScrollingWinControl) or (ACandidate is TScrollingWinControl));
end;

function TdxTileControlItemDetailProperty.GetCandidate(const AName: string): TComponent;
begin
  Result := Designer.GetComponent(AName);
end;

procedure TdxTileControlItemDetailProperty.GetValues(Proc: TGetStrProc);
begin
  FProc := Proc;
  inherited GetValues(GetStrProc);
end;

procedure TdxTileControlItemDetailProperty.GetStrProc(const AName: string);
begin
  if CheckCandidate(GetCandidate(AName), GetTileControl) then
    FProc(AName);
end;

function TdxTileControlItemDetailProperty.GetTileControl: TComponent;
begin
  Result := TdxTileControlItemDetailOptions(GetComponent(0)).TileControl;
end;

function TdxTileControlItemDetailProperty.GetTileItem: TComponent;
begin
  Result := TdxTileControlItemDetailOptions(GetComponent(0)).TileItem;
end;

{ TdxTileControlComponentEditor }

function TdxTileControlComponentEditor.GetProductName: string;
begin
  Result := dxTileControlProductName;
end;

function TdxTileControlComponentEditor.GetTileControl: TdxCustomTileControl;
begin
  Result := Component as TdxCustomTileControl;
end;

procedure TdxTileControlComponentEditor.Edit;
begin
  ExecuteVerb(1);
end;

function TdxTileControlComponentEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := dxTileControlEditorVerbs[AIndex];
end;

function TdxTileControlComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := Length(dxTileControlEditorVerbs);
end;

procedure TdxTileControlComponentEditor.CreateTileControlItem(const ASize: TdxTileControlItemSize = tcisRegular);
var
  AItem: TdxTileControlItem;
begin
  AItem := TileControl.CreateItem(ASize);
  Designer.SelectComponent(AItem);
end;

procedure TdxTileControlComponentEditor.InternalExecuteVerb(AIndex: Integer);
var
  AGroup: TdxTileControlGroup;
begin
  case AIndex of
    0:
      CreateTileControlItem(tcisSmall);
    1:
      CreateTileControlItem;
    2:
      CreateTileControlItem(tcisLarge);
    3:
      CreateTileControlItem(tcisExtraLarge);
    else
    begin
      AGroup := TileControl.CreateGroup;
      Designer.SelectComponent(AGroup);
    end;
  end;
end;

{ TdxTileBarComponentEditor }

function TdxTileBarComponentEditor.GetProductName: string;
begin
  Result := dxTileBarProductName;
end;

function TdxTileBarComponentEditor.GetTileBar: TdxCustomTileBar;
begin
  Result := Component as TdxCustomTileBar;
end;

function TdxTileBarComponentEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := dxTileBarEditorVerbs[AIndex];
end;

function TdxTileBarComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := Length(dxTileBarEditorVerbs);
end;

procedure TdxTileBarComponentEditor.CreateTileBarItem(const ASize: TdxTileBarItemSize = tbisRegular);
var
  AItem: TdxTileBarItem;
begin
  AItem := TileBar.CreateItem(ASize);
  Designer.SelectComponent(AItem);
end;

procedure TdxTileBarComponentEditor.InternalExecuteVerb(AIndex: Integer);
var
  AGroup: TdxTileControlGroup;
begin
  case AIndex of
    0:
      CreateTileBarItem(tbisRegular);
    1:
      CreateTileBarItem(tbisLarge);
    else
    begin
      AGroup := TileBar.CreateGroup;
      Designer.SelectComponent(AGroup);
    end;
  end;
end;

{ TdxTileBarFocusedItemProperty }

function TdxTileBarFocusedItemProperty.CheckCandidate(ACandidate, ATileControl: TComponent): Boolean;
begin
  Result := (ACandidate is TdxTileBarItem) and (TdxTileBarItem(ACandidate).TileBar = ATileControl) and
    TdxTileBarItem(ACandidate).ActuallyVisible and TdxTileBarItem(ACandidate).Enabled;
end;

function TdxTileBarFocusedItemProperty.GetTileControl: TComponent;
begin
  Result := GetComponent(0) as TComponent;
end;

{ TdxTileBarItemDetailProperty }

type
  TdxCustomTileBarAccess = class(TdxCustomTileBar);

function TdxTileBarItemDetailProperty.CheckCandidate(ACandidate, ATileControl: TComponent): Boolean;
begin
  Result := (ACandidate is TWinControl) and (ATileControl is TdxCustomTileBar) and
    inherited CheckCandidate(ACandidate, ATileControl) and
    (TWinControl(ACandidate) <> TdxTileBarItem(GetTileItem).PopupOptions.PopupControl);
end;

function TdxTileBarItemDetailProperty.GetTileControl: TComponent;
begin
  Result := TdxTileBarItemDetailOptions(GetComponent(0)).TileControl;
end;

function TdxTileBarItemDetailProperty.GetTileItem: TComponent;
begin
  Result := TdxTileBarItemDetailOptions(GetComponent(0)).TileItem;
end;

{ TdxTileBarItemPopupControlProperty }

function TdxTileBarItemPopupControlProperty.CheckCandidate(ACandidate, ATileControl: TComponent): Boolean;
begin
  Result := (ACandidate is TWinControl) and (ATileControl is TdxCustomTileBar) and
     inherited CheckCandidate(ACandidate, ATileControl) and
    (TWinControl(ACandidate) <> TdxTileBarItem(GetTileItem).DetailOptions.DetailControl);
end;

function TdxTileBarItemPopupControlProperty.GetTileControl: TComponent;
begin
  Result := TdxTileBarItemPopupOptions(GetComponent(0)).TileBar;
end;

function TdxTileBarItemPopupControlProperty.GetTileItem: TComponent;
begin
  Result := TdxTileBarItemPopupOptions(GetComponent(0)).BarItem;
end;

{ TdxTileControlDesignHelper }

constructor TdxTileControlDesignHelper.Create(AControl: TdxCustomTileControl);
begin
  inherited Create(AControl);
  FDesignHelper := TcxDesignHelper.Create(AControl);
  FDesignHelper.AddSelectionChangedListener(Self);
end;

destructor TdxTileControlDesignHelper.Destroy;
begin
  FDesignHelper.RemoveSelectionChangedListener(Self);
  FDesignHelper.Free;
  inherited Destroy;
end;

procedure TdxTileControlDesignHelper.CreateItemClickHandler(AItem: TdxTileControlItem);
const
  MethodParams: array[0..0] of TMethodParam = (
    (Flags: [pfAddress]; Name: 'Sender'; TypeName: 'TdxTileControlItem')
  );
begin
  if AItem <> nil then
    ShowEventMethod(FDesignHelper.Designer, AItem, 'OnClick', AItem.Name + 'Click', MethodParams);
end;

function TdxTileControlDesignHelper.IsObjectSelected(AObject: TPersistent): Boolean;
begin
  Result := FDesignHelper.IsObjectSelected(AObject);
end;

procedure TdxTileControlDesignHelper.Select(AObject: TPersistent;
  AShift: TShiftState);
begin
  if AShift * [ssCtrl, ssAlt] <> [] then Exit;
  if AObject = nil then
    FDesignHelper.SelectObject(Control)
  else
    if ssShift in AShift then
      FDesignHelper.ChangeSelection(AObject)
    else
      FDesignHelper.SelectObject(AObject);
end;

procedure TdxTileControlDesignHelper.SetSelection(AList: TList);
begin
  FDesignHelper.SetSelection(AList);
end;

procedure TdxTileControlDesignHelper.UnselectObject(AObject: TPersistent);
begin
  FDesignHelper.UnselectObject(AObject);
end;

function TdxTileControlDesignHelper.GetControl: TdxCustomTileControl;
begin
  Result := FDesignHelper.Component as TdxCustomTileControl;
end;

function TdxTileControlDesignHelper._AddRef: Integer; stdcall;
begin
  Result := -1;
end;

function TdxTileControlDesignHelper._Release: Integer; stdcall;
begin
  Result := -1;
end;

function TdxTileControlDesignHelper.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := cxE_NOINTERFACE;
end;

procedure TdxTileControlDesignHelper.DesignSelectionChanged(AList: TList);
begin
  if (Control <> nil) and not Control.IsDestroying or Control.IsLoading then
    Control.Invalidate;
end;

{ TdxTileBarDesignHelper }

function TdxTileBarDesignHelper.GetControl: TdxCustomTileBar;
begin
  Result := FDesignHelper.Component as TdxCustomTileBar;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterComponents(dxCoreLibraryProductPage, [TdxTileControl]);
  RegisterComponents(dxCoreLibraryProductPage, [TdxTileBar]);
  RegisterNoIcon([TdxTileControlGroup, TdxTileControlItemFrame, TdxTileControlItem, TdxTileControlActionBarItem, TdxTileBarItem]);
  RegisterClasses([TdxTileControl, TdxTileControlGroup, TdxTileControlItemFrame, TdxTileControlItem, TdxTileControlActionBarItem,
    TdxTileControlItemGlyph, TdxTileControlItemFrameGlyph, TdxTileBarItem]);
  RegisterComponentEditor(TdxTileControl, TdxTileControlComponentEditor);
  RegisterComponentEditor(TdxTileBar, TdxTileBarComponentEditor);
  RegisterPropertyEditor(TypeInfo(TWinControl), TdxTileControlItemDetailOptions, 'DetailControl', TdxTileControlItemDetailProperty);
  RegisterPropertyEditor(TypeInfo(TWinControl), TdxTileBarItemDetailOptions, 'DetailControl', TdxTileBarItemDetailProperty);
  RegisterPropertyEditor(TypeInfo(TWinControl), TdxTileBarItemPopupOptions, 'PopupControl', TdxTileBarItemPopupControlProperty);
  RegisterPropertyEditor(TypeInfo(TdxTileBarItem), TdxTileBar, 'FocusedItem', TdxTileBarFocusedItemProperty);
  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TdxTileControlItemCustomGlyph, 'ImageIndex', TdxTileControlItemCustomGlyphImageIndexProperty);
  // RegisterPropertyEditor(TypeInfo(Integer), TdxTileControlItem, 'GroupIndex', nil);
  RegisterPropertyEditor(TypeInfo(Integer), TdxTileControlItem, 'IndexInGroup', nil);
  RegisterPropertyEditor(TypeInfo(TColor), TdxTileControlStyle, 'BorderColor', nil);
  //
  RegisterPropertyEditor(TypeInfo(Integer), TdxTileBarOptionsView, 'GroupBlockMaxColumnCount', nil);
  RegisterPropertyEditor(TypeInfo(TdxTileControlGroupLayout), TdxTileBarOptionsView, 'GroupLayout', nil);
  RegisterPropertyEditor(TypeInfo(Integer), TdxTileBarOptionsView, 'GroupMaxRowCount', nil);
  RegisterPropertyEditor(TypeInfo(string), TdxTileBarItemDetailOptions, 'Caption', nil);
  RegisterPropertyEditor(TypeInfo(Boolean), TdxTileBarItemDetailOptions, 'ShowTab', nil);
end;

initialization
  dxDesignHelperClass := TdxTileControlDesignHelper;
  dxTileBarDesignHelperClass := TdxTileBarDesignHelper;

finalization
  dxDesignHelperClass := nil;
  dxTileBarDesignHelperClass := nil;
end.
