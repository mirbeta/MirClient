{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit cxCustomListBox;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, dxCore, Math, Types, Variants,
  Classes, Controls, Forms, Menus, StdCtrls, SysUtils, Graphics, ImgList,
  cxClasses, cxControls, cxContainer, cxDataUtils, cxGraphics, cxLookAndFeels, cxGeometry,
  cxScrollBar, cxLookAndFeelPainters, dxCustomHint, cxEdit;

type
  TcxCustomListBox = class(TcxCustomEditContainer)
  private
    FIsExitProcessing: Boolean;
    FInnerListBox: TcxCustomInnerListBox;
    FIntegralHeight: Boolean;
    function GetAutoComplete: Boolean;
    function GetAutoCompleteDelay: Cardinal;
    function GetCount: Integer;
    function GetItemHeight: Integer;
    function GetItemIndex: Integer;
    function GetReadOnly: Boolean;
    function GetScrollWidth: Integer;
    function GetSelected(Index: Integer): Boolean;
    function GetTabWidth: Integer;
    function GetTopIndex: Integer;
    procedure SetAutoComplete(Value: Boolean);
    procedure SetAutoCompleteDelay(Value: Cardinal);
    procedure SetCount(Value: Integer);
    procedure SetDataBinding(Value: TcxCustomDataBinding);
    procedure SetIntegralHeight(Value: Boolean);
    procedure SetItemIndex(Value: Integer);
    procedure SetReadOnly(Value: Boolean);
    procedure SetScrollWidth(Value: Integer);
    procedure SetSelected(Index: Integer; Value: Boolean);
    procedure SetTabWidth(Value: Integer);
    procedure SetTopIndex(Value: Integer);
  protected
    FDataBinding: TcxCustomDataBinding;

    function CanDisableAlignOnCreateInnerControl: Boolean; override;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure CreateInnerListBox;
    procedure DataChange; override;
    procedure DestroyInnerListBox;
    function DoCreateInnerListBox: TcxCustomInnerListBox; virtual; abstract;
    procedure DoExit; override;
    function GetDataBindingClass: TcxCustomDataBindingClass; virtual;
    function GetInnerListBoxTop: Integer; virtual;
    function GetItemText(AItemIndex: Integer): string; virtual; abstract;
    function GetListItemHeight(AIndex: Integer): Integer; virtual;
    function GetListStyle: TListBoxStyle; virtual;
    procedure GetOptimalHeight(var ANewHeight: Integer); virtual;
    function IndexOf(const S: string): Integer; virtual; abstract;
    procedure InitializeInnerListBox; virtual; abstract;
    procedure InternalKeyDown(var Key: Word; Shift: TShiftState); virtual;
    function IsReadOnly: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function NeedIgnorePressedKey(var Key: Char): Boolean; virtual;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure SetItemHeight(Value: Integer); virtual;
    procedure SetListStyle(Value: TListBoxStyle); virtual;
    procedure SynchronizeTextFlags; virtual;
    procedure UpdateData; override;
    procedure WndProc(var Message: TMessage); override;

    property AutoComplete: Boolean read GetAutoComplete write SetAutoComplete default True;
    property AutoCompleteDelay: Cardinal read GetAutoCompleteDelay write SetAutoCompleteDelay default cxDefaultAutoCompleteDelay;
    property Count: Integer read GetCount write SetCount;
    property DataBinding: TcxCustomDataBinding read FDataBinding write SetDataBinding;
    property InnerListBox: TcxCustomInnerListBox read FInnerListBox;
    property IntegralHeight: Boolean read FIntegralHeight write SetIntegralHeight default False;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property ListStyle: TListBoxStyle read GetListStyle write SetListStyle default lbStandard;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ScrollWidth: Integer read GetScrollWidth write SetScrollWidth default 0;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
    property TabWidth: Integer read GetTabWidth write SetTabWidth default 0;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  end;

implementation

type
  TcxCustomInnerListBoxAccess = class(TcxCustomInnerListBox);

{ TcxCustomListBox }

constructor TcxCustomListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataBinding := GetDataBindingClass.Create(Self, Self);
  FDataBinding.OnDataChange := DataChange;
  FDataBinding.OnDataSetChange := DataSetChange;
  FDataBinding.OnUpdateData := UpdateData;
end;

destructor TcxCustomListBox.Destroy;
begin
  FreeAndNil(FDataBinding);
  inherited Destroy;
end;

function TcxCustomListBox.CanDisableAlignOnCreateInnerControl: Boolean;
begin
  Result := False;
end;

function TcxCustomListBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or
    FDataBinding.ExecuteAction(Action);
end;

function TcxCustomListBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or
    FDataBinding.UpdateAction(Action);
end;

function TcxCustomListBox.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := inherited CanResize(NewWidth, NewHeight);
  if not Result or not IntegralHeight or IsLoading then
    Exit;
  if Align in [alLeft, alRight, alClient] then
    Exit;
  GetOptimalHeight(NewHeight);
end;

procedure TcxCustomListBox.CreateInnerListBox;
begin
  FInnerListBox := DoCreateInnerListBox;
  InitializeInnerListBox;
end;

procedure TcxCustomListBox.DataChange;
begin
  if DataBinding.IsDataSourceLive then
    ItemIndex := IndexOf(VarToStr(DataBinding.GetStoredValue(evsText, Focused)))
  else
    ItemIndex := -1;
end;

procedure TcxCustomListBox.DestroyInnerListBox;
begin
  FreeAndNil(FInnerListBox);
end;

procedure TcxCustomListBox.DoExit;
begin
  if IsDestroying or FIsExitProcessing then
    Exit;
  FIsExitProcessing := True;
  try
    try
      DataBinding.UpdateDataSource;
    except
      SetFocus;
      raise;
    end;
    inherited DoExit;
  finally
    FIsExitProcessing := False;
  end;
end;

function TcxCustomListBox.GetAutoComplete: Boolean;
begin
  Result := FInnerListBox.AutoComplete;
end;

function TcxCustomListBox.GetAutoCompleteDelay: Cardinal;
begin
  Result := InnerListBox.AutoCompleteDelay;
end;

function TcxCustomListBox.GetCount: Integer;
begin
  Result := FInnerListBox.Items.Count;
end;

function TcxCustomListBox.GetDataBindingClass: TcxCustomDataBindingClass;
begin
  Result := TcxCustomDataBinding;
end;

function TcxCustomListBox.GetInnerListBoxTop: Integer;
begin
  Result := 0;
end;

function TcxCustomListBox.GetListItemHeight(AIndex: Integer): Integer;
begin
  case ListStyle of
    lbStandard, lbVirtual:
      Result := Canvas.FontHeight(Font);
    lbOwnerDrawFixed, lbVirtualOwnerDraw:
      Result := ItemHeight;
    lbOwnerDrawVariable:
      begin
        Result := ItemHeight;
        if (AIndex < Count) and Assigned(FInnerListBox.OnMeasureItem) then
          FInnerListBox.OnMeasureItem(Self, AIndex, Result);
      end;
  end;
end;

function TcxCustomListBox.GetItemHeight: Integer;
begin
  Result := FInnerListBox.ItemHeight;
end;

function TcxCustomListBox.GetItemIndex: Integer;
begin
  Result := InnerListBox.ItemIndex;
end;

function TcxCustomListBox.GetListStyle: TListBoxStyle;
begin
  Result := FInnerListBox.Style;
end;

procedure TcxCustomListBox.GetOptimalHeight(var ANewHeight: Integer);
var
  I: Integer;
  ABorderExtent: TRect;
  AItemHeight: Integer;
  AListClientSize, AListSize, AScrollBarSize: TSize;
  AScrollWidth: Integer;
  AVScrollBar: Boolean;
begin
  ABorderExtent := GetBorderExtent;
  AListClientSize.cy := ABorderExtent.Top + ABorderExtent.Bottom;
  AScrollBarSize := cxSize(GetVScrollBarDefaultAreaWidth,
    GetHScrollBarDefaultAreaHeight);
  AScrollWidth := ScrollWidth;
  if AScrollWidth > 0 then
    Inc(AScrollWidth, 4);
  I := GetInnerListBoxTop;
  repeat
    AItemHeight := GetListItemHeight(I);
    AListClientSize.cy := AListClientSize.cy + AItemHeight;
    AListSize.cy := AListClientSize.cy;
    AListClientSize.cx := Width - (ABorderExtent.Left + ABorderExtent.Right);
    AVScrollBar := I + 1 < Count;
    if AVScrollBar then
      AListClientSize.cx := AListClientSize.cx - AScrollBarSize.cx;
    if AListClientSize.cx < AScrollWidth then
      AListSize.cy := AListSize.cy + AScrollBarSize.cy;
    if AListSize.cy = ANewHeight then
      Break;
    if AListSize.cy > ANewHeight then
    begin
      if I > 0 then
      begin
        AListClientSize.cy := AListClientSize.cy - AItemHeight;
        AListSize.cy := AListClientSize.cy;
        AListClientSize.cx := Width - (ABorderExtent.Left + ABorderExtent.Right);
        AVScrollBar := I < Count;
        if AVScrollBar then
          AListClientSize.cx := AListClientSize.cx - AScrollBarSize.cx;
        if AListClientSize.cx < AScrollWidth then
          AListSize.cy := AListSize.cy + AScrollBarSize.cy;
      end;
      Break;
    end;
    Inc(I);
  until False;
  ANewHeight := AListSize.cy;
end;

function TcxCustomListBox.GetReadOnly: Boolean;
begin
  Result := DataBinding.ReadOnly;
end;

function TcxCustomListBox.GetScrollWidth: Integer;
begin
  Result := FInnerListBox.ScrollWidth;
end;

function TcxCustomListBox.GetSelected(Index: Integer): Boolean;
begin
  Result := InnerListBox.Selected[Index];
end;

function TcxCustomListBox.GetTabWidth: Integer;
begin
  Result := InnerListBox.TabWidth;
end;

function TcxCustomListBox.GetTopIndex: Integer;
begin
  Result := InnerListBox.TopIndex;
end;

procedure TcxCustomListBox.InternalKeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_PRIOR, VK_NEXT, VK_END, VK_HOME, VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN:
      if not DataBinding.SetEditMode then
        Key := 0;
  end;
end;

function TcxCustomListBox.IsReadOnly: Boolean;
begin
  Result := DataBinding.IsControlReadOnly;
end;

procedure TcxCustomListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  InternalKeyDown(Key, Shift);
end;

procedure TcxCustomListBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if Key = Char(VK_ESCAPE) then
    DataBinding.Reset
  else
    if NeedIgnorePressedKey(Key) then
      Key := #0;
end;

function TcxCustomListBox.NeedIgnorePressedKey(var Key: Char): Boolean;
begin
  Result := IsTextChar(Key) and not DataBinding.SetEditMode;
end;

procedure TcxCustomListBox.Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  inherited Scroll(AScrollBarKind, AScrollCode, AScrollPos);
  if Enabled then
    TcxCustomInnerListBoxAccess(FInnerListBox).Scroll(AScrollBarKind, AScrollCode, AScrollPos);
end;

procedure TcxCustomListBox.SetAutoComplete(Value: Boolean);
begin
  FInnerListBox.AutoComplete := Value;
end;

procedure TcxCustomListBox.SetAutoCompleteDelay(Value: Cardinal);
begin
  InnerListBox.AutoCompleteDelay := Value;
end;

procedure TcxCustomListBox.SetCount(Value: Integer);
begin
  FInnerListBox.Count := Value;
end;

procedure TcxCustomListBox.SetDataBinding(Value: TcxCustomDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxCustomListBox.SetIntegralHeight(Value: Boolean);
begin
  if Value <> FIntegralHeight then
  begin
    FIntegralHeight := Value;
    SetSize;
  end;
end;

procedure TcxCustomListBox.SetItemHeight(Value: Integer);
begin
  FInnerListBox.ItemHeight := Value;
end;

procedure TcxCustomListBox.SetItemIndex(Value: Integer);
begin
  InnerListBox.ItemIndex := Value;
end;

procedure TcxCustomListBox.SetListStyle(Value: TListBoxStyle);
begin
  FInnerListBox.Style := Value;
end;

procedure TcxCustomListBox.SetReadOnly(Value: Boolean);
begin
  DataBinding.ReadOnly := Value;
end;

procedure TcxCustomListBox.SetScrollWidth(Value: Integer);
begin
  FInnerListBox.ScrollWidth := Value;
end;

procedure TcxCustomListBox.SetSelected(Index: Integer; Value: Boolean);
begin
  InnerListBox.Selected[Index] := Value;
end;

procedure TcxCustomListBox.SetTabWidth(Value: Integer);
begin
  InnerListBox.TabWidth := Value;
end;

procedure TcxCustomListBox.SetTopIndex(Value: Integer);
begin
  InnerListBox.TopIndex := Value;
end;

procedure TcxCustomListBox.SynchronizeTextFlags;
begin
  InnerListBox.Canvas.TextFlags := Canvas.TextFlags;
end;

procedure TcxCustomListBox.UpdateData;
begin
  if ItemIndex >= 0 then
    DataBinding.SetStoredValue(evsText, GetItemText(ItemIndex))
  else
    DataBinding.SetStoredValue(evsText, '');
end;

procedure TcxCustomListBox.WndProc(var Message: TMessage);
begin
  if InnerListBox <> nil then
    case Message.Msg of
      LB_ADDSTRING..LB_MSGMAX:
        begin
          Message.Result := SendMessage(InnerListBox.Handle, Message.Msg, Message.WParam, Message.LParam);
          Exit;
        end;
    end;
  inherited WndProc(Message);
end;

end.
