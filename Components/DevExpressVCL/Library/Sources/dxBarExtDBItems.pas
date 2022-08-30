{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars extended DB items                            }
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

unit dxBarExtDBItems;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI17}
  System.Generics.Defaults, Generics.Collections,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Classes, Graphics, Controls, Db, StdCtrls, Forms,
  dxCore, dxCommon, dxBar, cxGeometry, cxGraphics;

type
  TdxBarLookupCombo = class;
  TdxBarPopupLookupControl = class;

{$IFDEF DELPHI17}
  TdxBarDBFieldList = TList<TField>;
{$ELSE}
  TdxBarDBFieldList = TList;
{$ENDIF}

  TdxBarLookupLink = class(TDataLink)
  private
    FBarLookupCombo: TdxBarLookupCombo;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure LayoutChanged; override;
  end;

  TdxBarLookupCombo = class(TCustomdxBarCombo)
  private
    FAllowResizing: Boolean;
    FCurKeyValue: Variant;
    FListLink: TdxBarLookupLink;
    FPopupList: TdxBarPopupLookupControl;
    FKeyFieldName: string;
    FListFieldName: string;
    FListFieldIndex: Integer;
    FKeyField: TField;
    FListField: TField;
    FListFields: TdxBarDBFieldList;
    FKeyValue: Variant;
    FSetValue: Boolean;
    FListActive: Boolean;
    FColor: TColor;
    FImmediateDropDown : Boolean;
    FPopupWidth: Integer;

    FRowCount: Integer;

    FListVisible: Boolean;
    FFindSelection: Boolean;
    FFindStr: string;
    FInFindSelection: Boolean;

    FLocateEdit: TEdit;
    FLocateList: TdxBarPopupLookupControl;

    FOnKeyValueChange: TNotifyEvent;

    FForm: TForm;
    ButtonOk, ButtonCancel: TButton;

    function GetListSource: TDataSource;
    procedure SetKeyFieldName(const Value: string);
    procedure SetKeyValue(const Value: Variant);
    procedure SetListFieldIndex(Value: Integer);
    procedure SetListFieldName(const Value: string);
    procedure SetListSource(Value: TDataSource);
    procedure SetRowCount(Value: Integer);

    function GetEditHandle : Integer;
    function GetEditText : String;
    procedure SetEditText(AText : String);
    procedure DoKeyPress(Sender: TObject; var Key: Char);
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure FormSize(Sender: TObject);
  protected
    procedure CloseUp; override;
    procedure DoEnter; override;
    procedure DropDown(X, Y: Integer); override;
    function GetDropDownWindow: HWND; override;
    procedure CheckDropDownPoint(var X, Y: Integer); override;
    function CheckKeyForDropDownWindow(Key: Word; Shift: TShiftState): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

    procedure UpdateListFields;
    procedure ListLinkDataChanged;
    procedure KeyValueChanged;
    function LocateKey: Boolean;
    procedure ResetFindStr;

    property EditText: string read GetEditText write SetEditText;
    property ListFields: TdxBarDBFieldList read FListFields;
    property ListLink: TdxBarLookupLink read FListLink;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoClick; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    property KeyValue: Variant read FKeyValue write SetKeyValue;
  published
    property AllowResizing: Boolean read FAllowResizing write FAllowResizing default True;
    property Color: TColor read FColor write FColor default clWindow;
    property ImmediateDropDown: Boolean read FImmediateDropDown write FImmediateDropDown default False;
    property KeyField: string read FKeyFieldName write SetKeyFieldName;
    property ListField: string read FListFieldName write SetListFieldName;
    property ListFieldIndex: Integer read FListFieldIndex write SetListFieldIndex default 0;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property RowCount: Integer read FRowCount write SetRowCount;
    property Text stored False;
    property PopupWidth: Integer read FPopupWidth write FPopupWidth default 0;

    property OnKeyValueChange: TNotifyEvent read FOnKeyValueChange write FOnKeyValueChange;
  end;

  TdxBarLookupComboControl = class(TCustomdxBarComboControl)
  protected
    procedure SetFocused(Value: Boolean); override;
    procedure WndProc(var Message: TMessage); override;
  end;

  TdxBarPopupLookupLink = class(TDataLink)
  private
    FBarPopupLookup: TdxBarPopupLookupControl;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure LayoutChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
  end;

  TdxBarPopupLookupControl = class(TCustomControl)
  private
    FListLink: TdxBarPopupLookupLink;
    FListFieldName: string;
    FListFieldIndex: Integer;
    FListField: TField;
    FListFields: TdxBarDBFieldList;
    FListActive : Boolean;

    FRecordIndex: Integer;
    FRecordCount: Integer;
    FRowCount: Integer;
    FTracking: Boolean;
    FTimerActive: Boolean;
    FMousePos: Integer;
    FSelectedItem: string;

    FHScrollWidth : Integer;
    FVScrollWidth : Integer;
    FCloseBtnDown : Boolean;
    FCloseBtnPaint : Boolean;
    FComboTop : Integer;

    FCombo: TdxBarLookupCombo;
    FCorner: TdxCorner;
    FCloseButtonRect, FGripRect: TRect;
    FCloseButtonIsTracking: Boolean;
    FMouseAboveCloseButton: Boolean;

    function GetListSource: TDataSource;
    function GetPainter: TdxBarPainter;
    procedure SetListFieldName(const Value: string);
    procedure SetListSource(Value: TDataSource);

    procedure SelectCurrent;
    procedure SelectItemAt(X, Y: Integer);
    procedure SetRowCount(Value: Integer);
    procedure StopTimer;
    procedure StopTracking;
    procedure TimerScroll;
    procedure UpdateScrollBar;

    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest(var Message: TWMNCHITTEST); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMTimer(var Message: TMessage); message WM_TIMER;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMWindowPosChanging(var Message : TWMWINDOWPOSCHANGING); message WM_WINDOWPOSCHANGING;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DblClick; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ListLinkDataChanged;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;

    function GetTextHeight: Integer;
    procedure UpdateListFields;
    procedure UpdateSizeGripCorner(ADropDownPosition: TPoint);

    property ListField: string read FListFieldName write SetListFieldName;
    property ListFieldIndex: Integer read FListFieldIndex write FListFieldIndex default 0;
    property ListFields: TdxBarDBFieldList read FListFields;
    property ListLink: TdxBarPopupLookupLink read FListLink;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property Painter: TdxBarPainter read GetPainter;
  public
    IsPopup: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ComboTop: Integer read FComboTop write FComboTop;
    property RowCount: Integer read FRowCount write SetRowCount stored False;
    property SelectedItem: string read FSelectedItem;
  end;

implementation

{$R dxBarExtDBItems.res}

uses
  Variants, cxClasses, dxBarStrs, cxControls;

type
  TdxBarManagerAccess = class(TdxBarManager);

function VarEquals(const V1, V2: Variant): Boolean;
begin
  Result := False;
  try
    Result := V1 = V2;
  except
  end;
end;

{ TdxBarLookupLink }

procedure TdxBarLookupLink.ActiveChanged;
begin
  if FBarLookupCombo <> nil then FBarLookupCombo.UpdateListFields;
end;

procedure TdxBarLookupLink.DataSetChanged;
begin
  if FBarLookupCombo <> nil then FBarLookupCombo.ListLinkDataChanged;
end;

procedure TdxBarLookupLink.LayoutChanged;
begin
  if FBarLookupCombo <> nil then FBarLookupCombo.UpdateListFields;
end;

{ TdxBarLookupCombo }

constructor TdxBarLookupCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Glyph.LoadFromResource(HInstance, 'DXBARLOOKUPCOMBO', RT_BITMAP);
  FAllowResizing := True;
  FListLink := TdxBarLookupLink.Create;
  FListLink.FBarLookupCombo := Self;
  FListFields := TdxBarDBFieldList.Create;
  FKeyValue := Null;
  FRowCount := 7;
  FPopupList := TdxBarPopupLookupControl.Create(nil);
  FColor := clWindow;
  with FPopupList do
  begin
    FCombo := Self;
  end;
end;

destructor TdxBarLookupCombo.Destroy;
begin
  FPopupList.Free;
  FListFields.Free;
  FListLink.FBarLookupCombo := nil;
  FListLink.Free;

  inherited Destroy;
end;

procedure TdxBarLookupCombo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if (FListLink <> nil) and (AComponent = ListSource) then ListSource := nil;
end;

procedure TdxBarLookupCombo.CloseUp;
begin
  if GetCapture = FPopupList.Handle then ReleaseCapture;

  RowCount := FPopupList.RowCount;
  FPopupWidth := FPopupList.Width;
  FListVisible := False;
  ResetFindStr;
  if FKeyField = nil then
    FCurKeyValue := Null
  else
    FCurKeyValue := FKeyField.Value;

  inherited;

  FPopupList.ListSource := nil;
  FPopupList.Parent := nil;
end;

procedure TdxBarLookupCombo.DoEnter;
begin
  ResetFindStr;
  inherited;
end;

procedure TdxBarLookupCombo.DropDown(X, Y: Integer);
var
  AControlWidth: Integer;
  R: TRect;
  W: Integer;
begin
  FSetValue := False;

  with FPopupList do
  begin
    IsPopup := True;
    Parent := CurItemLink.Control.Parent;
    R := cxGetWindowRect(TdxBarLookupComboControl(CurItemLink.Control).Handle);
    ComboTop := R.Top + (R.Bottom - R.Top) div 2;
    InternalInitDropDownWindow(FPopupList);
    if Self.Color <> clWindow then
      Color := Self.Color;

    if Self.ListField <> '' then
      ListField := Self.ListField
    else
      ListField := Self.KeyField;
    ListFieldIndex := Self.ListFieldIndex;
    RowCount := Self.RowCount;
    ListSource := Self.ListSource;
    if FListLink.Active then
      FRecordCount := FListLink.RecordCount;
    if not FInFindSelection and not VarIsNull(FCurKeyValue) and FListLink.Active then
      FListLink.DataSet.Locate(FKeyFieldName, FCurKeyValue, []);
  end;

  with CurItemLink.ItemRect do
    W := Right - Left - TdxBarLookupComboControl(CurItemLink.Control).GetCaptionAreaWidth;
  if W > FPopupWidth then
    AControlWidth := W
  else
    AControlWidth := FPopupWidth;

  SetWindowPos(FPopupList.Handle, 0, 0, 0, AControlWidth, FPopupList.Height,
    SWP_NOZORDER or SWP_NOMOVE or SWP_NOACTIVATE);
  FListVisible := True;

  inherited DropDown(X, Y);
end;

procedure TdxBarLookupCombo.DoClick;
var
  W, H, D, I, J: Integer;
begin
  inherited DoClick;
  if Assigned(OnClick) or ReadOnly then
    Exit;

  FForm := TForm.Create(nil);
  with FForm do
  begin
    if FAllowResizing then
      BorderIcons := []
    else
      BorderStyle := bsDialog;
    Caption := cxGetResourceString(@dxSBAR_LOOKUPDIALOGCAPTION);
    Font := BarManager.Font;
    Position := poScreenCenter;

    FLocateEdit := TEdit.Create(FForm);
    with FLocateEdit do
    begin
      Parent := FForm;
      OnKeyPress := DoKeyPress;
      OnKeyDown := DoKeyDown;
    end;
    FLocateList := TdxBarPopupLookupControl.Create(FForm);
    with FLocateList do
    begin
      FCombo := Self;
      IsPopup := False;
      Parent := FForm;
      Color := clWindow;
      if Self.ListField <> '' then
        ListField := Self.ListField
      else
        ListField := Self.KeyField;
      ListFieldIndex := Self.ListFieldIndex;
      ListSource := Self.ListSource;

      Height := 2 * 2 + Self.RowCount * GetTextHeight;
      if Self.FPopupWidth = 0 then
        Width := FLocateEdit.Width
      else
        Width := Self.FPopupWidth;
    end;
    ButtonOk := TButton.Create(FForm);
    with ButtonOk do
    begin
      Caption := cxGetResourceString(@dxSBAR_LOOKUPDIALOGOK);
      Default := True;
      ModalResult := mrOk;
      Parent := FForm;
    end;
    ButtonCancel := TButton.Create(FForm);
    with ButtonCancel do
    begin
      Caption := cxGetResourceString(@dxSBAR_LOOKUPDIALOGCANCEL);
      Cancel := True;
      ModalResult := mrCancel;
      Parent := FForm;
    end;

    H := MulDiv(FLocateEdit.Height, 43, 42);
    W := MulDiv(H, 13, 4);
    D := FLocateEdit.Height div 4;

    FLocateEdit.SetBounds(D, D, FLocateList.Width, FLocateEdit.Height);
    with FLocateList do
    begin
      Left := D;
      Top := FLocateEdit.BoundsRect.Bottom + D;
    end;
    ButtonOk.SetBounds(FLocateList.BoundsRect.Right + D, D, W, H);
    ButtonCancel.SetBounds(ButtonOk.Left, ButtonOk.BoundsRect.Bottom + D, W, H);
    I := D + FLocateList.Width + D + W + D;
    J := D + FLocateEdit.Height + D + FLocateList.Height + D;
    if J < 3 * D + 2 * H then J := 3 * D + 2 * H;
    while (ClientWidth <> I) or (ClientHeight <> J) do
    begin
      ClientWidth := I;
      ClientHeight := J;
    end;

    OnResize := FormSize;
    FLocateEdit.Text := Text;
    LocateKey;
    FListVisible := True;
    if (ShowModal = mrOk) and FListActive then
    begin
      if FKeyField <> nil then FKeyValue := FKeyField.Value;
      KeyValueChanged;
    end;
    RowCount := FLocateList.RowCount;
    FPopupWidth := FLocateList.Width;
    ResetFindStr;
    FListVisible := False;
    Free;
    FLocateEdit := nil;
  end;
end;

function TdxBarLookupCombo.GetEditHandle: Integer;
begin
  if FLocateEdit = nil then
    Result := TCustomdxBarComboControl(CurItemLink.Control).Handle
  else
    Result := FLocateEdit.Handle;
end;

function TdxBarLookupCombo.GetEditText: string;
begin
  if FLocateEdit = nil then
    Result := CurText
  else
    Result := FLocateEdit.Text;
end;

procedure TdxBarLookupCombo.SetEditText(AText: string);
begin
  if FLocateEdit = nil then
    CurText := AText
  else
    FLocateEdit.Text := AText;
end;

function TdxBarLookupCombo.GetDropDownWindow: HWND;
begin
  Result := inherited GetDropDownWindow;
  if Result = 0 then Result := FPopupList.Handle;
end;

procedure TdxBarLookupCombo.DoKeyPress(Sender: TObject; var Key: Char);
begin
  KeyPress(Key);
end;

procedure TdxBarLookupCombo.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if CheckKeyForDropDownWindow(Key, Shift) then
  begin
    FLocateList.KeyDown(Key, Shift);
    Key := 0;
  end;
end;

procedure TdxBarLookupCombo.FormSize(Sender: TObject);
var
  H, W, D: Integer;
begin
  H := MulDiv(FLocateEdit.Height, 43, 42);
  W := MulDiv(H, 13, 4);
  D := FLocateEdit.Height div 4;

  FLocateEdit.SetBounds(D, D, FForm.ClientWidth - (D + D + W + D), FLocateEdit.Height);
  with FLocateList do
  begin
    Left := D;
    Top := FLocateEdit.Top + FLocateEdit.Height + D;
    Width := FLocateEdit.Width;
    Height := FForm.ClientHeight - D - Top;
  end;
  ButtonOk.SetBounds(FForm.ClientWidth - D - W, D, W, H);
  ButtonCancel.SetBounds(ButtonOk.Left, ButtonOk.Top + ButtonOk.Height + D, W, H);
end;

procedure TdxBarLookupCombo.CheckDropDownPoint(var X, Y: Integer);
begin
  inherited CheckDropDownPoint(X, Y);
  FPopupList.UpdateSizeGripCorner(cxPoint(X, Y));
end;

function TdxBarLookupCombo.CheckKeyForDropDownWindow(Key: Word; Shift: TShiftState): Boolean;
begin
//  if {(FCombo <> nil) and }(Key = VK_RETURN) then
  if Key in [VK_RETURN, VK_TAB] then
    FSetValue := True;
  Result := Key in [VK_UP, VK_LEFT, VK_DOWN, VK_RIGHT, VK_PRIOR, VK_NEXT, VK_HOME, VK_END];
end;

procedure TdxBarLookupCombo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ResetFindStr;
  inherited KeyDown(Key, Shift);
end;

procedure TdxBarLookupCombo.KeyPress(var Key: Char);
var
  lFind, ASelectedAll: Boolean;
  AStartPos, AEndPos: Integer;
begin
  if FListField <> nil then
  try
    if Key = #8 then // BkSpace
    begin
      SendMessage(GetEditHandle, EM_GETSEL, WPARAM(@AStartPos), LPARAM(@AEndPos));
      ASelectedAll := (AEndPos - AStartPos) = Length(EditText);
      if ASelectedAll then
      begin
        ResetFindStr;
        EditText := '';
      end
      else
        if FFindSelection then
        begin
          FFindStr := Copy(FFindStr, 0, Length(FFindStr)-1);
          SendMessage(GetEditHandle, EM_SETSEL, Length(FFindStr), Length(EditText));
        end;
    end
    else
      if IsTextChar(Key) then
      begin
        FInFindSelection := True;
        try
          if FFindSelection then begin
            FFindStr := FFindStr + Key;
          end else begin
            FFindSelection := true;
            FFindStr := Key;
          end;
          lFind := False;
          try
            lFind := FListLink.DataSet.Locate(FListField.FieldName, FFindStr, [loCaseInsensitive, loPartialKey])
          except end;
          if lFind then
          begin
            EditText := FListField.DisplayText;
            SendMessage(GetEditHandle, EM_SETSEL, Length(FFindStr), Length(EditText));
//            FSetValue := True;
          end
          else
          begin
            if FFindSelection and (Length(FFindStr) > 1) then
            begin
              FFindStr := Copy(FFindStr, 1, Length(FFindStr)-1);
              if not FListVisible then DroppedDown := True;
            end
            else
            begin
              ResetFindStr;
              EditText := '';
            end;
            if not FListVisible then DroppedDown := True;
          end;
         if FImmediateDropDown and not FListVisible then
          begin
            DroppedDown := True;
          end;
        finally
          FInFindSelection := False;
        end;
      end;
  finally
    Key := #0;
    inherited KeyPress(Key);
  end;
end;

procedure TdxBarLookupCombo.UpdateListFields;
var
  DataSet: TDataSet;
begin
  FKeyField := nil;
  FListField := nil;
  FListFields.Clear;
  FListActive := False;
  if FListLink.Active {and (FKeyFieldName <> '') }then
  begin
    DataSet := FListLink.DataSet;
    FKeyField := DataSet.FindField(FKeyFieldName);
    try
      DataSet.GetFieldList(FListFields, FListFieldName);
    except
      raise;
    end;
    if (FListFields.Count = 0) and (FKeyField <> nil) then
      FListFields.Add(FKeyField);
    if FListFields.Count <> 0 then
      if (0 <= FListFieldIndex) and (FListFieldIndex < FListFields.Count) then
        FListField := FListFields[FListFieldIndex]
      else
        FListField := FListFields[0];
    FListActive := FListField <> nil;
  end;
  if FKeyField = nil then FKeyValue := Null;
end;

procedure TdxBarLookupCombo.ListLinkDataChanged;
begin
  if FListActive then
  begin
    if not VarIsNull(FKeyValue) and VarEquals(FKeyValue, FKeyField.Value) then
      Text := FListField.DisplayText;
  end;
end;

procedure TdxBarLookupCombo.KeyValueChanged;
begin
  if FListActive and not LocateKey then
    ListLink.DataSet.First;
  if (FListField <> nil) {and not VarIsNULL(FKeyValue) }then
    CurText := FListField.DisplayText
  else
    CurText := '';
  if Assigned(FOnKeyValueChange) then FOnKeyValueChange(Self);
  Text := CurText;
end;

function TdxBarLookupCombo.LocateKey: Boolean;
var
  KeySave: Variant;
begin
  if FKeyField = nil then
    Result := True
  else
  begin
    Result := False;
    try
      KeySave := FKeyValue;
      if not VarIsNull(FKeyValue) and FListLink.Active and
        FListLink.DataSet.Locate(FKeyFieldName, FKeyValue, []) then
      begin
        Result := True;
        FKeyValue := KeySave;
      end;
    except
    end;
  end;
end;

procedure TdxBarLookupCombo.ResetFindStr;
begin
  FFindStr := '';
  FFindSelection := False;
//  FSetValue := False;
end;

function TdxBarLookupCombo.GetListSource: TDataSource;
begin
  Result := FListLink.DataSource;
end;

procedure TdxBarLookupCombo.SetKeyFieldName(const Value: string);
begin
  if FKeyFieldName <> Value then
  begin
    FKeyFieldName := Value;
    UpdateListFields;
  end;
end;

procedure TdxBarLookupCombo.SetKeyValue(const Value: Variant);
begin
  if not VarEquals(FKeyValue, Value) then
  begin
    FKeyValue := Value;
    KeyValueChanged;
  end;
end;

procedure TdxBarLookupCombo.SetListFieldIndex(Value: Integer);
begin
  if Value < 0 then Exit;
  FListFieldIndex := Value;
end;

procedure TdxBarLookupCombo.SetListFieldName(const Value: string);
begin
  if FListFieldName <> Value then
  begin
    FListFieldName := Value;
    UpdateListFields;
  end;
end;

procedure TdxBarLookupCombo.SetListSource(Value: TDataSource);
begin
  FListLink.DataSource := Value;
  cxAddFreeNotification(Self, Value);
end;

procedure TdxBarLookupCombo.SetRowCount(Value: Integer);
begin
  if Value < 1 then Exit;
  FRowCount := Value;
end;

{ TdxBarLookupComboControl }

procedure TdxBarLookupComboControl.SetFocused(Value: Boolean);
var
  FCombo: TdxBarLookupCombo;
begin
  if Focused <> Value then
  begin
    inherited SetFocused(Value);
    FCombo := TdxBarLookupCombo(Item);
    if Value then
      FCombo.FCurKeyValue := FCombo.FKeyValue;
    if FCombo.FListActive and FCombo.FSetValue then
      if Value then
        FCombo.LocateKey
      else
        if (Text <> '') and (FCombo.FKeyField <> nil) then
          FCombo.KeyValue := FCombo.FKeyField.Value
        else
          FCombo.KeyValue := Null;
  end;
end;

procedure TdxBarLookupComboControl.WndProc(var Message: TMessage);
begin
  with Message do
    if (Msg = WM_KEYDOWN) and ((wParam = VK_RETURN) or (wParam = VK_TAB)) then
      with TdxBarLookupCombo(Item) do
      begin
        if FKeyField <> nil then FKeyValue := FKeyField.Value;
        KeyValueChanged;
      end;
  inherited WndProc(Message);
end;

{ TdxBarPopupLookupLink }

procedure TdxBarPopupLookupLink.ActiveChanged;
begin
  if FBarPopupLookup <> nil then FBarPopupLookup.UpdateListFields;
end;

procedure TdxBarPopupLookupLink.DataSetChanged;
begin
  if FBarPopupLookup <> nil then FBarPopupLookup.ListLinkDataChanged;
end;

procedure TdxBarPopupLookupLink.LayoutChanged;
begin
  if FBarPopupLookup <> nil then FBarPopupLookup.UpdateListFields;
end;

procedure TdxBarPopupLookupLink.DataSetScrolled(Distance: Integer);
begin
  if FBarPopupLookup <> nil then FBarPopupLookup.ListLinkDataChanged;
end;

{ TdxBarPopupLookupControl }

constructor TdxBarPopupLookupControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csCaptureMouse];

  FListLink := TdxBarPopupLookupLink.Create;
  FListLink.FBarPopupLookup := Self;
  FListFields := TdxBarDBFieldList.Create;
  FRowCount := 7;
end;

destructor TdxBarPopupLookupControl.Destroy;
begin
  FListFields.Free;
  FListLink.FBarPopupLookup := nil;
  FListLink.Free;

  inherited Destroy;
end;

procedure TdxBarPopupLookupControl.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TdxBarPopupLookupControl.WMCaptureChanged(var Message: TMessage);
begin
  inherited;
  if FCloseButtonIsTracking then
  begin
    FCloseButtonIsTracking := False;
    FMouseAboveCloseButton := False;
    SendMessage(Handle, WM_NCPAINT, 0, 0);
  end;
  StopTracking;
end;

procedure TdxBarPopupLookupControl.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TdxBarPopupLookupControl.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TdxBarPopupLookupControl.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
begin
  inherited;
  Message.MinMaxInfo^.ptMinTrackSize := Point(100, 100);
end;

procedure TdxBarPopupLookupControl.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  if FCloseButtonIsTracking then
  begin
    FCloseButtonIsTracking := False;
    ReleaseCapture;
    if FMouseAboveCloseButton then
      FCombo.BarManager.HideAll
    else
      SendMessage(Handle, WM_NCPAINT, 0, 0);
  end;
end;

procedure TdxBarPopupLookupControl.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  if IsPopup then
    Painter.SysPanelCalcSize(Message.CalcSize_Params^.rgrc[0], FGripRect, FCloseButtonRect,
      FCorner, FCombo, FCombo.AllowResizing, TdxBarManagerAccess(FCombo.BarManager).ScaleFactor);
  inherited;
end;

procedure TdxBarPopupLookupControl.WMNCHitTest(var Message : TWMNCHITTEST);
var
  PrevMouseAboveCloseButton: Boolean;
begin
  inherited;
  with Message do
    if PtInRect(FGripRect, SmallPointToPoint(Pos)) then
      Result := GetHitTestByCorner(FCorner)
    else
    begin
      PrevMouseAboveCloseButton := FMouseAboveCloseButton;
      FMouseAboveCloseButton := (GetTopWindow(0) = Handle) and
        ((GetCapture = 0) or FCloseButtonIsTracking) and
        PtInRect(FCloseButtonRect, SmallPointToPoint(Pos));
      if FMouseAboveCloseButton then Result := HTBORDER;
      if PrevMouseAboveCloseButton <> FMouseAboveCloseButton then
        SendMessage(Handle, WM_NCPAINT, 0, 0);
    end;
end;

procedure TdxBarPopupLookupControl.WMNCLButtonDown(var Message: TWMNCLButtonDown);
begin
  inherited;
  if FMouseAboveCloseButton then
  begin
    FCloseButtonIsTracking := True;
    SetCapture(Handle);
    SendMessage(Handle, WM_NCPAINT, 0, 0);
  end;
end;

procedure TdxBarPopupLookupControl.WMNCPaint(var Message: TWMNCPaint);
begin
  inherited;
  if IsPopup then
    Painter.SysPanelDraw(Handle, FCombo.AllowResizing,
      FMouseAboveCloseButton, FCloseButtonIsTracking, FCloseButtonRect, FGripRect,
      FCorner, TdxBarManagerAccess(FCombo.BarManager).ScaleFactor);
end;

procedure TdxBarPopupLookupControl.WMSize(var Message: TWMSize);
var
  TextHeight, Rows: Integer;
begin
  inherited;
  TextHeight := GetTextHeight;
  Rows := Message.Height div TextHeight;
  if Rows < 1 then Rows := 1;
  FRowCount := Rows;
  if ListLink.BufferCount <> Rows then
  begin
    ListLink.BufferCount := Rows;
    ListLinkDataChanged;
  end;
end;

procedure TdxBarPopupLookupControl.WMTimer(var Message: TMessage);
begin
  TimerScroll;
end;

procedure TdxBarPopupLookupControl.WMVScroll(var Message: TWMVScroll);
var
  SI: TScrollInfo;
begin
  with Message, ListLink.DataSet do
    case ScrollCode of
      SB_LINEUP: MoveBy(-FRecordIndex - 1);
      SB_LINEDOWN: MoveBy(FRecordCount - FRecordIndex);
      SB_PAGEUP: MoveBy(-FRecordIndex - FRecordCount + 1);
      SB_PAGEDOWN: MoveBy(FRecordCount - FRecordIndex + FRecordCount - 2);
      SB_THUMBPOSITION:
        if IsSequenced then
        begin
          SI.cbSize := SizeOf(SI);
          SI.fMask := SIF_ALL;
          GetScrollInfo(Self.Handle, SB_VERT, SI);
          if SI.nTrackPos <= 1 then First
          else if SI.nTrackPos >= RecordCount then Last
          else RecNo := SI.nTrackPos;
        end
        else
          case Pos of
            0: First;
            1: MoveBy(-FRecordIndex - FRecordCount + 1);
            2: Exit;
            3: MoveBy(FRecordCount - FRecordIndex + FRecordCount - 2);
            4: Last;
          end;
      SB_BOTTOM: Last;
      SB_TOP: First;
    end;
end;

procedure TdxBarPopupLookupControl.WMWindowPosChanging(var Message : TWMWINDOWPOSCHANGING);
var
  BorderSize, TextHeight, Rows, AHeight: Integer;
begin
  if IsPopup then
  begin
    BorderSize := 2 + Byte(FCombo.AllowResizing) * dxDropDownNCHeight;
    TextHeight := GetTextHeight;
    with Message.WindowPos^ do
      AHeight := cy;
    Rows := (AHeight - BorderSize) div TextHeight;
    if Rows < 1 then Rows := 1;
    with Message.WindowPos^ do
      if ComboTop < y + cy then
        cy :=  Rows * TextHeight + BorderSize
      else
      if (AHeight <> 0) then begin
        cy := Rows * TextHeight + BorderSize;
        y := y + AHeight - cy;
      end;
  end;
  inherited;
end;

procedure TdxBarPopupLookupControl.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseAboveCloseButton then
  begin
    FMouseAboveCloseButton := False;
    if HandleAllocated then SendMessage(Handle, WM_NCPAINT, 0, 0);
  end;
end;

procedure TdxBarPopupLookupControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    if IsPopup then
      ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST
    else
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
end;

procedure TdxBarPopupLookupControl.CreateWnd;
begin
  inherited CreateWnd;
  if IsPopup then
  begin
    Windows.SetParent(Handle, 0);
    CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
    FHScrollWidth := GetSystemMetrics(SM_CYHSCROLL);
    FVScrollWidth := GetSystemMetrics(SM_CXVSCROLL);
    FCloseBtnDown := False;
    FCloseBtnPaint := False;
  end;
  UpdateScrollBar;
end;

procedure TdxBarPopupLookupControl.DblClick;
begin
  inherited;
  if not IsPopup then
    FCombo.FForm.ModalResult := mrOk;
end;

procedure TdxBarPopupLookupControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if (FListLink <> nil) and (AComponent = ListSource) then ListSource := nil;
end;

procedure TdxBarPopupLookupControl.KeyDown(var Key: Word; Shift: TShiftState);
var
  Delta: Integer;
begin
  inherited KeyDown(Key, Shift);
  if not FListActive then Exit;
  Delta := 0;
  case Key of
    VK_UP, VK_LEFT: Delta := -1;
    VK_DOWN, VK_RIGHT: Delta := 1;
    VK_PRIOR: Delta := 1 - FRowCount;
    VK_NEXT: Delta := FRowCount - 1;
    VK_HOME: Delta := MinInt;
    VK_END: Delta := MaxInt;
  end;
  if Delta <> 0 then
  begin
    if Delta = MinInt then
      ListLink.DataSet.First
    else
      if Delta = MaxInt then
        ListLink.DataSet.Last
      else
        ListLink.DataSet.MoveBy(Delta);
    SelectCurrent;
  end;
end;

procedure TdxBarPopupLookupControl.ListLinkDataChanged;
begin
  if FListActive then
  begin
    FRecordIndex := ListLink.ActiveRecord;
    FRecordCount := ListLink.RecordCount;
  end else
  begin
    FRecordIndex := 0;
    FRecordCount := 0;
  end;
  if HandleAllocated then
  begin
    UpdateScrollBar;
    Invalidate;
  end;
end;

procedure TdxBarPopupLookupControl.MouseDown(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer);
begin
  if (Button = mbLeft) and
    Assigned(ListLink.DataSet) {and ListLink.DataSet.CanModify} then
    if ssDouble in Shift then
      if FRecordIndex = Y div GetTextHeight then
        DblClick
      else
    else
    begin
      MouseCapture := True;
      FTracking := True;
      SelectItemAt(X, Y);
    end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TdxBarPopupLookupControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FTracking then
  begin
    SelectItemAt(X, Y);
    FMousePos := Y;
    TimerScroll;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TdxBarPopupLookupControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FTracking then
  begin
    StopTracking;
    SelectItemAt(X, Y);
    if (FCombo <> nil) and ListLink.Active and IsPopup then
    begin
      if Y < 0 then Y := 0;
      if Y >= ClientHeight then Y := ClientHeight - 1;
      Y := Y div GetTextHeight;
      if Y >= ListLink.RecordCount then Exit;
      with FCombo do
        try
          if FKeyField <> nil then FKeyValue := FKeyField.Value;
          KeyValueChanged;
        finally
          if (CurItemLink <> nil) and (CurItemLink.RealItemLink <> nil) then
            CurItemLink.RealItemLink.BringToTopInRecentList(True);
          try
            BarManager.HideAll;
          except
          end;
        end;
    end;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TdxBarPopupLookupControl.Paint;
var
  I, J, W, X, TextWidth, TextHeight, LastFieldIndex, SelectedRecord: Integer;
  Selected : Boolean;
  S: string;
  R: TRect;
  Field: TField;
  AAlignment: TAlignment;
begin
  if not FListActive then
  begin
    Canvas.FillRect(ClientRect);
    Exit;
  end;

  Canvas.Font := Font;
  TextWidth := Canvas.TextWidth('0');
  TextHeight := Canvas.TextHeight('0');
  LastFieldIndex := ListFields.Count - 1;
  if ColorToRGB(Color) <> ColorToRGB(clBtnFace) then
    Canvas.Pen.Color := clBtnFace else
    Canvas.Pen.Color := clBtnShadow;
  SelectedRecord := ListLink.ActiveRecord;
  for I := 0 to FRowCount - 1 do
  begin
    Canvas.Font.Color := Font.Color;
    Canvas.Brush.Color := Color;
    R.Top := I * TextHeight;
    R.Bottom := R.Top + TextHeight;
    Selected := False;
    if I < FRecordCount then
    begin
      ListLink.ActiveRecord := I;
      if (SelectedRecord = I) then
      begin
        Canvas.Font.Color := clHighlightText;
        Canvas.Brush.Color := clHighlight;
        Selected := True;
      end;
      R.Right := 0;
      for J := 0 to LastFieldIndex do
      begin
        Field := ListFields[J];
        if J < LastFieldIndex then
          W := Field.DisplayWidth * TextWidth + 4 else
          W := ClientWidth - R.Right;
        S := Field.DisplayText;
        X := 2;
        AAlignment := Field.Alignment;
        case AAlignment of
          taRightJustify: X := W - Canvas.TextWidth(S) - 3;
          taCenter: X := (W - Canvas.TextWidth(S)) div 2;
        end;
        R.Left := R.Right;
        R.Right := R.Right + W;
        Canvas.TextRect(R, R.Left + X, R.Top, S);
        if J < LastFieldIndex then
        begin
          Canvas.MoveTo(R.Right, R.Top);
          Canvas.LineTo(R.Right, R.Bottom);
          Inc(R.Right);
          if R.Right >= ClientWidth then Break;
        end;
      end;
    end;
    R.Left := 0;
    R.Right := ClientWidth;
    if I >= FRecordCount then Canvas.FillRect(R);
    if Selected then
      Canvas.DrawFocusRect(R);
  end;
  R.Top := R.Bottom;
  R.Bottom := ClientHeight;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(R);
  if FRecordCount <> 0 then ListLink.ActiveRecord := FRecordIndex;
end;

function TdxBarPopupLookupControl.GetTextHeight: Integer;
begin
  Result := cxTextHeight(Font);
end;

procedure TdxBarPopupLookupControl.UpdateListFields;
var
  DataSet: TDataSet;
begin
  FListField := nil;
  FListFields.Clear;
  FListActive := False;
  if FListLink.Active then
  begin
    DataSet := FListLink.DataSet;
    try
      DataSet.GetFieldList(FListFields, FListFieldName);
    except
      raise;
    end;
    if (FListFieldIndex >= 0) and (FListFieldIndex < FListFields.Count) then
      FListField := FListFields[FListFieldIndex]
    else
      if (FListFields.Count > 0) then
        FListField := FListFields[0];
    FListActive := FListField <> nil;
  end;
end;

procedure TdxBarPopupLookupControl.UpdateSizeGripCorner(ADropDownPosition: TPoint);
var
  AEditRect, ADropDownRect: TRect;
begin
  if IsPopup and (FCombo.CurItemLink <> nil) and (FCombo.CurItemLink.Control <> nil) then
  begin
    AEditRect := FCombo.CurItemLink.ItemRect;
    AEditRect.TopLeft := FCombo.CurItemLink.Control.Parent.ClientToScreen(AEditRect.TopLeft);
    AEditRect.BottomRight := FCombo.CurItemLink.Control.Parent.ClientToScreen(AEditRect.BottomRight);
    ADropDownRect := cxRectBounds(ADropDownPosition.X, ADropDownPosition.Y, Width, Height);
    FCorner := GetCornerForRects(AEditRect, ADropDownRect);
  end;
end;

function TdxBarPopupLookupControl.GetListSource: TDataSource;
begin
  Result := FListLink.DataSource;
end;

function TdxBarPopupLookupControl.GetPainter: TdxBarPainter;
begin
  if IsPopup then
    Result := FCombo.CurItemLink.Control.Painter
  else
    Result := FCombo.BarManager.DefaultPainter;
end;

procedure TdxBarPopupLookupControl.SetListFieldName(const Value: string);
begin
  if FListFieldName <> Value then
  begin
    FListFieldName := Value;
    UpdateListFields;
  end;
end;

procedure TdxBarPopupLookupControl.SetListSource(Value: TDataSource);
begin
  FListLink.DataSource := Value;
  cxAddFreeNotification(Self, Value);
end;

procedure TdxBarPopupLookupControl.SelectCurrent;
begin
  if FCombo <> nil then
  begin
    FCombo.EditText := FListField.DisplayText;
    FCombo.ResetFindStr;
    SendMessage(FCombo.GetEditHandle, EM_SETSEL, 0, Length(FCombo.EditText));
  end;
end;

procedure TdxBarPopupLookupControl.SelectItemAt(X, Y: Integer);
var
  Delta: Integer;
begin
  if not FCombo.FListActive then Exit;
  if Y < 0 then Y := 0;
  if Y >= ClientHeight then Y := ClientHeight - 1;
  Delta := Y div GetTextHeight - FRecordIndex;
  ListLink.DataSet.MoveBy(Delta);
  SelectCurrent;
end;

procedure TdxBarPopupLookupControl.SetRowCount(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 100 then Value := 100;
  Height := Value * GetTextHeight + 2 +
    Byte(IsPopup and FCombo.AllowResizing) * dxDropDownNCHeight;
end;

procedure TdxBarPopupLookupControl.StopTimer;
begin
  if FTimerActive then
  begin
    KillTimer(Handle, 1);
    FTimerActive := False;
  end;
end;

procedure TdxBarPopupLookupControl.StopTracking;
begin
  if FTracking then
  begin
    StopTimer;
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TdxBarPopupLookupControl.TimerScroll;
var
  Delta, Distance, Interval: Integer;
begin
  Delta := 0;
  Distance := 0;
  if FMousePos < 0 then
  begin
    Delta := -1;
    Distance := -FMousePos;
  end;
  if FMousePos >= ClientHeight then
  begin
    Delta := 1;
    Distance := FMousePos - ClientHeight + 1;
  end;
  if Delta = 0 then StopTimer else
  begin
    if ListLink.DataSet.MoveBy(Delta) <> 0 then SelectCurrent;
    Interval := 200 - Distance * 15;
    if Interval < 0 then Interval := 0;
    SetTimer(Handle, 1, Interval, nil);
    FTimerActive := True;
  end;
end;

procedure TdxBarPopupLookupControl.UpdateScrollBar;
var
  SIOld, SINew: TScrollInfo;
begin
  if FListLink.Active and HandleAllocated then
    with ListLink.DataSet do
    begin
      SIOld.cbSize := SizeOf(SIOld);
      SIOld.fMask := SIF_ALL;
      GetScrollInfo(Self.Handle, SB_VERT, SIOld);
      SINew := SIOld;
      if IsSequenced then
      begin
        SINew.nMin := 1;
        SINew.nPage := FRowCount;
        SINew.nMax := Integer(DWORD(RecordCount) + SINew.nPage - 1);
        if State in [dsInactive, dsBrowse, dsEdit] then
          SINew.nPos := RecNo;
      end
      else
      begin
        SINew.nMin := 0;
        SINew.nPage := 0;
        SINew.nMax := 4;
        if BOF then SINew.nPos := 0
        else if EOF then SINew.nPos := 4
        else SINew.nPos := 2;
      end;
      if (SINew.nMin <> SIOld.nMin) or (SINew.nMax <> SIOld.nMax) or
        (SINew.nPage <> SIOld.nPage) or (SINew.nPos <> SIOld.nPos) then
        SetScrollInfo(Self.Handle, SB_VERT, SINew, True);
    end;
end;

initialization
  dxBarRegisterItem(TdxBarLookupCombo, TdxBarLookupComboControl, True);

finalization
  dxBarUnregisterItem(TdxBarLookupCombo);

end.
