{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express image controls                                   }
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

unit dximctrl;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  ImgList, Types, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, cxControls;

type
  TdxImageAlign = (dxliLeft, dxliRight);
  TVertAlignment = (tvaTop, tvaCenter, tvaBottom);
  TdxImageDrawItemEvent = procedure(Sender : TObject; Index: Integer; Rect: TRect) of object;

  TdxCustomImageListBox = class(TCustomListBox)
  private
    FImageList : TCustomImageList;
    FChangeLink : TChangeLink;
    FAlignment : TAlignment;
    FVertAlignment : TVertAlignment;
    FImageAlign : TdxImageAlign;
    FMultiLines : Boolean;
    FItemHeight : Integer;
    FOnDrawItem : TdxImageDrawItemEvent;
    FDrawEdgeIndex : Integer;
    FDrawImageOnly : Boolean;
    FDeletedSt : String;
    FDeletedIndex : Integer;
    FHintWindow : THintWindow;
    FHintWindowShowing : Boolean;
    FHintIndex : Integer;
    FItemTextHeight : Integer;

    function GetImageIndex(Index : Integer) : Integer;
    function GetValue(Index : Integer) : String;
    procedure SetImageIndex(Index : Integer; Value : Integer);
    procedure SetImageList(Value : TCustomImageList);
    procedure SetAlignment(Value : TAlignment);
    procedure SetImageAlign(Value : TdxImageAlign);
    procedure SetItemHeight(Value : Integer);
    procedure SetMultiLines(Value : Boolean);
    procedure SetVertAlignment(Value : TVertAlignment);
    procedure SetValue(Index : Integer; const Value : String);
    procedure StringsRead(Reader: TReader);
    procedure StringsWrite(Writer: TWriter);
    procedure SetInheritedItemHeight;
    procedure OnChangeLink(Sender : TObject);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    function GetImageRect(ItemIndex : Integer) : TRect;
    procedure DrawImageFocus(Index : Integer);
  protected
    FStrings : TStrings;
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure WndProc(var Message : TMessage); override;
    function ValuesIndexOf(Text : String) : Integer;

    property Alignment : TAlignment read FAlignment write SetAlignment;
    property ImageAlign : TdxImageAlign read FImageAlign write SetImageAlign;
    property ItemHeight : Integer read FItemHeight write SetItemHeight;
    property ImageList : TCustomImageList read FImageList write SetImageList;
    property MultiLines : Boolean read FMultiLines write SetMultiLines;
    property VertAlignment : TVertAlignment read FVertAlignment write SetVertAlignment;
    property OnDrawItem : TdxImageDrawItemEvent read FOnDrawItem write FOnDrawItem;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AddItem(St :String; ImageIndex : Integer); reintroduce;
    procedure InsertItem(Index: Integer; St :String; ImageIndex : Integer);
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure MoveItem(CurIndex, NewIndex: Integer);

    property ImageIndexes[Index : Integer] : Integer read GetImageIndex write SetImageIndex;
    property Values[Index : Integer] : String read GetValue write SetValue;
  end;

  TdxImageListBox = class(TdxCustomImageListBox)
  published
    property Alignment;
    property ImageAlign;
    property ItemHeight;
    property ImageList;
    property MultiLines;
    property VertAlignment;
    property OnDrawItem;

    property ExtendedSelect;
    property MultiSelect;

    property Align;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property IntegralHeight;
    property Items;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property Anchors;
    property Constraints;
    property OnStartDock;
    property OnEndDock;
  end;

  TdxCustomImageComboBox = class(TCustomComboBox)
  private
    FImageList : TCustomImageList;
    FChangeLink : TChangeLink;
    FAlignment : TAlignment;
    FVertAlignment : TVertAlignment;
    FImageAlign : TdxImageAlign;
    FMultiLines : Boolean;
    FItemHeight : Integer;
    FOnDrawItem : TdxImageDrawItemEvent;
    FDeletedSt : String;
    FDeletedIndex : Integer;

    function GetImageIndex(Index : Integer) : Integer;
    function GetValue(INdex : Integer) : String;
    procedure SetImageIndex(Index : Integer; Value : Integer);
    procedure SetImageList(Value : TCustomImageList);
    procedure SetAlignment(Value : TAlignment);
    procedure SetImageAlign(Value : TdxImageAlign);
    procedure SetInternalItemHeight(Value : Integer);
    procedure SetMultiLines(Value : Boolean);
    procedure SetVertAlignment(Value : TVertAlignment);
    procedure SetValue(Index : Integer; const Value : String);
    procedure StringsRead(Reader: TReader);
    procedure StringsWrite(Writer: TWriter);
    procedure SetInheritedItemHeight;
    procedure OnChangeLink(Sender : TObject);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    FStrings : TStrings;
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure WndProc(var Message : TMessage); override;
    function ValuesIndexOf(Text : String) : Integer;
  protected
    property Alignment : TAlignment read FAlignment write SetAlignment;
    property ImageAlign : TdxImageAlign read FImageAlign write SetImageAlign;
    property ItemHeight : Integer read FItemHeight write SetInternalItemHeight;
    property ImageList : TCustomImageList read FImageList write SetImageList;
    property MultiLines : Boolean read FMultiLines write SetMultiLines;
    property VertAlignment : TVertAlignment read FVertAlignment write SetVertAlignment;
    property OnDrawItem : TdxImageDrawItemEvent read FOnDrawItem write FOnDrawItem;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AddItem(St :String; ImageIndex : Integer); reintroduce;
    procedure InsertItem(Index: Integer; St :String; ImageIndex : Integer);
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure MoveItem(CurIndex, NewIndex: Integer);

    property Values[Index : Integer] : String read GetValue write SetValue;
    property ImageIndexes[Index : Integer] : Integer read GetImageIndex write SetImageIndex;
  end;

  TdxImageComboBox = class(TdxCustomImageComboBox)
  published
    property Alignment;
    property ImageAlign;
    property ItemHeight;
    property ImageList;
    property MultiLines;
    property VertAlignment;
    property OnDrawItem;

    property BiDiMode;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property Items;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
    property Anchors;
    property Constraints;
    property OnStartDock;
    property OnEndDock;
  end;

  TdxUpDownAlign = (udaBottom, udaLeft, udaRight, udaTop);
  TdxHSpinImageAlign = (hsiLeft, hsiCenter, hsiRight);
  TdxVSpinImageAlign = (vsiTop, vsiCenter, vsiBottom);

  TdxSpinImageItems = class;
  TdxCustomSpinImage = class;

  TdxSpinImageItem = class(TCollectionItem)
  private
    Owner : TdxSpinImageItems;
    FImageIndex : Integer;
    FValue : String;
    FHint : String;

    procedure SetImageIndex(Value : Integer);
    procedure SetValue(Value : String);
    procedure SetHint(Value : String);
  public
    constructor Create(Collection : TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ImageIndex : Integer read FImageIndex write SetImageIndex;
    property Hint : String read FHint write SetHint;
    property Value : String read FValue write SetValue;
  end;

  TdxSpinImageItems = class(TCollection)
  private
    Owner : TdxCustomSpinImage;

    function GetItem(Index : Integer) : TdxSpinImageItem;
    procedure SetItem(Index : Integer; Value : TdxSpinImageItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner : TdxCustomSpinImage);

    function Add : TdxSpinImageItem;
    function IndexOf(Value : String) : Integer;
    property Items[Index : Integer] : TdxSpinImageItem read GetItem write SetItem; default;
  end;

  TdxSIChange = procedure(Sender: TObject; ItemIndex: Integer) of object;
  TdxsiOrientation = (siHorizontal, siVertical);

  TdxCustomSpinImage = class(TCustomControl)
  private
    FTimer: TTimer;
    FScrollTimerCount: Integer;

    FAutoSize : Boolean;
    FDefaultImages : Boolean;
    FBorderStyle: TBorderStyle;
    FChangeLink : TChangeLink;
    FItemIndex : Integer;
    FImageList : TCustomImageList;
    FImageHAlign : TdxHSpinImageAlign;
    FImageVAlign : TdxVSpinImageAlign;
    FItems : TdxSpinImageItems;
    FOnChange : TdxSIChange;
    FReadOnly : Boolean;
    FUseDblClick : Boolean;

    FStretch : Boolean;
    FUpDownAlign : TdxUpDownAlign;
    FUpDownOrientation: TdxsiOrientation;
    FUpDownWidth : Integer;
    FNCSide : Integer;
    FUpPress : Boolean;
    FDownPress : Boolean;
    FUpButtonRect : TRect;
    FDownButtonRect : TRect;
    FUpButtonEnabled : Boolean;
    FDownButtonEnabled : Boolean;

    procedure SetInternalAutoSize(Value : Boolean);
    procedure SetBorderStyle(Value : TBorderStyle);
    procedure SetDefaultImages(Value : Boolean);
    procedure SetItemIndex(Value : Integer);
    procedure SetImageList(Value : TCustomImageList);
    procedure SetImageHAlign(Value : TdxHSpinImageAlign);
    procedure SetImageVAlign(Value : TdxVSpinImageAlign);
    procedure SetItems(Value : TdxSpinImageItems);
    procedure SetStretch(Value : Boolean);
    procedure SetUpDownAlign(Value : TdxUpDownAlign);
    procedure SetUpDownOrientation(Value : TdxsiOrientation);
    procedure SetUpDownWidth(Value : Integer);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMNCCalcSize(var Message : TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message : TWMNCPaint); message WM_NCPAINT;
    procedure WMNCLButtonDblClk(var Message : TWMNCLBUTTONDOWN); message WM_NCLBUTTONDBLCLK;
    procedure WMNCMouseDown(var Message : TWMNCLBUTTONDOWN); message WM_NCLBUTTONDOWN;
    procedure WMNCMouseUp(var Message : TWMNCLBUTTONUP); message WM_NCLBUTTONUP;
    procedure WMMouseUp(var Message : TWMLBUTTONUP); message WM_LBUTTONUP;
    procedure WMNCHitTest(var Message : TWMNCHITTEST); message WM_NCHITTEST;
    procedure UpDownClick(AKey : Word);
    procedure OnChangeLink(Sender : TObject);
    procedure MakeAutoSize;
    procedure SetNextItem;
    function IsLastItem : Boolean;
    procedure UpdateNCRegion;
    procedure NCMouseDown(X, Y : Integer);
    procedure DoTimerScroll(Sender: TObject);
  protected
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    procedure Change; virtual;
    function CanChange : Boolean; virtual;
    procedure UpdateItems; virtual;

  {$IFDEF DELPHIBERLIN}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
  {$ELSE}
    procedure ChangeScale(M, D: Integer); override;
  {$ENDIF}

    property AutoSize : Boolean read FAutoSize write SetInternalAutoSize;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property DefaultImages : Boolean read FDefaultImages write SetDefaultImages;
    property ImageHAlign : TdxHSpinImageAlign read FImageHAlign write SetImageHAlign;
    property ImageVAlign : TdxVSpinImageAlign read FImageVAlign write SetImageVAlign;
    property ItemIndex : Integer read FItemIndex write SetItemIndex;
    property ReadOnly : Boolean read FReadOnly write FReadOnly;
    property Stretch : Boolean read FStretch write SetStretch;
    property UpDownAlign : TdxUpDownAlign read FUpDownAlign write SetUpDownAlign;
    property UpDownOrientation: TdxsiOrientation read FUpDownOrientation
                                write SetUpDownOrientation;
    property UpDownWidth : Integer read FUpDownWidth write SetUpDownWidth;
    property UseDblClick : Boolean read FUseDblClick write FUseDblClick;
    property OnChange : TdxSIChange read FOnChange write FOnChange;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property ImageList : TCustomImageList read FImageList write SetImageList;
    property Items : TdxSpinImageItems read FItems write SetItems;
  end;

  TdxSpinImage = class(TdxCustomSpinImage)
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property Constraints;
    property DefaultImages;
    property ImageList;
    property ImageHAlign;
    property ImageVAlign;
    property Items;
    property ItemIndex;
    property ReadOnly;
    property Stretch;
    property UpDownAlign;
    property UpDownOrientation;
    property UpDownWidth;
    property UseDblClick;
    property OnChange;

    property Align;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnStartDock;
    property OnEndDock;
  end;

implementation

{TdxCustomImageListBox}
constructor TdxCustomImageListBox.Create(AOwner : TComponent);
begin
  inherited;
  FStrings := TStringList.Create;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := OnChangeLink;
  FHintWindow := THintWindow.Create(self);
  FHintWindowShowing := False;
  FHintIndex := -1;

  Style :=  lbOwnerDrawFixed;
  FItemHeight := 0;
  FVertAlignment := tvaCenter;
  FDrawEdgeIndex := -1;
  FDrawImageOnly := False;
  FDeletedIndex := -1;
end;

destructor TdxCustomImageListBox.Destroy;
begin
  FHintWindow.Free;
  FChangeLink.Free;
  FStrings.Free;
  inherited;
end;

procedure TdxCustomImageListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FImageList <> nil) and
    (AComponent = FImageList) then ImageList := nil;
end;

procedure TdxCustomImageListBox.Assign(Source: TPersistent);
Var
  lb : TdxCustomImageComboBox;
  lb1 : TdxCustomImageListBox;
begin
  if(Source is TdxCustomImageComboBox)
  Or(Source is TdxCustomImageListBox) then begin
    if(Source is TdxCustomImageComboBox) then begin
      lb := TdxCustomImageComboBox(Source);
      FImageList := lb.FImageList;
      Items.Assign(lb.Items);
      FStrings.Assign(lb.FStrings);
      FImageAlign := lb.FImageAlign;
      FAlignment := lb.FAlignment;
      MultiLines := lb.MultiLines;
      ItemHeight := lb.ItemHeight;
      FVertAlignment := lb.FVertAlignment;
      Font := lb.Font;
    end;
    if(Source is TdxCustomImageListBox) then begin
      lb1 := TdxCustomImageListBox(Source);
      FImageList := lb1.FImageList;
      IntegralHeight := lb1.IntegralHeight;
      ItemHeight := lb1.ItemHeight;
      Items.Assign(lb1.Items);
      FStrings.Assign(lb1.FStrings);
      FImageAlign := lb1.FImageAlign;
      FAlignment := lb1.FAlignment;
      MultiLines := lb1.MultiLines;
      FVertAlignment := lb1.FVertAlignment;
      Font := lb1.Font;
    end;
    SetInheritedItemHeight;
    Repaint;
  end
  else inherited;
end;

function TdxCustomImageListBox.GetImageIndex(Index : Integer) : Integer;
Var
  St : String;
begin
  Result := -1;
  if(Index < FStrings.Count) then begin
    St := FStrings[Index];
    if(Pos(',', St) > 0) then begin
      St := Copy(St, 1, Pos(',', St) - 1);
      if(St <> '') then
        Result := StrToInt(St);
    end;
  end;
end;

function TdxCustomImageListBox.GetValue(Index : Integer) : String;
begin
  Result := '';
  if(Index < FStrings.Count) And (Pos(',', FStrings[Index]) > 0) then
    Result := Copy(FStrings[Index], Pos(',', FStrings[Index]) + 1, 1000);
end;

procedure TdxCustomImageListBox.SetImageIndex(Index : Integer; Value : Integer);
Var
  St : String;
begin
  if(Index < FStrings.Count) And (Index > -1)
  And (Value <> ImageIndexes[Index])then begin
    St := Values[Index];
    FStrings[Index] := IntToStr(Value) + ',' + St;
    if(HandleAllocated) then Repaint;
  end;
end;

procedure TdxCustomImageListBox.SetAlignment(Value : TAlignment);
begin
  if(Value <> FAlignment) then begin
    FAlignment := Value;
    Repaint;
  end;
end;

procedure TdxCustomImageListBox.SetVertAlignment(Value : TVertAlignment);
begin
  if(Value <> FVertAlignment) then begin
    FVertAlignment := Value;
    Repaint;
  end;
end;

procedure TdxCustomImageListBox.SetImageAlign(Value : TdxImageAlign);
begin
  if(Value <> FImageAlign) then begin
    FImageAlign := Value;
    Repaint;
  end;
end;

procedure TdxCustomImageListBox.SetImageList(Value : TCustomImageList);
begin
  if(Value <> FImageList) then begin
    if(FImageList <> Nil) then
      FImageList.UnRegisterChanges(FChangeLink);
    FImageList := Value;
    if(FImageList <> Nil) then
      FImageList.RegisterChanges(FChangeLink);
    SetInheritedItemHeight;
  end;
end;

procedure TdxCustomImageListBox.SetItemHeight(Value : Integer);
begin
  if(Value <> FItemHeight) then begin
    if(Value < 10) then
      FItemHeight := 0
    else FItemHeight := Value;
    SetInheritedItemHeight;
  end;
end;

procedure TdxCustomImageListBox.SetMultiLines(Value : Boolean);
begin
  if(Value <> FMultiLines) then begin
    FMultiLines := Value;
    Repaint;
  end;
end;

procedure TdxCustomImageListBox.SetValue(Index : Integer; const Value : String);
Var
  St : String;
begin
  if(Index < FStrings.Count) And (Index > -1)
  And (Value <> Values[Index])then begin
    St := IntToStr(ImageIndexes[Index]);
    FStrings[Index] := St + ',' + Value;
  end;
end;

procedure TdxCustomImageListBox.AddItem(St :String; ImageIndex : Integer);
begin
  Items.Add(St);
  SetImageIndex(Items.Count -1, ImageIndex);
end;

procedure TdxCustomImageListBox.InsertItem(Index : Integer;
          St :String; ImageIndex : Integer);
begin
  if(Index < 0) then Index := 0;
  if(Index >= Items.Count) then
    AddItem(St, ImageIndex)
  else begin
    Items.Insert(Index, St);
    SetImageIndex(Index, ImageIndex);
  end;
end;

procedure TdxCustomImageListBox.ExchangeItems(Index1, Index2 : Integer);
var
  flag : Boolean;
  St1, St2 : string;
begin
  flag := (Index1 > -1 ) And (Index1 < Items.Count)
           And (Index2 > -1 ) And (Index2 < Items.Count);
  if(flag) then begin
    St1 := FStrings[Index1];
    St2 := FStrings[Index2];
  end;
  Items.Exchange(Index1, Index2);
  if(flag) then begin
    FStrings[Index1] := St2;
    FStrings[Index2] := St1;
  end;
end;

procedure TdxCustomImageListBox.MoveItem(CurIndex, NewIndex: Integer);
Var
  TempString: string;
begin
  if (CurIndex <> NewIndex) And (CurIndex > -1) And (CurIndex < Items.Count) then begin
    TempString := FStrings[CurIndex];
    Items.Move(CurIndex, NewIndex);
    FStrings[NewIndex] := TempString;
  end;
end;

procedure TdxCustomImageListBox.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('SaveStrings', StringsRead, StringsWrite, True);
end;

procedure TdxCustomImageListBox.StringsRead(Reader: TReader);
begin
  Reader.ReadListBegin;
  FStrings.Clear;
  while not Reader.EndOfList do
    FStrings.Add(Reader.ReadString);
  Reader.ReadListEnd;
end;

procedure TdxCustomImageListBox.StringsWrite(Writer: TWriter);
var
  i: Integer;
begin
  Writer.WriteListBegin;
  for i := 0 to FStrings.Count - 1 do
    Writer.WriteString(FStrings[I]);
  Writer.WriteListEnd;
end;

function TdxCustomImageListBox.GetImageRect(ItemIndex: Integer): TRect;
var
  AImageAlign: TdxImageAlign;
  AImageWidth : Integer;
  R : TRect;
begin
  R := ItemRect(ItemIndex);
  if FImageList <> nil then
  begin
    Result.Top := R.Top + 1;
    Result.Bottom := R.Bottom - 1;
    AImageWidth := ((Result.Bottom - Result.Top) * FImageList.Width) div FImageList.Height;
    AImageAlign := FImageAlign;
    if UseRightToLeftAlignment then
    begin
      if AImageAlign = dxliLeft then
        AImageAlign := dxliRight
      else
        AImageAlign := dxliLeft;
    end;
    if AImageAlign = dxliLeft then
    begin
       Result.Left := R.Left + 1 ;
       Result.Right := Result.Left + AImageWidth + 2;
    end
    else
    begin
      Result.Right := R.Right - 1;
      Result.Left := Result.Right - 2 - AImageWidth;
    end;
  end;
end;

procedure TdxCustomImageListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  CHorizontalAlignFlags: array [TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
  CVerticalAlignFlags: array [TVertAlignment] of Integer = (DT_TOP, DT_VCENTER, DT_BOTTOM);
var
  R, R1: TRect;
  AImage: TBitmap;
  AImageAlign: TdxImageAlign;
  AImageFlag: Boolean;
  ADrawFlags: Integer;
begin
  AImageFlag := (FImageList <> nil) and (ImageIndexes[Index] > -1) and (ImageIndexes[Index] < FImageList.Count);
  R := GetImageRect(Index);
  R1 := Rect;
  AImageAlign := FImageAlign;
  if UseRightToLeftAlignment then
  begin
    if AImageAlign = dxliLeft then
      AImageAlign := dxliRight
    else
      AImageAlign := dxliLeft;
  end;
  if AImageFlag and (Canvas.Brush.Color = clHighlight) then
  begin
    if AImageAlign = dxliLeft then
      R1.Left := R.Right + 1
    else
      R1.Right := R.Left - 1;
    DrawImageFocus(Index);
  end;
  Canvas.FillRect(R1);
  if AImageFlag then
  begin
    InflateRect(R, -1, 0);
    AImage := TBitmap.Create;
    try
      FImageList.GetBitmap(GetImageIndex(Index), AImage);
      Canvas.StretchDraw(R, AImage);
    finally
      AImage.Free;
    end;
  end;
  if FImageList <> nil then
  begin
    InflateRect(Rect, -2, -2);
    if AImageAlign = dxliLeft then
      Rect.Left := R.Right + 2
    else
      Rect.Right := R.Left - 2;
  end;
  Inc(Rect.Left);
  Dec(Rect.Right);
  SetBkMode(Canvas.Handle, TRANSPARENT);
  if Assigned(FOnDrawItem) then
    FOnDrawItem(Self, Index, Rect)
  else
  begin
    ADrawFlags := CHorizontalAlignFlags[Alignment];
    if UseRightToLeftAlignment then
      case ADrawFlags of
        DT_LEFT:
          ADrawFlags := DT_RIGHT;
        DT_RIGHT:
          ADrawFlags := DT_LEFT;
      end;
    ADrawFlags := ADrawFlags or DT_EXPANDTABS or DT_NOPREFIX or DT_EDITCONTROL;
    if FMultiLines then
      ADrawFlags := ADrawFlags or DT_WORDBREAK
    else
      ADrawFlags := ADrawFlags or CVerticalAlignFlags[VertAlignment] or DT_SINGLELINE;
    if UseRightToLeftReading then
      ADrawFlags := ADrawFlags or DT_RTLREADING;
    DrawText(Canvas.Handle, PChar(Items[Index]), Length(Items[Index]), Rect, ADrawFlags);
    FItemTextHeight := R.Bottom - R.Top;
  end;
end;

procedure TdxCustomImageListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
  ARect: TRect;
begin
  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clHighlightText
    end;
    ARect := rcItem;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, ARect, State)
    else Canvas.FillRect(ARect);
    if odFocused in State then begin
      if(FImageList <> Nil) then
        InflateRect(ARect, -2, -3);
      DrawFocusRect(hDC, ARect);
    end;
    Canvas.Handle := 0;
  end;
end;

procedure TdxCustomImageListBox.SetInheritedItemHeight;
Var
  h : Integer;
begin
  if(FItemHeight < 10) then begin
    Canvas.Font.Size := Font.Size;
    h := Canvas.TextHeight('Wg');
    if(FImageList <> NIl) And (h < FImageList.Height) then
      h := FImageList.Height;
    Inc(h, 2);
  end else h := FItemHeight;
  if(h <> inherited ItemHeight) then
    inherited ItemHeight := h;
  if HandleAllocated then
    Repaint;
end;

procedure TdxCustomImageListBox.OnChangeLink(Sender : TObject);
begin
  SetInheritedItemHeight;
end;

procedure TdxCustomImageListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  SetInheritedItemHeight;
end;

procedure TdxCustomImageListBox.DrawImageFocus(Index : Integer);
Var
  r : TRect;
  SColor : TColor;
begin
  if (FImageList <> Nil) then begin
    if(Index > -1) And (FDrawEdgeIndex <> Index) then begin
      if(FDrawEdgeIndex > -1) And (ImageIndexes[FDrawEdgeIndex] > -1)
      And (ImageIndexes[FDrawEdgeIndex] < FImageList.Count) then begin
        r := GetImageRect(FDrawEdgeIndex);
        SColor := Canvas.Brush.Color;
        Canvas.Brush.Color := Color;
        Canvas.FrameRect(r);
        Canvas.Brush.Color := SColor;
      end;
      FDrawEdgeIndex := Index;
      if(FDrawEdgeIndex > -1) And (ImageIndexes[FDrawEdgeIndex] > -1)
      And (ImageIndexes[FDrawEdgeIndex] < FImageList.Count) then begin
        r := GetImageRect(FDrawEdgeIndex);
        DrawEdge(Canvas.Handle, R, BDR_RAISEDOUTER, BF_BOTTOMRIGHT);
        Dec(R.Bottom);
        Dec(R.Right);
        if(Color = clWindow) Or (Color = clWhite) then
          DrawEdge(Canvas.Handle, R, BDR_RAISEDOUTER, BF_TOPLEFT)
       else DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER, BF_TOPLEFT);
      end;
    end;
  end;
end;

procedure TdxCustomImageListBox.WndProc(var Message : TMessage);
var
  AIndex: Integer;
begin
  with Message do
    case Msg of
      LB_INSERTSTRING:
      begin
        AIndex := wParam;
        if (FDeletedIndex = AIndex) And (AIndex <> -1) then
          FStrings.Insert(AIndex, FDeletedSt)
        else begin
          FStrings.Insert(AIndex, '');
          ImageIndexes[AIndex] := -1;
        end;
      end;
      LB_ADDSTRING:
      begin
        FStrings.Add('');
        ImageIndexes[FStrings.Count - 1] := -1;
      end;
      LB_DELETESTRING:
      begin
       FDeletedIndex := wParam;
       FDeletedSt := FStrings[wParam];
       FStrings.Delete(wParam);
      end;
      else FDeletedIndex := -1;
    end;
  inherited;
end;

function TdxCustomImageListBox.ValuesIndexOf(Text : String) : Integer;
begin
  for Result := 0 to FStrings.Count - 1 do
    if AnsiCompareText(Text, Values[Result]) = 0 then Exit;
  Result := -1;
end;

{TdxCustomImageComboBox}
constructor TdxCustomImageComboBox.Create(AOwner : TComponent);
begin
  inherited;
  FStrings := TStringList.Create;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := OnChangeLink;

  Style := csOwnerDrawFixed;
  FItemHeight := 0;
  FVertAlignment := tvaCenter;
end;

destructor TdxCustomImageComboBox.Destroy;
begin
  FChangeLink.Free;
  FStrings.Free;
  inherited;
end;

procedure TdxCustomImageComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FImageList <> nil) and
    (AComponent = FImageList) then ImageList := nil;
end;

procedure TdxCustomImageComboBox.Assign(Source: TPersistent);
Var
  lb : TdxCustomImageComboBox;
  lb1 : TdxCustomImageListBox;
begin
  if(Source is TdxCustomImageComboBox)
  Or(Source is TdxCustomImageListBox) then begin
    if(Source is TdxCustomImageComboBox) then begin
      lb := TdxCustomImageComboBox(Source);
      FImageList := lb.FImageList;
      Items.Assign(lb.Items);
      FStrings.Assign(lb.FStrings);
      FImageAlign := lb.FImageAlign;
      FAlignment := lb.FAlignment;
      MultiLines := lb.MultiLines;
      FVertAlignment := lb.FVertAlignment;
      Font := lb.Font;
    end;
    if(Source is TdxCustomImageListBox) then begin
      lb1 := TdxCustomImageListBox(Source);
      FImageList := lb1.FImageList;
      Items.Assign(lb1.Items);
      FStrings.Assign(lb1.FStrings);
      FImageAlign := lb1.FImageAlign;
      FAlignment := lb1.FAlignment;
      MultiLines := lb1.MultiLines;
      FVertAlignment := lb1.FVertAlignment;
      Font := lb1.Font;
    end;
    SetInheritedItemHeight;
  end
  else inherited;
end;

function TdxCustomImageComboBox.GetImageIndex(Index : Integer) : Integer;
Var
  St : String;
begin
  Result := -1;
  if(Index < FStrings.Count) then begin
    St := FStrings[Index];
    if(Pos(',', St) > 0) then begin
      St := Copy(St, 1, Pos(',', St) - 1);
      if(St <> '') then
        Result := StrToInt(St);
    end;
  end;
end;

function TdxCustomImageComboBox.GetValue(INdex : Integer) : String;
begin
  Result := '';
  if(Index < FStrings.Count) And (Pos(',', FStrings[Index]) > 0) then
    Result := Copy(FStrings[Index], Pos(',', FStrings[Index]) + 1, 1000);
end;

procedure TdxCustomImageComboBox.SetImageIndex(Index : Integer; Value : Integer);
Var
  St : String;
begin
  if(Index < FStrings.Count)
  And (Value <> ImageIndexes[Index])then begin
    St := Values[Index];
    FStrings[Index] := IntToStr(Value) + ',' + St;
  end;
end;

procedure TdxCustomImageComboBox.SetAlignment(Value : TAlignment);
begin
  if(Value <> FAlignment) then
    FAlignment := Value;
end;

procedure TdxCustomImageComboBox.SetVertAlignment(Value : TVertAlignment);
begin
  if(Value <> FVertAlignment) then
    FVertAlignment := Value;
end;

procedure TdxCustomImageComboBox.SetImageAlign(Value : TdxImageAlign);
begin
  if(Value <> FImageAlign) then
    FImageAlign := Value;
end;

procedure TdxCustomImageComboBox.SetImageList(Value : TCustomImageList);
begin
  if(Value <> FImageList) then begin
    if(FImageList <> Nil) then
      FImageList.UnRegisterChanges(FChangeLink);
    FImageList := Value;
    if(FImageList <> Nil) then
      FImageList.RegisterChanges(FChangeLink);
    SetInheritedItemHeight;
  end;
end;

procedure TdxCustomImageComboBox.SetInternalItemHeight(Value : Integer);
begin
  if(Value <> FItemHeight) then begin
    if(Value < 10) then
      FItemHeight := 0
    else FItemHeight := Value;
    SetInheritedItemHeight;
  end;
end;

procedure TdxCustomImageComboBox.SetMultiLines(Value : Boolean);
begin
  if(Value <> FMultiLines) then
    FMultiLines := Value;
end;

procedure TdxCustomImageComboBox.SetValue(Index : Integer; const Value : String);
Var
  St : String;
begin
  if(Index < FStrings.Count) And (Index > -1)
  And (Value <> Values[Index])then begin
    St := IntToStr(ImageIndexes[Index]);
    FStrings[Index] := St + ',' + Value;
  end;
end;

procedure TdxCustomImageComboBox.AddItem(St :String; ImageIndex : Integer);
begin
  Items.Add(St);
  SetImageIndex(Items.Count -1, ImageIndex);
end;

procedure TdxCustomImageComboBox.InsertItem(Index : Integer;
          St :String; ImageIndex : Integer);
begin
  if(Index < 0) then Index := 0;
  if(Index >= Items.Count) then
    AddItem(St, ImageIndex)
  else begin
    Items.Insert(Index, St);
    SetImageIndex(Index, ImageIndex);
  end;
end;

procedure TdxCustomImageComboBox.ExchangeItems(Index1, Index2 : Integer);
var
  flag : Boolean;
  St1, St2 : string;
begin
  flag := (Index1 > -1 ) And (Index1 < Items.Count)
           And (Index2 > -1 ) And (Index2 < Items.Count);
  if(flag) then begin
    St1 := FStrings[Index1];
    St2 := FStrings[Index2];
  end;
  Items.Exchange(Index1, Index2);
  if(flag) then begin
    FStrings[Index1] := St2;
    FStrings[Index2] := St1;
  end;
end;

procedure TdxCustomImageComboBox.MoveItem(CurIndex, NewIndex: Integer);
Var
  TempString: string;
begin
  if (CurIndex <> NewIndex) And (CurIndex > -1) And (CurIndex < Items.Count) then begin
    TempString := FStrings[CurIndex];
    Items.Move(CurIndex, NewIndex);
    FStrings[NewIndex] := TempString;
  end;
end;

procedure TdxCustomImageComboBox.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('SaveStrings', StringsRead, StringsWrite, True);
end;

procedure TdxCustomImageComboBox.StringsRead(Reader: TReader);
begin
  Reader.ReadListBegin;
  FStrings.Clear;
  while not Reader.EndOfList do
    FStrings.Add(Reader.ReadString);
  Reader.ReadListEnd;
end;

procedure TdxCustomImageComboBox.StringsWrite(Writer: TWriter);
var
  i: Integer;
begin
  Writer.WriteListBegin;
  for i := 0 to FStrings.Count - 1 do
    Writer.WriteString(FStrings[I]);
  Writer.WriteListEnd;
end;

procedure TdxCustomImageComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  CHorizontalAlignFlags: array [TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
  CVerticalAlignFlags: array [TVertAlignment] of Integer = (DT_TOP, DT_VCENTER, DT_BOTTOM);
var
  AImage: TBitmap;
  AImageAlign: TdxImageAlign;
  ADrawFlags, AImageWidth: Integer;
  R: TRect;
begin
  Canvas.FillRect(Rect);
  AImageAlign := FImageAlign;
  if UseRightToLeftAlignment then
  begin
    if AImageAlign = dxliLeft then
      AImageAlign := dxliRight
    else
      AImageAlign := dxliLeft;
  end;
  if FImageList <> nil then
  begin
    R.Top := Rect.Top + 1;
    R.Bottom := Rect.Bottom - 1;
    AImageWidth := ((R.Bottom - R.Top) * FImageList.Width) div FImageList.Height;
    if AImageAlign = dxliLeft then
    begin
      R.Left := Rect.Left - 1 ;
      R.Right := R.Left + AImageWidth + 2;
    end
    else
    begin
      R.Right := Rect.Right - 1;
      R.Left := R.Right - 2 - AImageWidth;
    end;
  end;
  if (FImageList <> nil) and (ImageIndexes[Index] > -1) and (ImageIndexes[Index] < FImageList.Count) then
  begin
    if (Index = ItemIndex) and (Canvas.Brush.Color <> Color) then
      DrawEdge(Canvas.Handle, R, EDGE_RAISED, BF_RECT);
    InflateRect(R, -1, 0);
    AImage := TBitmap.Create;
    try
      FImageList.GetBitmap(ImageIndexes[Index], AImage);
      Canvas.StretchDraw(R, AImage);
    finally
      AImage.Free;
    end;
  end;
  Inc(Rect.Top);
  Dec(Rect.Bottom);
  if FImageList <> nil then
  begin
    if AImageAlign = dxliLeft then
      Rect.Left := R.Right
    else
      Rect.Right := R.Left;
  end;
  Inc(Rect.Left);
  Dec(Rect.Right);
  SetBkMode(Canvas.Handle, TRANSPARENT);
  if Assigned(FOnDrawItem) then
    FOnDrawItem(Self, Index, Rect)
  else
  begin
    ADrawFlags := CHorizontalAlignFlags[Alignment];
    if UseRightToLeftAlignment then
      case ADrawFlags of
        DT_LEFT:
          ADrawFlags := DT_RIGHT;
        DT_RIGHT:
          ADrawFlags := DT_LEFT;
      end;
    ADrawFlags := ADrawFlags or DT_EXPANDTABS or DT_NOPREFIX or DT_EDITCONTROL;
    if FMultiLines then
      ADrawFlags := ADrawFlags or DT_WORDBREAK
    else
      ADrawFlags := ADrawFlags or CVerticalAlignFlags[VertAlignment] or DT_SINGLELINE;
    if UseRightToLeftReading then
      ADrawFlags := ADrawFlags or DT_RTLREADING;
    DrawText(Canvas.Handle, PChar(Items[Index]), Length(Items[Index]), Rect, ADrawFlags);
  end;
end;

procedure TdxCustomImageComboBox.SetInheritedItemHeight;
Var
  h : Integer;
begin
  if(FItemHeight < 10) then begin
    h := Font.Height;
    if(FImageList <> NIl) And (h < FImageList.Height) then
      h := FImageList.Height;
    Inc(h, 2);
  end else h := FItemHeight;
  if(h <> inherited ItemHeight) then
    inherited ItemHeight := h;
  RecreateWnd;
end;

procedure TdxCustomImageComboBox.OnChangeLink(Sender : TObject);
begin
  SetInheritedItemHeight;
end;

procedure TdxCustomImageComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  SetInheritedItemHeight;
end;

procedure TdxCustomImageComboBox.WndProc(var Message : TMessage);
var
  AIndex: Integer;
begin
  with Message do
    case Msg of
      CB_INSERTSTRING:
      begin
        AIndex := wParam;
        if (FDeletedIndex = AIndex) and (AIndex <> -1) then
          FStrings.Insert(AIndex, FDeletedSt)
        else begin
          FStrings.Insert(AIndex, '');
          ImageIndexes[AIndex] := -1;
        end;
      end;
      CB_ADDSTRING:
      begin
        FStrings.Add('');
        ImageIndexes[FStrings.Count - 1] := -1;
      end;
      CB_DELETESTRING:
      begin
       FDeletedIndex := wParam;
       FDeletedSt := FStrings[wParam];
       FStrings.Delete(wParam);
      end;
      else FDeletedIndex := -1;
    end;
  inherited;
end;

function TdxCustomImageComboBox.ValuesIndexOf(Text : String) : Integer;
begin
  for Result := 0 to FStrings.Count - 1 do
    if AnsiCompareText(Text, Values[Result]) = 0 then Exit;
  Result := -1;
end;

{TdxSpinImageItem}
constructor TdxSpinImageItem.Create(Collection : TCollection);
begin
  inherited Create(Collection);
  Owner := TdxSpinImageItems(Collection);
  FImageIndex := -1;
end;

procedure TdxSpinImageItem.Assign(Source: TPersistent);
begin
  if Source is TdxSpinImageItem then begin
    ImageIndex := TdxSpinImageItem(Source).ImageIndex;
    Value := TdxSpinImageItem(Source).Value;
    Hint := TdxSpinImageItem(Source).Hint;
  end;
end;

procedure TdxSpinImageItem.SetImageIndex(Value : Integer);
begin
  if(FImageIndex <> Value) then begin
    FImageIndex := Value;
    if(Owner.Owner <> Nil) then
      Owner.Owner.UpdateItems;
  end;
end;

procedure TdxSpinImageItem.SetValue(Value : String);
begin
  if(FValue <> Value) then begin
    FValue := Value;
    if(Owner.Owner <> Nil) then
      Owner.Owner.UpdateItems;
  end;
end;

procedure TdxSpinImageItem.SetHint(Value : String);
begin
  if(FHint <> Value) then
    FHint := Value;
end;

{TdxSpinImageItems}
constructor TdxSpinImageItems.Create(AOwner : TdxCustomSpinImage);
begin
  inherited Create(TdxSpinImageItem);
  Owner := AOwner;
end;

function TdxSpinImageItems.Add : TdxSpinImageItem;
begin
  Result := TdxSpinImageItem(inherited Add);
end;

function TdxSpinImageItems.IndexOf(Value : String) : Integer;
begin
  for Result := 0 to Count - 1 do
    if (CompareText(Value, Items[Result].Value) = 0) then
      exit;
  Result := -1;
end;

function TdxSpinImageItems.GetItem(Index : Integer) : TdxSpinImageItem;
begin
  Result := TdxSpinImageItem(inherited Items[Index]);
end;

procedure TdxSpinImageItems.SetItem(Index: Integer; Value: TdxSpinImageItem);
begin
  Items[Index].Assign(Value);
end;

procedure TdxSpinImageItems.Update(Item: TCollectionItem);
begin
  if(Item <> Nil) And (Owner <> Nil) then
    Owner.UpdateItems;
end;

{TdxCustomSpinImage}
constructor TdxCustomSpinImage.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed, csOpaque];

  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := OnChangeLink;
  FItems := TdxSpinImageItems.Create(self);

  TabStop := True;
  FBorderStyle := bsSingle;
  FItemIndex := -1;
  FUpDownAlign := udaRight;
  FUpDownWidth := 16;
  FUpDownOrientation := siVertical;
  FImageHAlign := hsiCenter;
  FImageVAlign := vsiCenter;
  FStretch := True;
  FDefaultImages := True;
  Height := 100;
  Width := 118;
  FUseDblClick := True;
  FNCSide := 0;

  FTimer := TTimer.Create(self);
  FTimer.Enabled := False;
  FTimer.Interval := 300;
  FTimer.OnTimer := DoTimerScroll;
end;

destructor TdxCustomSpinImage.Destroy;
begin
  FTimer.Free;
  FTimer := nil;
  FItems.Free;
  FChangeLink.Free;
  inherited Destroy;
end;

procedure TdxCustomSpinImage.CreateWnd;
begin
  inherited CreateWnd;
  UpdateNCRegion;
end;

procedure TdxCustomSpinImage.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_TABSTOP;
  if FBorderStyle = bsSingle then
    Params.Style := Params.Style or WS_BORDER;
end;

procedure TdxCustomSpinImage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FImageList <> nil) and
    (AComponent = FImageList) then ImageList := nil;
end;

procedure TdxCustomSpinImage.SetInternalAutoSize(Value : Boolean);
begin
  if(Value <> FAutoSize) And (FImageList <> Nil) then begin
    FAutoSize := Value;
    if(FAutoSize) then
      MakeAutoSize;
  end;
end;

procedure TdxCustomSpinImage.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TdxCustomSpinImage.SetDefaultImages(Value : Boolean);
begin
  if(Value <> FDefaultImages) then begin
    FDefaultImages := Value;
    if Not (csLoading in ComponentState) then begin
      if(Value And (FImageList <> Nil) And (FImageList.Count > 0))
      Or (Not Value And (Items.Count > 0)) then
        ItemIndex := 0
      else ItemIndex := -1;
      UpdateItems;
      Repaint;
    end;
  end;
end;

procedure TdxCustomSpinImage.SetItemIndex(Value : Integer);
begin
  if (csLoading in ComponentState) then begin
    FItemIndex := Value;
    Change;
    exit;
  end;
  if(Value >= -1) And (FItemIndex <> Value) And (FImageList <> Nil)
  And ((FDefaultImages And (Value < FImageList.Count))
   Or (Not FDefaultImages And (Value < Items.Count)))then begin
    FItemIndex := Value;
    Change;
    Paint;
  end;
end;

procedure TdxCustomSpinImage.SetImageList(Value : TCustomImageList);
begin
  if(Value <> FImageList) then begin
    if(FImageList <> Nil) then
      FImageList.UnRegisterChanges(FChangeLink);
    FImageList := Value;
    if(FImageList <> Nil) then
      FImageList.RegisterChanges(FChangeLink);
    if Not (csLoading in ComponentState) then
      ItemIndex := -1;
    Paint;
  end;
end;

procedure TdxCustomSpinImage.SetImageHAlign(Value : TdxHSpinImageAlign);
begin
  if(FImageHAlign <> Value) then begin
    FImageHAlign := Value;
    Repaint;
  end;
end;

procedure TdxCustomSpinImage.SetImageVAlign(Value : TdxVSpinImageAlign);
begin
  if(FImageVAlign <> Value) then begin
    FImageVAlign := Value;
    Repaint;
  end;
end;

procedure TdxCustomSpinImage.SetItems(Value : TdxSpinImageItems);
begin
  FItems.Assign(Value);
  Update;
  Repaint;
end;

procedure TdxCustomSpinImage.SetStretch(Value : Boolean);
begin
  if(FStretch <> Value) then begin
    FStretch := Value;
    Repaint;
  end;
end;

procedure TdxCustomSpinImage.SetUpDownAlign(Value : TdxUpDownAlign);
begin
  if(Value <> FUpDownAlign) then begin
    FUpDownAlign := Value;
    UpdateNCRegion;
  end;
end;

procedure TdxCustomSpinImage.SetUpDownOrientation(Value : TdxsiOrientation);
begin
  if(FUpDownOrientation <> Value) then begin
    FUpDownOrientation := Value;
    UpdateNCRegion;
  end;
end;

procedure TdxCustomSpinImage.SetUpDownWidth(Value : Integer);
begin
  if(FUpDownWidth <> Value) then begin
    FUpDownWidth := Value;
    UpdateNCRegion;
  end;
  if(FAutoSize) then
    MakeAutoSize;
end;

procedure TdxCustomSpinImage.CMEnter(var Message: TCMEnter);
begin
  Invalidate;
  inherited;
end;

procedure TdxCustomSpinImage.CMExit(var Message: TCMExit);
begin
  Invalidate;
  inherited;
end;

procedure TdxCustomSpinImage.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Perform(WM_NCPAINT, 0, 0);
end;

procedure TdxCustomSpinImage.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if TabStop and CanFocus then SetFocus;
  inherited;
end;

procedure TdxCustomSpinImage.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if FUseDblClick then
    SetNextItem
  else inherited;
end;

procedure TdxCustomSpinImage.UpDownClick(AKey : Word);
begin
  if(Not Focused) then
    SetFocus;
  if not CanChange then exit;

  case AKey of
    VK_DOWN, VK_RIGHT, VK_END:
    begin
      if IsLastItem then exit;
      FDownPress := True;
      FUpPress := False;
      if (AKey = VK_END) then begin
        if(FDefaultImages) then
          ItemIndex := FImageList.Count - 1
        else ItemIndex := Items.Count - 1;
      end else
        if not IsLastItem then
          ItemIndex := ItemIndex + 1;
      Perform(WM_NCPAINT, 0, 0);
    end;
    VK_UP, VK_LEFT, VK_HOME:
    begin
      FUpPress := True;
      FDownPress := False;
      if ItemIndex = 0 then exit;
      if(FDefaultImages and (FImageList.Count > 0))
      or (not FDefaultImages and (Items.Count > 0)) then
      begin
        if (AKey = VK_HOME) then
          ItemIndex := 0
       else
         if (ItemIndex > 0) then
           ItemIndex := ItemIndex - 1;
      end;
      Perform(WM_NCPAINT, 0, 0);
    end;
    VK_SPACE:
    begin
      SetNextItem;
      Perform(WM_NCPAINT, 0, 0);
    end;
 end;
end;

procedure TdxCustomSpinImage.MakeAutoSize;
Var
  R : TRect;
  FWidth, FHeight : Integer;
begin
  R := ClientRect;
  FWidth := R.Right - R.Left;
  FHeight := R.Bottom - R.Top;
  Width := Width + FImageList.Width - FWidth;
  Height := Height + FImageList.Height - FHeight;
end;

function TdxCustomSpinImage.IsLastItem : Boolean;
begin
  Result := not ( FDefaultImages and (FItemIndex < FImageList.Count - 1))
    Or (Not FDefaultImages and (FItemIndex < Items.Count - 1));
end;

procedure TdxCustomSpinImage.SetNextItem;
begin
  if CanChange And ((FDefaultImages And (FImageList.Count > 0))
  Or(Not FDefaultImages And (Items.Count >0))) then begin
    if not IsLastItem then
      ItemIndex := FItemIndex + 1
    else ItemIndex := 0;
  end;
end;

procedure TdxCustomSpinImage.UpdateNCRegion;
begin
  if HandleAllocated then
    dxRecalculateNonClientPart(Handle);
end;

procedure TdxCustomSpinImage.OnChangeLink(Sender : TObject);
begin
  if(FAutoSize) then MakeAutoSize;
  Repaint;
end;

procedure TdxCustomSpinImage.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TdxCustomSpinImage.WMNCCalcSize(var Message : TWMNCCalcSize);
var
  AUpDownAlign : TdxUpDownAlign;
  r : TRect;
begin
  inherited;
  GetWindowRect(Handle, r);
  AUpDownAlign := FUpDownAlign;
  if UseRightToLeftAlignment then
    case AUpDownAlign of
      udaLeft:
        AUpDownAlign := udaRight;
      udaRight:
        AUpDownAlign := udaLeft;
    end;
  with Message.CalcSize_Params^.rgrc[0] do
  begin
    if Left > r.Left then
      FNCSide := Left - r.Left;
    case AUpDownAlign of
      udaBottom:
        Dec(Bottom, FUpDownWidth);
      udaLeft:
        Inc(Left, FUpDownWidth);
      udaRight:
        Dec(Right, FUpDownWidth);
      udaTop:
        Inc(Top, FUpDownWidth);
    end;
  end;
end;

procedure TdxCustomSpinImage.WMNCPaint(var Message : TWMNCPaint);
var
  AUpDownAlign : TdxUpDownAlign;
  DC : HDC;
  r : TRect;
  rgn, rgn1 : HRGN;
const
  EnableFlag : Array[False..True] of Integer = (DFCS_INACTIVE, 0);
  FlatFlag : Array[False..True] of Integer = (DFCS_FLAT, 0);
  PressFlag : Array[False..True] of Integer = (0, DFCS_PUSHED);
  UpLeftFlag : Array[False..True] of Integer = (DFCS_SCROLLLEFT, DFCS_SCROLLUP);
  DownRightFlag : Array[False..True] of Integer = (DFCS_SCROLLRIGHT, DFCS_SCROLLDOWN);
begin
  inherited;
  DC := GetWindowDC(Handle);
  GetWindowRect(Handle, r);
  OffSetRect(r, -r.Left, -r.Top);
  InflateRect(r, -FNCSide, -FNCSide);
  AUpDownAlign := FUpDownAlign;
  if UseRightToLeftAlignment then
    case AUpDownAlign of
      udaLeft:
        AUpDownAlign := udaRight;
      udaRight:
        AUpDownAlign := udaLeft;
    end;
  case AUpDownAlign of
    udaBottom: r.Top := r.Bottom - FUpDownWidth;
    udaLeft: r.Right := r.Left + FUpDownWidth;
    udaRight: r.Left := r.Right - FUpDownWidth;
    udaTop: r.Bottom := r.Top + FUpDownWidth;
  end;
  FUpButtonRect := r;
  FDownButtonRect := r;
  FUpButtonEnabled := CanChange and (ItemIndex > 0);
  FDownButtonEnabled := CanChange and not IsLastItem;

  if (FUpDownOrientation = siVertical) then
    FUpButtonRect.Bottom := r.Top + (r.Bottom - r.Top) div 2
  else FUpButtonRect.Right := r.Right - (r.Right - r.Left) div 2;
  if (FUpDownOrientation = siVertical) then
    FDownButtonRect.Top := r.Bottom - (r.Bottom - r.Top) div 2
  else FDownButtonRect.Left := r.Left + (r.Right - r.Left) div 2 + 1;
  DrawFrameControl(DC, FUpButtonRect, DFC_SCROLL, UpLeftFlag[FUpDownOrientation = siVertical]
    or EnableFlag[FUpButtonEnabled] or FlatFlag[Ctl3D] or PressFlag[FUpPress and FUpButtonEnabled]);
  DrawFrameControl(DC, FDownButtonRect, DFC_SCROLL, DownRightFlag[FUpDownOrientation = siVertical]
    or EnableFlag[FDownButtonEnabled] or FlatFlag[Ctl3D] or PressFlag[FDownPress and FDownButtonEnabled]);

  rgn := CreateRectRgnIndirect(r);
  rgn1 := CreateRectRgnIndirect(FUpButtonRect);
  CombineRgn(rgn, rgn, rgn1, RGN_XOR);
  DeleteObject(rgn1);
  rgn1 := CreateRectRgnIndirect(FDownButtonRect);
  CombineRgn(rgn, rgn, rgn1, RGN_XOR);
  DeleteObject(rgn1);
  FillRgn(DC, rgn, Canvas.Brush.Handle);
  DeleteObject(rgn);
  ReleaseDC(Handle, DC);
end;

procedure TdxCustomSpinImage.DoTimerScroll(Sender: TObject);
Var
  p : TPoint;
begin
  if(FUpPress or FDownPress) then
  begin
    if FScrollTimerCount > 1 then
    begin
      GetCursorPos(p);
      p := ScreenToClient(p);
      if FUpPress then
      begin
        if PtInRect(FUpButtonRect, Point(p.X, p.Y)) then
          UpDownClick(VK_UP)
         else FScrollTimerCount := 0;
      end else
        if PtInRect(FDownButtonRect, Point(p.X, p.Y)) then
          UpDownClick(VK_DOWN)
        else FScrollTimerCount := 0;
    end else Inc(FScrollTimerCount);
  end
  else
  begin
    FTimer.Enabled := False;
  end;
end;

procedure TdxCustomSpinImage.NCMouseDown(X, Y : Integer);
var
  r : TRect;
begin
  GetWindowRect(Handle, r);
  Dec(X, r.Left);
  Dec(Y, r.Top);
  if PtInRect(FUpButtonRect, Point(X, Y)) then
    UpDownClick(VK_UP);
  if PtInRect(FDownButtonRect, Point(X, Y)) then
    UpDownClick(VK_DOWN);
  if FUpPress or FDownPress then
  begin
    SetCapture(Handle);
    FScrollTimerCount := 0;
    FTimer.Enabled := True;
  end;
end;

procedure TdxCustomSpinImage.WMNCLButtonDblClk(var Message : TWMNCLBUTTONDOWN);
begin
  with Message do
    NCMouseDown(XCursor, YCursor);
  inherited;
end;

procedure TdxCustomSpinImage.WMNCMouseDown(var Message : TWMNCLBUTTONDOWN);
begin
  with Message do
    NCMouseDown(XCursor, YCursor);
end;

procedure TdxCustomSpinImage.WMNCMouseUp(var Message : TWMNCLBUTTONUP);
begin
  if (GetCapture = Handle) then
  begin
    ReleaseCapture;
    FUpPress := False;
    FDownPress := False;
    Perform(WM_NCPAINT, 0, 0);
  end;
  inherited;
end;

procedure TdxCustomSpinImage.WMMouseUp(var Message : TWMLBUTTONUP);
begin
  if (GetCapture = Handle) then
  begin
    ReleaseCapture;
    FUpPress := False;
    FDownPress := False;
    Perform(WM_NCPAINT, 0, 0);
  end;
  inherited;
end;

procedure TdxCustomSpinImage.WMNCHitTest(var Message : TWMNCHITTEST);
begin
  inherited;
  with Message do
    if(Result = HTNOWHERE) then
      Result := HTBORDER;
end;

procedure TdxCustomSpinImage.KeyDown(var Key: Word; Shift: TShiftState);
begin
  UpDownClick(Key);
  inherited;
end;

procedure TdxCustomSpinImage.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if(FImageList <> NIl) And (FImageList.Count > 0) then
    case Key of
      VK_UP, VK_LEFT, VK_HOME:
      begin
        FUpPress := False;
        Perform(WM_NCPAINT, 0, 0);
      end;
      VK_DOWN, VK_RIGHT, VK_END:
      begin
        FDownPress := False;
        Perform(WM_NCPAINT, 0, 0);
      end;
   end;
   inherited;
end;

procedure TdxCustomSpinImage.Paint;
var
  AImageHAlign : TdxHSpinImageAlign;
  Image : TBitmap;
  R: TRect;
  FLeft, FTop : Integer;
begin
  with Canvas do  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    R := ClientRect;
    if (FImageList <> Nil) And (FItemIndex <> -1) then  begin
      Image := TBitmap.Create;
      if FDefaultImages then
        FImageList.GetBitmap(ItemIndex, Image)
      else FImageList.GetBitmap(Items[ItemIndex].ImageIndex, Image);
      if(FStretch) then
        StretchDraw(r, Image)
      else begin
        FillRect(R);
        FLeft := R.Left;
        FTop := R.Top;
        if(R.Right - R.Left > FImageList.Width) then
        begin
          AImageHAlign := FImageHAlign;
          if UseRightToLeftAlignment then
            case AImageHAlign of
              hsiLeft:
                AImageHAlign := hsiRight;
              hsiRight:
                AImageHAlign := hsiLeft;
            end;
          case AImageHAlign of
            hsiCenter: Inc(FLeft, (R.Right - R.Left - FImageList.Width) div 2);
            hsiRight: Inc(FLeft, R.Right - R.Left - FImageList.Width);
          end;
        end;
        if(R.Bottom - R.Top > FImageList.Height) then
          case FImageVAlign of
            vsiCenter: Inc(FTop, (R.Bottom - R.Top - FImageList.Height) div 2);
            vsiBottom: Inc(FTop, R.Bottom - R.Top - FImageList.Height);
          end;
        Draw(FLeft, FTop, Image);
      end;
      Image.Free;
    end
    else begin
      FillRect(R);
    end;
    if (GetParentForm(Self) <> nil) and (GetParentForm(Self).ActiveControl = Self) and
      not (csDesigning in ComponentState) then  begin
      Brush.Color := clWindowFrame;
      FrameRect(R);
    end;
  end;
end;

function TdxCustomSpinImage.CanChange : Boolean;
begin
  Result := Enabled and not FReadOnly and (FImageList <> nil) and (FImageList.Count > 0);
end;

procedure TdxCustomSpinImage.Change;
begin
  if Not FDefaultImages And (FItemIndex > -1)
  And Not (csDesigning in ComponentState) then begin
    Hint := Items[FItemIndex].Hint;
    if(Hint <> '') then
      ShowHint := True
    else ShowHint := False;
  end;
  if Assigned(FOnChange) then
    FOnChange(self, FItemIndex);
end;

procedure TdxCustomSpinImage.UpdateItems;
begin
end;

{$IFDEF DELPHIBERLIN}
procedure TdxCustomSpinImage.ChangeScale(M, D: Integer; isDpiChange: Boolean);
{$ELSE}
procedure TdxCustomSpinImage.ChangeScale(M, D: Integer);
{$ENDIF}
begin
  inherited ChangeScale(M, D{$IFDEF DELPHIBERLIN}, isDPIChange{$ENDIF});
  RecreateWnd;
end;

end.
