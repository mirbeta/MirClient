{***************************************************************************}
{ TMS W7 Controls Pack                                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2011                                               } 
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit W7ListViewItems;

interface

{$I TMSDEFS.INC}

uses
  Windows,  Classes, StdCtrls, Controls, Graphics, W7Classes, W7Common, W7Graphics,
  W7Labels, W7Images, ImgList, CommCtrl, Messages;

type
  TW7ListViewItemMode = (imSmall, imLarge, imCategory);

  TW7MultiTaskItemNotify = procedure(Sender: TObject; ItemIndex: integer) of object;

  TW7MultiTaskItemMouseEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer; ItemIndex: integer) of object;
  TW7MultiTaskMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState;
    X, Y: Integer; ItemIndex: integer) of object;

  TW7CustomListViewItem = class(TW7TransparentControl)
  private
    FSetFocusOnClick: boolean;
    FInternalBitmap: TBitmap;
    FMouseInControl: boolean;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    ReturnButtonPressed: boolean;
    procedure DrawFocusedBorder;
    procedure DrawStandartBorder;
    procedure DrawBorder;
    procedure DrawFocusedBody;
    procedure DrawStandartBody;
    procedure DrawBody;
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
{    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SetFocusOnClick: boolean read FSetFocusOnClick write FSetFocusOnClick default True;
    property MouseInControl: boolean read FMouseInControl write FMouseInControl default False;
  end;

  TW7CustomMultiTaskItem = class(TW7CustomListViewItem)
  private
    FSubItems: TStrings;
    FLabels: array of TW7ActiveLabel;
    FCaptionLabel: TW7ActiveLabel;
    FFont: TFont;
    FSubItemsFont: TFont;
    FCaptionMouseInColor: TColor;
    FCaptionMouseOutColor: TColor;
    FSubItemsMouseInColor: TColor;
    FSubItemsMouseOutColor: TColor;
    FMode: TW7ListViewItemMode;
    FIcon: TW7Image;
    FSubItemsAlign: TW7VerticalAlignment;
    FOnItemClick: TW7MultiTaskItemNotify;
    FOnItemDblClick: TW7MultiTaskItemNotify;
    FOnItemMouseDown: TW7MultiTaskItemMouseEvent;
    FOnItemMouseEnter: TW7MultiTaskItemNotify;
    FOnItemMouseLeave: TW7MultiTaskItemNotify;
    FOnItemMouseMove: TW7MultiTaskMouseMoveEvent;
    FOnItemMouseUp: TW7MultiTaskItemMouseEvent;
    procedure SetSubItems(Value: TStrings);
    procedure SetFont(Value: TFont);
    procedure SetSubItemsFont(Value: TFont);
    procedure SetCaptionMouseInColor(Value: TColor);
    procedure SetCaptionMouseOutColor(Value: TColor);
    procedure SetSubItemsMouseInColor(Value: TColor);
    procedure SetSubItemsMouseOutColor(Value: TColor);
    procedure SetMode(Value: TW7ListViewItemMode);
    procedure SetIcon(Value: TW7Image);
    procedure SetSubItemsAlign(Value: TW7VerticalAlignment);
  protected
    VerticalDelta: integer;
    HorizontalDelta: integer;
    FirstUpdate: boolean;
    procedure NeedToUpdate;
    procedure ObjectsMouseEnter(Sender: TObject);
    procedure ObjectsMouseLeave(Sender: TObject);
    procedure ObjectsClick(Sender: TObject);
    procedure ObjectsDblClick(Sender: TObject);
    procedure ObjectsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ObjectsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ObjectsMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Caption;
    property SubItems: TStrings read FSubItems write SetSubItems;
    property Font: TFont read FFont write SetFont;
    property ItemsFont: TFont read FSubItemsFont write SetSubItemsFont;
    property CaptionMouseInColor: TColor read FCaptionMouseInColor write SetCaptionMouseInColor nodefault;
    property CaptionMouseOutColor: TColor read FCaptionMouseOutColor write SetCaptionMouseOutColor nodefault;
    property SubItemsMouseInColor: TColor read FSubItemsMouseInColor write SetSubItemsMouseInColor nodefault;
    property SubItemsMouseOutColor: TColor read FSubItemsMouseOutColor write SetSubItemsMouseOutColor nodefault;
    property Mode: TW7ListViewItemMode read FMode write SetMode default imLarge;
    property Icon: TW7Image read FIcon write SetIcon;
    property SubItemsAlign: TW7VerticalAlignment read FSubItemsAlign write SetSubItemsAlign default wvaTop;
    property OnItemClick: TW7MultiTaskItemNotify read FOnItemClick write FOnItemClick;
    property OnItemDblClick: TW7MultiTaskItemNotify read FOnItemDblClick write FOnItemDblClick;
    property OnItemMouseDown: TW7MultiTaskItemMouseEvent read FOnItemMouseDown write FOnItemMouseDown;
    property OnItemMouseEnter: TW7MultiTaskItemNotify read FOnItemMouseEnter write FOnItemMouseEnter;
    property OnItemMouseLeave: TW7MultiTaskItemNotify read FOnItemMouseLeave write FOnItemMouseLeave;
    property OnItemMouseMove: TW7MultiTaskMouseMoveEvent read FOnItemMouseMove write FOnItemMouseMove;
    property OnItemMouseUp: TW7MultiTaskItemMouseEvent read FOnItemMouseUp write FOnItemMouseUp;
  end;

  TW7SubItemsList = class (TStrings)
  private
    ListViewItem: TW7CustomMultiTaskItem;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

  TW7CustomTaskItem = class (TW7CustomListViewItem)
  private
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FImageIndex: integer;
    FIconSize: TW7ButtonIconSize;
    FIconHeight: integer;
    FDescription: TCaption;
    FIconAlignment: TW7VerticalAlignment;
    FDescriptionFont: TFont;
    FDescriptionAlignment: TW7VerticalAlignment;
    FVerticalDelta: integer;
    FHorizontalDelta: integer;
    procedure SetImages(Value: TCustomImageList);
    procedure SetImageIndex(Value: integer);
    procedure SetIconSize(Value: TW7ButtonIconSize);
    procedure SetDescription(Value: TCaption);
    procedure SetIconAlignment(Value: TW7VerticalAlignment);
    procedure SetDescriptionFont(Value: TFont);
    procedure SetDescriptionAlignment(Value: TW7VerticalAlignment);
    procedure SetVerticalDelta(Value: integer);
    procedure SetHorizontalDelta(Value: integer);
  protected
    procedure DrawButtonElements;
    procedure Paint; override;
    procedure ImageListChange(Sender: TObject);
    procedure UpdateImageList;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property IconSize: TW7ButtonIconSize read FIconSize write SetIconSize default is48px;
    property Caption;
    property Description: TCaption read FDescription write SetDescription;
    property IconAlignment: TW7VerticalAlignment read FIconAlignment write SetIconAlignment default wvaTop;
    property Font;
    property DescriptionFont: TFont read FDescriptionFont write SetDescriptionFont;
    property DescriptionAlignment: TW7VerticalAlignment read FDescriptionAlignment write SetDescriptionAlignment default wvaTop;
    property VerticalDelta: integer read FVerticalDelta write SetVerticalDelta;
    property HorizontalDelta: integer read FVerticalDelta write SetHorizontalDelta;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TW7ListViewItem = class (TW7CustomListViewItem)
  published
    property SetFocusOnClick;
    property Align;
    property Anchors;
    property Font;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    {$IFDEF DELPHI_UNICODE}
    property ParentDoubleBuffered;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property MouseInControl;
    property OnClick;
    property OnDblClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TW7MultiTaskItem = class(TW7CustomMultiTaskItem)
  published
    property Caption;
    property SubItems;
    property Font;
    property ItemsFont;
    property CaptionMouseInColor;
    property CaptionMouseOutColor;
    property SubItemsMouseInColor;
    property SubItemsMouseOutColor;
    property Mode;
    property Icon;
    property SubItemsAlign;
    property Action;
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnItemClick;
    property OnItemDblClick;
    property OnItemMouseDown;
    property OnItemMouseEnter;
    property OnItemMouseLeave;
    property OnItemMouseMove;
    property OnItemMouseUp;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TW7TaskItem = class (TW7CustomTaskItem)
  published
    property Action;
    property Images;
    property ImageIndex;
    property IconSize;
    property Caption;
    property Description;
    property IconAlignment;
    property Font;
    property DescriptionFont;
    property DescriptionAlignment;
    property VerticalDelta;
    property HorizontalDelta;
    property Align;
    property Anchors;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    {$IFDEF DELPHI_UNICODE}
    property ParentDoubleBuffered;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

procedure TW7CustomListViewItem.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
end;

constructor TW7CustomListViewItem.Create(AOwner: TComponent);
begin
  inherited;
  Width := 240;
  Height := 50;
  FMouseInControl := False;
  FInternalBitmap := TBitmap.Create;
  with FInternalBitmap do
  begin
    Width := 250;
    Height := 52;
    PixelFormat := pf24bit;
    Canvas.Brush.Color := clBlack;
    Canvas.FillRect(Rect(0, 0, Width, Height));
  end;
  FSetFocusOnClick := True;
  ReturnButtonPressed := False;
end;

destructor TW7CustomListViewItem.Destroy;
begin
  FInternalBitmap.Destroy;
  inherited;
end;

procedure TW7CustomListViewItem.DrawFocusedBorder;
begin
  with FInternalBitmap do
  begin
    Canvas.Pen.Style := psSolid;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := $00A75614;
    Canvas.RoundRect(0, 0, Width, Height, 4, 4);

    Canvas.Pen.Style := psSolid;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := $00F2EBE0;
    Canvas.RoundRect(1, 1, Width - 1, Height - 1, 4, 4);

    Canvas.Pixels[0, 1] := $00CC9670;
    Canvas.Pixels[1, 1] := $00CC9670;
    Canvas.Pixels[1, 0] := $00CC9670;

    Canvas.Pixels[Width - 1, 1] := $00CC9670;
    Canvas.Pixels[Width - 2, 1] := $00CC9670;
    Canvas.Pixels[Width - 2, 0] := $00CC9670;

    Canvas.Pixels[0, Height - 2] := $00CC9670;
    Canvas.Pixels[1, Height - 2] := $00CC9670;
    Canvas.Pixels[1, Height - 1] := $00CC9670;

    Canvas.Pixels[Width - 1, Height - 2] := $00CC9670;
    Canvas.Pixels[Width - 2, Height - 2] := $00CC9670;
    Canvas.Pixels[Width - 2, Height - 1] := $00CC9670;
  end;
end;

procedure TW7CustomListViewItem.DrawStandartBorder;
begin
  with FInternalBitmap do
    if MouseInClient or FMouseInControl then
    begin
      Canvas.Pen.Style := psSolid;
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Color := $00F05C00;
      Canvas.RoundRect(0, 0, Width, Height, 4, 4);

      Canvas.Pen.Style := psSolid;
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Color := $00EEEAE7;
      Canvas.RoundRect(1, 1, Width - 1, Height - 1, 4, 4);

      Canvas.Pixels[0, 1] := $00F0A05C;
      Canvas.Pixels[1, 1] := $00F0A05C;
      Canvas.Pixels[1, 0] := $00F0A05C;

      Canvas.Pixels[Width - 1, 1] := $00F0A05C;
      Canvas.Pixels[Width - 2, 1] := $00F0A05C;
      Canvas.Pixels[Width - 2, 0] := $00F0A05C;

      Canvas.Pixels[0, Height - 2] := $00F0A05C;
      Canvas.Pixels[1, Height - 2] := $00F0A05C;
      Canvas.Pixels[1, Height - 1] := $00F0A05C;

      Canvas.Pixels[Width - 1, Height - 2] := $00F0A05C;
      Canvas.Pixels[Width - 2, Height - 2] := $00F0A05C;
      Canvas.Pixels[Width - 2, Height - 1] := $00F0A05C;
    end;
end;

procedure TW7CustomListViewItem.DrawBorder;
begin
  if Focused then
    DrawFocusedBorder
  else
    DrawStandartBorder;
end;

procedure TW7CustomListViewItem.DrawFocusedBody;
begin
  with FInternalBitmap do
    DrawGradient(Canvas, $00FADCC0, $00FABE8E, Rect(2, 2, Width - 2, Height - 2), True);
end;

procedure TW7CustomListViewItem.DrawStandartBody;
begin
  with FInternalBitmap do
    if MouseInClient then
      DrawGradient(Canvas, $00F8F0E8, $00FEDEB7, Rect(2, 2, Width - 2, Height - 2), True);
end;

procedure TW7CustomListViewItem.DrawBody;
begin
  if Focused then
    DrawFocusedBody
  else
    DrawStandartBody;
end;

procedure TW7CustomListViewItem.Paint;
var
  Opacity: byte;
begin
  inherited;

  if (Width <> FInternalBitmap.Width) or (Height <> FInternalBitmap.Height) then
  begin
    FInternalBitmap.Width := Width;
    FInternalBitmap.Height := Height;
  end;

  BitBlt(FInternalBitmap.Canvas.Handle, 0, 0, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);

  DrawBody;

  DrawBorder;

  if Focused then
    Opacity := 150
  else
    Opacity := 64;

  if csDesigning in ComponentState then
  begin
    FInternalBitmap.Canvas.Brush.Style := bsSolid;
    FInternalBitmap.Canvas.DrawFocusRect(Rect(0, 0, Width, Height));
  end;
  AlphaBlendBitmap(FInternalBitmap, Canvas, Rect(0, 0, Width, Height), Opacity);
end;

procedure TW7CustomListViewItem.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_RETURN) or (Key = VK_SPACE) then
    ReturnButtonPressed := True;

  if FSetFocusOnClick then
    SetFocus;

  Invalidate;
end;

procedure TW7CustomListViewItem.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  ReturnButtonPressed := False;
  Invalidate;
end;

procedure TW7CustomListViewItem.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
//  if FSetFocusOnClick then
//    SetFocus;
  inherited;
end;

/////////////////////////////////

constructor TW7CustomMultiTaskItem.Create(AOwner: TComponent);
begin
  inherited;
  FMode := imLarge;
  FSubItems := TW7SubItemsList.Create;
  TW7SubItemsList(FSubItems).ListViewItem := Self;
  FCaptionLabel := TW7ActiveLabel.Create(Self);
  FCaptionLabel.AutoSize := False;
  FCaptionLabel.Parent := Self;
  FFont := TFont.Create;
  FSubItemsFont := TFont.Create;
  if GetWindowsVersion >= 6 then
  begin
    FFont.Name := W7StandartFontName;
    FFont.Size := 12;
    FSubItemsFont.Name := W7StandartFontName;
    FSubItemsFont.Size := W7StandartFontSize;
  end
  else
  begin
    FFont.Size := 12;
    FSubItemsFont.Size := 8;
  end;
  FSubItemsFont.Color := FSubItemsMouseOutColor;
  FSubItemsMouseInColor := $00FF9933;
  FSubItemsMouseOutColor := $00D56600;

  FCaptionMouseInColor := $001DAE00;
  FCaptionMouseOutColor := $00126E00;
  FCaptionLabel.MouseInColor := $001DAE00;
  FCaptionLabel.MouseOutColor := $00126E00;

  FSubItemsFont.Color := FSubItemsMouseOutColor;
  FFont.Color := FCaptionMouseOutColor;
  VerticalDelta := 4;
  HorizontalDelta := 4;
  SetFocusOnClick := False;
  FirstUpdate := False;
  FIcon := TW7Image.Create(Self);
  FIcon.Parent := Self;
  FIcon.Width := 32;
  FIcon.Height := 32;
  FIcon.Top := Height div 2 - FIcon.Height div 2;
  FIcon.Left := HorizontalDelta;
  FSubItemsAlign := wvaTop;
  FCaptionLabel.Tag := -1;
  with FCaptionLabel do
  begin
    OnMouseEnter := ObjectsMouseEnter;
    OnMouseLeave := ObjectsMouseLeave;
    OnClick := ObjectsClick;
    OnDblClick := ObjectsDblClick;
    OnMouseMove := ObjectsMouseMove;
    OnMouseUp := ObjectsMouseUp;
    OnMouseDown := ObjectsMouseDown;
  end;
  with FIcon do
  begin
    OnMouseEnter := ObjectsMouseEnter;
    OnMouseLeave := ObjectsMouseLeave;
    OnClick := ObjectsClick;
    OnDblClick := ObjectsDblClick;
    OnMouseMove := ObjectsMouseMove;
    OnMouseUp := ObjectsMouseUp;
    OnMouseDown := ObjectsMouseDown;
  end;
  TabStop := False;
end;

destructor TW7CustomMultiTaskItem.Destroy;
var
  Ind: integer;
begin
  FSubItems.Free;
  FCaptionLabel.Free;
  FFont.Free;
  FSubItemsFont.Free;
  FIcon.Free;
  for Ind := 0 to FSubItems.Count - 1 do
    FLabels[Ind].Visible := False;
  SetLength(FLabels, 0);
  inherited;
end;

procedure TW7CustomMultiTaskItem.NeedToUpdate;
var
  Ind: integer;
  HeightsSum: integer;
  FirstTop: integer;
begin
  FCaptionLabel.Font.Assign(Font);
  FCaptionLabel.Font.Color := FCaptionMouseOutColor;
  if FMode = imSmall then
    FCaptionLabel.Left := HorizontalDelta + FIcon.Width + 4
  else
    FCaptionLabel.Left := HorizontalDelta + FIcon.Width + 11;
  FCaptionLabel.AutoSize := True;
  FCaptionLabel.AutoSize := False;
//  FCaptionLabel.Width := (Width - FCaptionLabel.Left - HorizontalDelta - 2);
  case FMode of
    imSmall:
    begin
      FCaptionLabel.Top := Height div 2 - FCaptionLabel.Height div 2;
      FCaptionLabel.Caption := Caption;
      FCaptionLabel.WordWrap := False;
      FCaptionLabel.Layout := tlCenter;
      for Ind := 0 to FSubItems.Count - 1 do
        FLabels[Ind].Visible := False;
      FIcon.Top := Height div 2 - FIcon.Height div 2;
      FIcon.Left := HorizontalDelta;
    end;
    imLarge:
    begin
      FCaptionLabel.Top := Height div 2 - FCaptionLabel.Height div 2;
      FCaptionLabel.Caption := Caption;
      FCaptionLabel.WordWrap := True;
      FCaptionLabel.Layout := tlCenter;
      for Ind := 0 to FSubItems.Count - 1 do
        FLabels[Ind].Visible := False;
      FIcon.Top := Height div 2 - FIcon.Height div 2;
      FIcon.Left := HorizontalDelta;
    end;
    imCategory:
    begin
      FCaptionLabel.WordWrap := False;
      FCaptionLabel.Height := FCaptionLabel.Canvas.TextHeight(Caption);
      FCaptionLabel.Caption := Caption;
      FCaptionLabel.Top := VerticalDelta;
      FCaptionLabel.Layout := tlCenter;
      HeightsSum := 0;
      for Ind := 0 to FSubItems.Count - 1 do
      begin
        FLabels[Ind].AutoSize := True;
        FLabels[Ind].Left := FCaptionLabel.Left;
        if FLabels[Ind].Width > (Width - FLabels[Ind].Left - HorizontalDelta) then
          FLabels[Ind].Width := (Width - FLabels[Ind].Left - HorizontalDelta);
        FLabels[Ind].Height := FLabels[Ind].Canvas.TextHeight(FLabels[Ind].Caption);
        HeightsSum := HeightsSum + FLabels[Ind].Height;
        FLabels[Ind].Visible := True;
        FLabels[Ind].Font.Assign(FSubItemsFont);
      end;
      FirstTop := 0;
      case FSubItemsAlign of
        wvaCenter:
        begin
          FirstTop := FCaptionLabel.Top + FCaptionLabel.Height + VerticalDelta + (Height - (FCaptionLabel.Top + FCaptionLabel.Height + VerticalDelta * 2)) div 2 - (HeightsSum + (VerticalDelta -2) * (FSubItems.Count - 1)) div 2;
        end;
        wvaBottom:
        begin
          FirstTop := Height - VerticalDelta * FSubItems.Count - HeightsSum;
        end;
      end;
      if FirstTop < FCaptionLabel.Top + FCaptionLabel.Height + VerticalDelta then
        FirstTop := FCaptionLabel.Top + FCaptionLabel.Height + VerticalDelta;


      for Ind := 0 to FSubItems.Count - 1 do
      begin
        FLabels[Ind].Top := FirstTop;
        FirstTop := FirstTop + FLabels[Ind].Height + VerticalDelta - 2;
      end;

    end;
  end;
end;

procedure TW7CustomMultiTaskItem.Paint;
begin
  inherited;
  if FCaptionLabel.Caption <> Caption then
  begin
    FCaptionLabel.AutoSize := True;
    FCaptionLabel.AutoSize := False;
    FCaptionLabel.Width := (Width - FCaptionLabel.Left - HorizontalDelta - 2);
{    if FMode = imLarge then
      FCaptionLabel.Top := Height div 2 - FCaptionLabel.Height div 2;}
  end;

  if (not FirstUpdate) then
  begin
    FirstUpdate := True;
    NeedToUpdate;
  end;
end;

procedure TW7CustomMultiTaskItem.Resize;
begin
  NeedToUpdate;
end;

procedure TW7CustomMultiTaskItem.SetSubItems(Value: TStrings);
begin
  FSubItems.Assign(Value);
end;

procedure TW7CustomMultiTaskItem.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  NeedToUpdate;
  FFont.Color := FCaptionMouseOutColor;
  Invalidate;
end;

procedure TW7CustomMultiTaskItem.SetSubItemsFont(Value: TFont);
begin
  FSubItemsFont.Assign(Value);
  NeedToUpdate;
  FSubItemsFont.Color := FSubItemsMouseOutColor;
end;

procedure TW7CustomMultiTaskItem.SetCaptionMouseInColor(Value: TColor);
begin
  FCaptionMouseInColor := Value;
  FCaptionLabel.MouseInColor := Value;
  NeedToUpdate;
end;

procedure TW7CustomMultiTaskItem.SetCaptionMouseOutColor(Value: TColor);
begin
  FCaptionMouseOutColor := Value;
  FCaptionLabel.MouseOutColor := Value;
  FFont.Color := FCaptionMouseOutColor;
  NeedToUpdate;
end;

procedure TW7CustomMultiTaskItem.SetSubItemsMouseInColor(Value: TColor);
var
  Ind: integer;
begin
  FSubItemsMouseInColor := Value;
  for Ind := 0 to FSubItems.Count - 1 do
    FLabels[Ind].MouseInColor := Value;
  NeedToUpdate;
end;

procedure TW7CustomMultiTaskItem.SetSubItemsMouseOutColor(Value: TColor);
var
  Ind: integer;
begin
  FSubItemsMouseOutColor := Value;
  for Ind := 0 to FSubItems.Count - 1 do
    FLabels[Ind].MouseOutColor := Value;
  FSubItemsFont.Color := FSubItemsMouseOutColor;
  NeedToUpdate;
end;

procedure TW7CustomMultiTaskItem.ObjectsMouseEnter(Sender: TObject);
begin
//  MouseInClient := True;
  Cursor := crHandPoint;
  if Assigned(FOnItemMouseEnter) then
  begin
    if Sender is TW7ActiveLabel then
      FOnItemMouseEnter(Sender, TW7ActiveLabel(Sender).Tag)
    else
      FOnItemMouseEnter(Sender, -2);
  end;
  Invalidate;
end;

procedure TW7CustomMultiTaskItem.ObjectsMouseLeave(Sender: TObject);
begin
{  if not MouseInClient then
    MouseInControl := False;}
  Cursor := crDefault;
  if Assigned(FOnItemMouseLeave) then
  begin
    if Sender is TW7ActiveLabel then
      FOnItemMouseLeave(Sender, TW7ActiveLabel(Sender).Tag)
    else
      FOnItemMouseLeave(Sender, -2);
  end;
  Invalidate;
end;

procedure TW7CustomMultiTaskItem.ObjectsClick(Sender: TObject);
begin
  if Assigned(FOnItemClick) then
  begin
    if Sender is TW7ActiveLabel then
      FOnItemClick(Sender, TW7ActiveLabel(Sender).Tag)
    else
      FOnItemClick(Sender, -2);
  end;
end;

procedure TW7CustomMultiTaskItem.ObjectsDblClick(Sender: TObject);
begin
  if Assigned(FOnItemDblClick) then
  begin
    if Sender is TW7ActiveLabel then
      FOnItemDblClick(Sender, TW7ActiveLabel(Sender).Tag)
    else
      FOnItemDblClick(Sender, -2);
  end;
end;

procedure TW7CustomMultiTaskItem.ObjectsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnItemMouseDown) then
  begin
    if Sender is TW7ActiveLabel then
      FOnItemMouseDown(Sender, Button, Shift, X, Y, TW7ActiveLabel(Sender).Tag)
    else
      FOnItemMouseDown(Sender, Button, Shift, X, Y, -2);
  end;
end;

procedure TW7CustomMultiTaskItem.ObjectsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnItemMouseUp) then
  begin
    if Sender is TW7ActiveLabel then
      FOnItemMouseUp(Sender, Button, Shift, X, Y, TW7ActiveLabel(Sender).Tag)
    else
      FOnItemMouseUp(Sender, Button, Shift, X, Y, -2);
  end;
end;

procedure TW7CustomMultiTaskItem.ObjectsMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Assigned(FOnItemMouseMove) then
  begin
    if Sender is TW7ActiveLabel then
      FOnItemMouseMove(Sender, Shift, X, Y, TW7ActiveLabel(Sender).Tag)
    else
      FOnItemMouseMove(Sender, Shift, X, Y, -2);
  end;
end;

procedure TW7CustomMultiTaskItem.SetMode(Value: TW7ListViewItemMode);
begin
  if FMode = Value then
    Exit;
  FMode := Value;
  case FMode of
    imSmall:
    begin
{      if csDesigning in ComponentState then
        Height := 25;}
      VerticalDelta := 2;
      HorizontalDelta := 2;
      FIcon.Width := 16;
      FIcon.Height := 16;
      if GetWindowsVersion >= 6 then
        FFont.Size := 9
      else
        FFont.Size := 8;
      FIcon.Top := Height div 2 - FIcon.Height div 2;
      FIcon.Left := HorizontalDelta;
//      FSubItems.Clear;
    end;
    imLarge:
    begin
{      if csDesigning in ComponentState then
        Height := 50;}
      VerticalDelta := 4;
      HorizontalDelta := 4;
      FIcon.Width := 32;
      FIcon.Height := 32;
      FFont.Size := 12;
      FIcon.Top := Height div 2 - FIcon.Height div 2;
      FIcon.Left := HorizontalDelta;
//      FSubItems.Clear;
    end;
    imCategory:
    begin
{      if csDesigning in ComponentState then
        Height := 54;}
      VerticalDelta := 4;
      HorizontalDelta := 4;
      FIcon.Width := 48;
      FIcon.Height := 48;
      FFont.Size := 12;
      FIcon.Top := VerticalDelta;
      FIcon.Left := HorizontalDelta;
    end;
  end;
  NeedToUpdate;
end;

procedure TW7CustomMultiTaskItem.SetIcon(Value: TW7Image);
begin
  FIcon.Assign(Value);
  NeedToUpdate;
end;

procedure TW7CustomMultiTaskItem.SetSubItemsAlign(Value: TW7VerticalAlignment);
begin
  FSubItemsAlign := Value;
  NeedToUpdate;
end;

//////////////////////////////

function TW7SubItemsList.GetCount: Integer;
begin
  Result := Length(ListViewItem.FLabels);
end;

function TW7SubItemsList.Get(Index: Integer): string;
begin
  Result := '';
  if Index < Length(ListViewItem.FLabels) then
    Result := ListViewItem.FLabels[Index].Caption;
end;

procedure TW7SubItemsList.Put(Index: Integer; const S: string);
begin
  if Index < Length(ListViewItem.FLabels) then
    ListViewItem.FLabels[Index].Caption := S;
end;

procedure TW7SubItemsList.Insert(Index: Integer; const S: string);
var
  Ind: integer;
begin
  if Index > Length(ListViewItem.FLabels) then
    Exit;
  SetLength(ListViewItem.FLabels, Length(ListViewItem.FLabels) + 1);
  ListViewItem.FLabels[Length(ListViewItem.FLabels) - 1] := TW7ActiveLabel.Create(ListViewItem);
  with ListViewItem.FLabels[Length(ListViewItem.FLabels) - 1] do
  begin
    Parent := ListViewItem;
    MouseInColor := ListViewItem.FSubItemsMouseInColor;
    MouseOutColor := ListViewItem.FSubItemsMouseOutColor;
    Font.Assign(ListViewItem.FSubItemsFont);
    AutoSize := False;
    Tag := Length(ListViewItem.FLabels) - 1;
    OnMouseEnter := ListViewItem.ObjectsMouseEnter;
    OnMouseLeave := ListViewItem.ObjectsMouseLeave;
    OnClick := ListViewItem.ObjectsClick;
    OnDblClick := ListViewItem.ObjectsDblClick;
    OnMouseMove := ListViewItem.ObjectsMouseMove;
    OnMouseUp := ListViewItem.ObjectsMouseUp;
    OnMouseDown := ListViewItem.ObjectsMouseDown;
  end;

  for Ind := Length(ListViewItem.FLabels) - 1 downto Index + 1 do
    ListViewItem.FLabels[Ind].Caption := ListViewItem.FLabels[Ind - 1].Caption;
  ListViewItem.FLabels[Index].Caption := S;
  ListViewItem.NeedToUpdate;
end;

procedure TW7SubItemsList.Delete(Index: Integer);
var
  Ind: integer;
begin
  if Index >= Length(ListViewItem.FLabels) then
    Exit;
  for Ind := Index to Length(ListViewItem.FLabels) - 2 do
    ListViewItem.FLabels[Ind].Caption := ListViewItem.FLabels[Ind + 1].Caption;
  ListViewItem.FLabels[Length(ListViewItem.FLabels) - 1].Free;
  SetLength(ListViewItem.FLabels, Length(ListViewItem.FLabels) - 1);
  ListViewItem.NeedToUpdate;
end;

procedure TW7SubItemsList.Clear;
var
  Ind: integer;
begin
  for Ind := 0 to Length(ListViewItem.FLabels) - 1 do
    ListViewItem.FLabels[Ind].Free;
  SetLength(ListViewItem.FLabels, 0);
  ListViewItem.NeedToUpdate;
end;

procedure TW7SubItemsList.SetUpdateState(Updating: Boolean);
begin
  if not Updating then
    ListViewItem.NeedToUpdate;
end;

///////////////////////

constructor TW7CustomTaskItem.Create(AOwner: TComponent);
begin
  inherited;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FIconSize := is48px;
  FIconHeight := 48;
  FImageIndex := -1;
  Height := 57;
  FDescription := 'Task description';
  FIconAlignment := wvaTop;
  FVerticalDelta := 7;
  FHorizontalDelta := 7;
  if GetWindowsVersion >= 6 then
    Font.Name := W7StandartFontName;
  FDescriptionFont := TFont.Create;
  FDescriptionFont.Assign(Font);
  FDescriptionFont.Color := $00CC6600;
  Font.Color := $00126E00;
  if GetWindowsVersion >= 6 then
    FDescriptionFont.Size := W7StandartFontSize;
  Font.Size := 12;
  FDescriptionAlignment := wvaTop;
end;

destructor TW7CustomTaskItem.Destroy;
begin
  FImageChangeLink.Free;
  FImageChangeLink := nil;
  FDescriptionFont.Free;
  inherited;
end;

procedure TW7CustomTaskItem.DrawButtonElements;
var
  CurY, CurX: integer;
  Flags: Cardinal;
  DstRect: TRect;
begin
  CurX := FHorizontalDelta;
  CurY := FVerticalDelta;
  case FIconAlignment of
    wvaTop: CurY := FVerticalDelta;
    wvaCenter: CurY := Height div 2 - FIconHeight div 2;
    wvaBottom: CurY := Height - FVerticalDelta - FIconHeight;
  end;
  if Assigned(FImages) and (FImageIndex >= 0) and (FImageIndex < FImages.Count) then
  begin
    FImages.Draw(Canvas, CurX, CurY, FImageIndex);
    CurX := FHorizontalDelta + 10 + FIconHeight;
  end;

  Canvas.Font.Assign(Font);
  Canvas.Brush.Style := bsClear;

  if not Enabled then
    Canvas.Font.Color := clGray;

  Flags := 0;
  Flags := Flags or DT_LEFT or DT_SINGLELINE or DT_VCENTER or DT_WORD_ELLIPSIS;
  DstRect := Rect(CurX, FVerticalDelta, Width - FHorizontalDelta, Canvas.TextHeight('W') + FVerticalDelta);
  DrawText(Canvas.Handle, PChar(Caption), Length(Caption), DstRect, Flags);

  CurY := Canvas.TextHeight('W') + FVerticalDelta;
  if FDescriptionAlignment = wvaTop then
    CurY := CurY + FVerticalDelta;
  Canvas.Font.Assign(FDescriptionFont);
  Canvas.Brush.Style := bsClear;

  if not Enabled then
    Canvas.Font.Color := clGray;

  Flags := 0;
  Flags := Flags or DT_LEFT or DT_WORD_ELLIPSIS;
  case FDescriptionAlignment of
    wvaTop: Flags := Flags or DT_TOP or DT_WORDBREAK;
    wvaCenter: Flags := Flags or DT_VCENTER or DT_SINGLELINE;
    wvaBottom: Flags := Flags or DT_BOTTOM or DT_SINGLELINE;
  end;
  DstRect := Rect(CurX, CurY, Width - FHorizontalDelta, Height - FVerticalDelta);
  DrawText(Canvas.Handle, PChar(FDescription), Length(FDescription), DstRect, Flags);
end;

procedure TW7CustomTaskItem.Paint;
begin
  inherited;
  DrawButtonElements;
end;

procedure TW7CustomTaskItem.SetImages(Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    if Images <> nil then
      Images.UnRegisterChanges(FImageChangeLink);
    FImages := Value;
    if Images <> nil then
    begin
      Images.RegisterChanges(FImageChangeLink);
      Images.FreeNotification(Self);
    end;
    UpdateImageList;
  end;
end;

procedure TW7CustomTaskItem.SetImageIndex(Value: integer);
begin
  FImageIndex := Value;
  UpdateImageList;
end;

procedure TW7CustomTaskItem.SetIconSize(Value: TW7ButtonIconSize);
begin
  FIconSize := Value;
  case FIconSize of
    is16px: FIconHeight := 16;
    is24px: FIconHeight := 24;
    is32px: FIconHeight := 32;
    is48px: FIconHeight := 48;
  end;
  UpdateImageList;
end;

procedure TW7CustomTaskItem.ImageListChange(Sender: TObject);
begin
  if HandleAllocated then
    UpdateImageList;
end;

procedure TW7CustomTaskItem.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if not (csDestroying in ComponentState) and (AOperation = opRemove) then
  begin
    if (AComponent = FImages) then
    begin
      Images := nil;
      Invalidate;
    end;
  end;
end;

procedure TW7CustomTaskItem.UpdateImageList;
begin
  Invalidate;
end;

procedure TW7CustomTaskItem.SetDescription(Value: TCaption);
begin
  FDescription := Value;
  Invalidate;
end;

procedure TW7CustomTaskItem.SetIconAlignment(Value: TW7VerticalAlignment);
begin
  FIconAlignment := Value;
  Invalidate;
end;

procedure TW7CustomTaskItem.SetDescriptionFont(Value: TFont);
begin
  FDescriptionFont.Assign(Value);
  Invalidate;
end;

procedure TW7CustomTaskItem.SetDescriptionAlignment(Value: TW7VerticalAlignment);
begin
  FDescriptionAlignment := Value;
  Invalidate;
end;

procedure TW7CustomTaskItem.SetVerticalDelta(Value: integer);
begin
  FVerticalDelta := Value;
end;

procedure TW7CustomTaskItem.SetHorizontalDelta(Value: integer);
begin
  FHorizontalDelta := Value;
end;

end.