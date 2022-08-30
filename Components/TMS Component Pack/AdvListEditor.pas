{***************************************************************************}
{ TAdvListEditor component                                                  }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012 - 2015                                        }
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

{$I TMSDEFS.INC}

unit AdvListEditor;

{$R AdvListEditor.res}

interface

uses
  Classes, Graphics, Controls, StdCtrls, Forms, AdvGDIP, Types, Windows,
  SysUtils, AdvEdit, AdvEddd, Messages, ImgList;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 14; // Build nr.

  // version history
  // 1.0.0.0 : First release
  // 1.0.1.0 : New: Added property MaxItems
  // 1.0.1.1 : Fixed : Issue with readonly handling
  // 1.1.0.0 : New : Font in lookup dropdown customizable
  //         : New : Spacing added for items in lookup dropdown
  //         : New : Added capability to define multiple separator characters
  //         : New : LookupPopup.ValueSeparator property added
  // 1.1.0.1 : Fixed & improvements wrt item click & hint sensitivity
  // 1.1.0.2 : Fixed : Issue with OnKey* event handling
  // 1.1.0.3 : Fixed : Various small issues
  // 1.1.0.4 : Fixed : Issue with backspace under specific circumstances
  // 1.1.1.0 : New : Lookup methods lmFromStartDisplayAndValue and lmFullDisplayAndValue added
  //         : New : Event OnLookupNeedData added
  // 1.2.1.0 : New : Cancel adding an item via setting EditText = '' in the OnValueEditDone event.
  // 1.2.1.1 : Improved : Performance with adding Lookup items when using BeginUpdate/EndUpdate
  //         : Fixed : Issue with directly assigning TAdvListValues to AdvListEditor.Lookup
  // 1.2.2.0 : New : Property MinLines added
  // 1.2.2.1 : Improved : lookup editing inserts lookup value at edit position
  // 1.2.2.2 : Fixed : Issue with display when editor position = 0 and first item is selected
  // 1.2.2.3 : Fixed : Rare issue with inplace editor width update incorrect
  //         : Fixed : Rare issue with lookup incorrectly stopping
  //         : Fixed : Issue with AutoSize and control resize
  // 1.2.2.4 : Fixed : Issue with AutoSize and runtime imagelist assignment
  // 1.2.2.5 : Fixed : Issue with use of MaxItems
  // 1.2.2.6 : Fixed : Issue with setting ValueSeparator
  // 1.2.2.7 : Fixed : Issues with high DPI setting
  //         : Fixed : Issue with autosize & alignment
  // 1.2.2.8 : Fixed : Issue with persisting tag during editing
  // 1.2.2.9 : Fixed : Rare issue with using spaces in value entry
  // 1.2.2.10: Fixed : Issue with ampersand character in values
  // 1.2.2.11: Fixed : Issue with use of larger than standard font
  // 1.2.2.12: Improved : Small change with handling Tag during start of editing item
  // 1.2.2.13: Fixed : Issue with editor height when autosize = true
  // 1.2.2.14: Fixed : Rare issue with autosize & editing


type
  TAdvListValue = class(TCollectionItem)
  private
    FTag: integer;
    FValue: string;
    FDisplayText: string;
    FImageIndex: integer;
    procedure SetDisplayText(const AValue: string);
    procedure SetValue(const AValue: string);
    procedure SetImageIndex(const Value: integer);
  protected
     procedure Changed; virtual;
     function GetWidth(Canvas: TCanvas): integer;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property DisplayText: string read FDisplayText write SetDisplayText;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Value: string read FValue write SetValue;
    property Tag: integer read FTag write FTag default 0;
  end;

  TAdvListValues = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItem(Index: integer): TAdvListValue;
    procedure SetItem(Index: integer; const Value: TAdvListValue);
  protected
    procedure Changed; virtual;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property Items[Index: integer]: TAdvListValue read GetItem write SetItem; default;
    function Add: TAdvListValue;
    function Insert(Index: integer): TAdvListValue;
    function AddPair(DisplayText, Value: string): TAdvListValue; overload;
    function AddPair(DisplayText, Value: string; ImageIndex: integer): TAdvListValue; overload;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TValueClickEvent = procedure(Sender: TObject; ValueIndex: integer) of object;

  TItemAppearance = class(TPersistent)
  private
    FBorderColor: TColor;
    FTextColor: TColor;
    FColorTo: TColor;
    FColorFrom: TColor;
    FRounding: integer;
    FOnChange: TNotifyEvent;
    procedure SetBorderColor(const Value: TColor);
    procedure SetColorFrom(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetRounding(const Value: integer);
    procedure SetTextColor(const Value: TColor);
  protected
    procedure DoChange; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property ColorFrom: TColor read FColorFrom write SetColorFrom;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property Rounding: integer read FRounding write SetRounding default 6;
    property TextColor: TColor read FTextColor write SetTextColor default clBlack;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAppearance = class(TPersistent)
  private
    FNormal: TItemAppearance;
    FSelected: TItemAppearance;
    FOnChange: TNotifyEvent;
    procedure SetNormal(const Value: TItemAppearance);
    procedure SetSelected(const Value: TItemAppearance);
    procedure ItemAppearanceChange(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Normal: TItemAppearance read FNormal write SetNormal;
    property Selected: TItemAppearance read FSelected write SetSelected;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TValueEditEvent = procedure(Sender: TObject; Value: TAdvListValue; var EditText: string) of object;
  TValueHintEvent = procedure(Sender: TObject; Value: TAdvListValue; var HintStr: string) of object;
  TValueDeleteEvent = procedure(Sender: TObject; Value: TAdvListValue; var Allow: boolean) of object;

  TLookupMethod = (lmFromStart, lmFull, lmFromStartDisplayAndValue, lmFullDisplayAndValue);

  TLookupPopup = class(TPersistent)
  private
    FCount: integer;
    FColor: TColor;
    FCaseSensitive: boolean;
    FEnabled: boolean;
    FOnChange: TNotifyEvent;
    FFromChar: integer;
    FFont: TFont;
    FSpacing: integer;
    FValueSeparator: TValueSeparator;
    procedure SetColor(const Value: TColor);
    procedure SetCount(const Value: integer);
    procedure SetCaseSensitive(const Value: boolean);
    procedure SetEnabled(const Value: boolean);
    procedure SetFromChar(const Value: integer);
    procedure SetFont(const Value: TFont);
    procedure FontChanged(Sender: TObject);
    procedure SetSpacing(const Value: integer);
    procedure SetValueSeparator(const Value: TValueSeparator);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property CaseSensitive: boolean read FCaseSensitive write SetCaseSensitive default false;
    property Color: TColor read FColor write SetColor default clWindow;
    property Count: integer read FCount write SetCount default 6;
    property Enabled: boolean read FEnabled write SetEnabled default true;
    property Font: TFont read FFont write SetFont;
    property FromChar: integer read FFromChar write SetFromChar default 1;
    property Spacing: integer read FSpacing write SetSpacing default 0;
    property ValueSeparator: TValueSeparator read FValueSeparator write SetValueSeparator default vsBracket;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvListEditor = class(TCustomControl)
  private
    FAppearance: TAppearance;
    FEdit: TAdvEdit;
    FList: TAdvListValues;
    FLookup: TAdvListValues;
    FSeparator: string;
    FCaption: string;
    FItemIndex: integer;
    FEditPos: integer;
    FDownIndex: integer;
    FImages: TCustomImageList;
    FOnValueClick: TValueClickEvent;
    FOnValueEditStart: TValueEditEvent;
    FOnValueEditDone: TValueEditEvent;
    FOnValueHint: TValueHintEvent;
    FAutoSize: boolean;
    FHintItem: integer;
    FOnValueDelete: TValueDeleteEvent;
    FBorderStyle: TBorderStyle;
    FBorderColor: TColor;
    FLookupMethod: TLookupMethod;
    FDesignTime: boolean;
    FReadOnly: boolean;
    FLookupPopup: TLookupPopup;
    FMaxLines: integer;
    FMaxItems: integer;
    FOnLookupNeedData: TLookupNeedDataEvent;
    FEditOffset: integer;
    FMinLines: integer;
    FEditTag: integer;
    FEditImageIndex: integer;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure SetList(const Value: TAdvListValues);
    procedure SetCaption(const Value: string);
    procedure SetItemIndex(const Value: integer);
    procedure SetLookup(const Value: TAdvListValues);
    procedure SetAppearance(const Value: TAppearance);
    procedure EditLookupIndexSelect(Sender: TObject; Index: integer; var AValue: string);
    procedure EditLookupNeedData(Sender: TObject; Value: string; List: TStrings; var ItemIndex: integer);
    procedure SetImages(const Value: TCustomImageList);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetAutoSizeEx(const Value: boolean);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetBorderColor(const Value: TColor);
    procedure SetLookupMethod(const Value: TLookupMethod);
    procedure SetReadOnly(const Value: boolean);
    procedure AppearanceChange(Sender: TObject);
    procedure LookupPopupChange(Sender: TObject);
    procedure SetLookupPopup(const Value: TLookupPopup);
    procedure SetMaxLines(const Value: integer);
    procedure SetMaxItems(const Value: integer);
    procedure SetMinLines(const Value: integer);
  protected
    procedure EditKeyPress(Sender: TObject; var Key: Char); virtual;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure EditKeyUp(Sender: TObject; var Key: word; Shift: TShiftState); virtual;
    procedure EditClipboardPaste(Sender: TObject; value: string; var allow: Boolean);
    procedure DrawItem(ACanvas: TCanvas; R: TRect; AItem: TAdvListValue; Selected: boolean); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function GetEditPos(Index: integer; AWidth: integer): TPoint;
    procedure UpdateEditPos(Index: integer);
    function ItemHeight: integer;
    function EditWidth: integer;
    procedure HideEdit;
    procedure CreateWnd; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure DoExit; override;
    procedure DoEnter; override;
    procedure Loaded; override;
    procedure ListChanged(Sender: TObject); virtual;
    procedure LookupChanged(Sender: TObject); virtual;
    procedure DoAutoSize; virtual;
    function GetAutoHeight(AWidth: integer): integer;
    procedure DoValueClick(Index: integer); virtual;
    procedure DoValueHint(Index: integer; var HintStr: string); virtual;
    procedure DoValueEditStart(AValue: TAdvListValue; var EditText: string); virtual;
    procedure DoValueEditDone(AValue: TAdvListValue; var EditText: string); virtual;
    procedure DoValueDelete(AValue: TAdvListValue; var Allow: boolean); virtual;
    procedure DoLookupNeedData(Value: string; List: TStrings; var ItemIndex: integer); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function HasMaxItems: boolean;
    function GetVersionNr: Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function ValueAtXY(X,Y: integer): integer;
    function ValueRect(Index: integer): TRect;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    procedure StartEdit(Index: integer);
    procedure StopEdit;
  published
    property Align;
    property Anchors;
    property Appearance: TAppearance read FAppearance write SetAppearance;
    property AutoSize: boolean read FAutoSize write SetAutoSizeEx default true;
    property BorderColor: TColor read FBorderColor write SetBorderColor default $00B99D7F;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Caption: string read FCaption write SetCaption;
    property Color;
    property EditOffset: integer read FEditOffset write FEditOffset;
    property Font;
    property HelpContext;
    property HelpKeyword;
    property Images: TCustomImageList read FImages write SetImages;
    property Lookup: TAdvListValues read FLookup write SetLookup;
    property LookupMethod: TLookupMethod read FLookupMethod write SetLookupMethod default lmFromStart;
    property LookupPopup: TLookupPopup read FLookupPopup write SetLookupPopup;
    property MaxItems: integer read FMaxItems write SetMaxItems default 0;
    property MaxLines: integer read FMaxLines write SetMaxLines default 0;
    property MinLines: integer read FMinLines write SetMinLines default 0;
    property PopupMenu;
    property ReadOnly: boolean read FReadOnly write SetReadOnly default false;
    property Separator: string read FSeparator write FSeparator;
    property ShowHint;
    property TabOrder;
    property TabStop;
    {$IFDEF DELPHIXE_LVL}
    property Touch;
    {$ENDIF}
    property Values: TAdvListValues read FList write SetList;
    property Version: string read GetVersion write SetVersion;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF DELPHIXE_LVL}
    property OnGesture;
    {$ENDIF}
    {$IFDEF DELPHI2007_LVL}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnValueDelete: TValueDeleteEvent read FOnValueDelete write FOnValueDelete;
    property OnValueEditStart: TValueEditEvent read FOnValueEditStart write FOnValueEditStart;
    property OnValueEditDone: TValueEditEvent read FOnValueEditDone write FOnValueEditDone;
    property OnValueClick: TValueClickEvent read FOnValueClick write FOnValueClick;
    property OnValueHint: TValueHintEvent read FOnValueHint write FOnValueHint;
    property OnLookupNeedData: TLookupNeedDataEvent read FOnLookupNeedData write FOnLookupNeedData;
  end;


implementation

uses
  Math;

const
  ITEMMARGIN = 10;
  EDITMARGIN = 7;
  LEFTOFFSET = 4;
  TOPOFFSET = 2;
  ITEMHEIGHT = 18;
  LINEMARGIN = 6;
  IMAGEMARGIN = 4;

  {$IFDEF DELPHI_UNICODE}
type
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}


{ TAdvListEditor }


procedure TAdvListEditor.AppearanceChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvListEditor.CMColorChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FEdit) then
    FEdit.Color := Color;
end;

procedure TAdvListEditor.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateEditPos(Values.Count);
  Repaint;
end;

procedure TAdvListEditor.CMHintShow(var Msg: TMessage);
var
  hi: PHintInfo;
  i: integer;
begin
  inherited;

  hi := PHintInfo(Msg.LParam);

  i := ValueAtXY(hi.CursorPos.X, hi.CursorPos.Y);

  if (i <> -1) then
  begin
    hi.HintStr := Values[i].Value;
    DoValueHint(i, hi.HintStr);
  end;
end;

constructor TAdvListEditor.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := true;
  FEdit := TAdvEdit.Create(Self);
  FEdit.BorderStyle := bsNone;
  FEdit.OnKeyPress := EditKeyPress;
  FEdit.OnKeyDown := EditKeyDown;
  FEdit.OnKeyUp := EditKeyUp;
  FEdit.OnClipboardPaste := EditClipboardPaste;
  FEdit.OnLookupIndexSelect := EditLookupIndexSelect;
  FEdit.OnLookupNeedData := EditLookupNeedData;
  FEdit.Width := 2;
  FEdit.Height := 15;
  FEdit.Left := 1;
  FEdit.FullTextSearch := false;

  FEditOffset := -2;

  FList := TAdvListValues.Create(Self);
  FList.OnChange := ListChanged;

  FLookup := TAdvListValues.Create(Self);
  FLookup.OnChange := LookupChanged;
  FLookupMethod := lmFromStart;

  FLookupPopup := TLookupPopup.Create;
  FLookupPopup.OnChange := LookupPopupChange;

  Width := 250;
  Height := 24;
  Color := clWhite;
  FItemIndex := -1;
  FMaxLines := 0;
  Separator := ';';
  FBorderColor := $00B99D7F;
  FBorderStyle := bsSingle;
  FAutoSize := True;
  FHintItem := -1;

  FAppearance := TAppearance.Create;
  FAppearance.OnChange := AppearanceChange;

  FAppearance.Normal.ColorFrom := RGB(220,230,248);
  FAppearance.Normal.ColorTo := RGB(189,207,241);
  FAppearance.Normal.BorderColor := RGB(120,133,215);
  FAppearance.Normal.TextColor := clBlack;

  FAppearance.Selected.ColorFrom := RGB(115,163,230);
  FAppearance.Selected.ColorTo := RGB(35,110,216);
  FAppearance.Selected.BorderColor := RGB(35,110,216);
  FAppearance.Selected.TextColor := clWhite;

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
end;

procedure TAdvListEditor.CreateWnd;
begin
  inherited;
  FEdit.Parent := Self;
  UpdateEditPos(Values.Count);

  FEdit.Lookup.Enabled := true;
  FEdit.Lookup.CaseSensitive := false;
  FEdit.Lookup.NumChars := 1;
  FEdit.Lookup.ShowValue := true;
  FEdit.Font.Assign(Font);
  FEdit.FullTextSearch := (LookupMethod = lmFull) or (LookupMethod = lmFullDisplayAndValue);
  FEdit.Lookup.SearchValue := (LookupMethod = lmFromStartDisplayAndValue) or (LookupMethod = lmFullDisplayAndValue);

  LookupPopupChange(Self);

  if FDesignTime then
    Values.AddPair('Value 1','');

  if AutoSize then
    DoAutoSize;
end;

destructor TAdvListEditor.Destroy;
begin
  FEdit.Free;
  FList.Free;
  FLookup.Free;
  FLookupPopup.Free;
  FAppearance.Free;
  inherited;
end;

function TAdvListEditor.GetAutoHeight(AWidth: integer): integer;
var
  pt: TPoint;
  NewHeight: integer;
  MinHeight: integer;
begin
  Result := Height;

  pt := GetEditPos(Values.Count, AWidth);

  NewHeight := pt.Y + ItemHeight + LINEMARGIN;

  if MinLines > 0 then
  begin
    MinHeight :=  TOPOFFSET + (ItemHeight - Canvas.TextHeight('gh')) div 2
      + MinLines * (ItemHeight + LINEMARGIN);
    if BorderStyle = bsSingle then
      inc(MinHeight);
    if NewHeight < MinHeight then
      NewHeight := MinHeight;
  end;

  if (MaxLines > 0) and (NewHeight > (MaxLines + 1) * (ItemHeight + LINEMARGIN)) then
    Exit;

  Result := NewHeight;
end;

procedure TAdvListEditor.DoAutoSize;
begin
  Height := GetAutoHeight(Width);
  FEdit.Height := Canvas.TextHeight('gh')+ 4;
end;

procedure TAdvListEditor.DoEnter;
begin
  inherited;

  if ReadOnly then
  begin
    FEdit.Width := 2;
  end;

  UpdateEditPos(Values.Count);

  FEdit.SetFocus;
end;

procedure TAdvListEditor.DoExit;
begin
  inherited;
  if not ReadOnly then
  begin
    StopEdit;
    UpdateEditPos(Values.Count);
  end;
end;

procedure TAdvListEditor.DoLookupNeedData(Value: string; List: TStrings; var ItemIndex: integer);
begin
  if Assigned(OnLookupNeedData) then
    OnLookupNeedData(Self, Value, List, ItemIndex);
end;

procedure TAdvListEditor.DoValueClick(Index: integer);
begin
  if Assigned(OnValueClick) then
    OnValueClick(Self, Index);
end;

procedure TAdvListEditor.DoValueDelete(AValue: TAdvListValue;
  var Allow: boolean);
begin
  if Assigned(OnValueDelete) then
    OnValueDelete(Self, AValue, Allow);
end;

procedure TAdvListEditor.DoValueEditDone(AValue: TAdvListValue; var EditText: string);
begin
  if Assigned(OnValueEditDone) then
    OnValueEditDone(Self, AValue, EditText);
end;

procedure TAdvListEditor.DoValueEditStart(AValue: TAdvListValue; var EditText: string);
begin
  if Assigned(OnValueEditStart) then
    OnValueEditStart(Self, AValue, EditText);
end;

procedure TAdvListEditor.DoValueHint(Index: integer; var HintStr: string);
begin
  if Assigned(OnValueHint) then
    OnValueHint(Self, Values[Index], HintStr);
end;

procedure TAdvListEditor.DrawItem(ACanvas: TCanvas; R: TRect;
  AItem: TAdvListValue; Selected: boolean);
var
  gp: TGPGraphics;
  gpBrush: TGPLinearGradientBrush;
  gpPen: TGPPen;
  gprect: TGPRectF;
  gpPath: TGPGraphicsPath;
  clrfrom,clrto,clrbrdr: TColor;
  d: integer;
begin
  gp := TGPGraphics.Create(ACanvas.Handle);

  try
    gpRect := MakeRect(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);

    gp.SetSmoothingMode(SmoothingModeAntiAlias);

    gpPath := CreateRoundRectangle(R, Appearance.Normal.Rounding);

    if Selected then
    begin
      clrfrom := Appearance.Selected.ColorFrom;
      clrto := Appearance.Selected.ColorTo;
      clrbrdr := Appearance.Selected.BorderColor;
    end
    else
    begin
      clrfrom := Appearance.Normal.ColorFrom;
      clrto := Appearance.Normal.ColorTo;
      clrbrdr := Appearance.Normal.BorderColor;
    end;

    if clrto = clNone then
      clrto := clrfrom;

    gpBrush := TGPLinearGradientBrush.Create(gpRect,ColorToARGB(clrfrom),
      ColorToARGB(clrto),LinearGradientModeVertical);

    gpPen := TGPPen.Create(ColorToARGB(clrbrdr),1);

    gp.FillPath(gpBrush, gpPath);

    gp.DrawPath(gpPen, gpPath);

    gpPen.Free;
    gpBrush.Free;
    gpPath.Free;
  finally
    gp.Free;
  end;

  if Selected then
    Canvas.Font.Color := Appearance.Selected.TextColor
  else
    Canvas.Font.Color := Appearance.Normal.TextColor;

  if (AItem.ImageIndex >= 0) and Assigned(Images) then
  begin
    d := R.Bottom - R.Top - Images.Height;
    if d > 0 then
      d := d div 2
    else
      d := 0;

    Images.Draw(Canvas, R.Left + 2 + (Appearance.Normal.Rounding div 2), R.Top + d, AItem.ImageIndex);
    R.Left := R.Left + Images.Width + IMAGEMARGIN;
  end;

  Canvas.Brush.Style := bsClear;

  R.Left := R.Left + 2 + (Appearance.Normal.Rounding div 2);

  DrawText(Canvas.Handle, PChar(AItem.DisplayText), Length(AItem.DisplayText), R, DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
end;

procedure TAdvListEditor.EditClipboardPaste(Sender: TObject; value: string;
  var allow: Boolean);
begin
  FEdit.Width := Canvas.TextWidth(Value) + EDITMARGIN;
end;

procedure TAdvListEditor.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  EditText: string;
  lv: TAdvListValue;
  Allow, Lst, isHidden: boolean;
  edPos: integer;

begin
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);

  if (Key in [VK_RETURN]) and (FEdit.Text <> '') and not FEdit.LookupVisible then
  begin
    StopEdit;
    UpdateEditPos(FEditPos + 1);
    ItemIndex := -1;
    Key := 0;
  end;

  inherited;

  isHidden := (FEdit.Width = 2) and (FEditPos = Values.Count);

  if (Key in [VK_ESCAPE]) and (FEdit.OrigText <> '') and (FEdit.Text <> '') then
  begin
    StopEdit;
    ItemIndex := FEditPos;
    FEdit.Width := 2;
    UpdateEditPos(Values.Count);
    Key := 0;
  end;

  if (Key = VK_F2) and (ItemIndex >= 0) and not ReadOnly then
  begin
    StartEdit(ItemIndex);
  end;

  if (Key = VK_BACK) and (FEdit.SelStart = 0) and (FEditPos > 0) and not isHidden then
  begin
    if FEditPos > Values.Count then
      FEditPos := Values.Count;

    if (ItemIndex >=0) then
    begin
      if Values.Count > 0 then
      begin
        Allow := true;
        edPos := FEditPos;
        DoValueDelete(Values[FEditPos - 1], Allow);
        if Allow then
        begin
          Lst := FEditPos = Values.Count;
          Values.Delete(FEditPos - 1);
          Invalidate;

          if Lst and (FEditPos < Values.Count) then
            UpdateEditPos(edPos)
          else
            UpdateEditPos(edPos - 1);
        end;
      end;
    end
    else
    begin
      if FEditPos > 0 then
        ItemIndex := FEditPos - 1;
      Invalidate;
      Key := 0;
      Exit;
    end;
  end;

  if ((Key = VK_DELETE) and (ItemIndex >=0)) or ((Key = VK_BACK) and isHidden) then
  begin
    Allow := true;
    DoValueDelete(Values[ItemIndex],Allow);
    if Allow then
    begin
      Values.Delete(ItemIndex);
    end;
  end;

  if (Key = VK_DELETE) and (FEdit.Text = '') then
  begin
    if (FEditPos < Values.Count) and (ItemIndex >= 0) then
    begin
      Values.Delete(FEditPos);
      UpdateEditPos(FEditPos);
    end
    else
    begin
      ItemIndex := FEditPos;
      HideEdit;
      FEdit.Width := 2;
      UpdateEditPos(Values.Count);
    end;
    Invalidate;
  end;

  if (Key = VK_HOME) and (FEdit.SelStart = 0) and (FEdit.SelLength = 0) then
  begin
    if ReadOnly then
      ItemIndex := 0
    else
    begin
      FEditPos := 0;
      UpdateEditPos(0);
    end;
  end;

  if (Key = VK_END) and (FEdit.SelStart = 0) and (FEdit.SelLength = 0) then
  begin
    if ReadOnly then
      ItemIndex := Values.Count - 1
    else
      UpdateEditPos(Values.Count);
  end;

  if (Key = VK_LEFT) and (FEdit.SelStart = 0) and (FEdit.SelLength = 0) then
  begin
    if (ItemIndex >= 0) then
    begin
      if ReadOnly or HasMaxItems then
      begin
        if ItemIndex > 0 then
          ItemIndex := ItemIndex - 1;
      end
      else
      begin
        FEditPos := ItemIndex;
        ItemIndex := -1;
        UpdateEditPos(FEditPos);
        FEdit.SetEditText('');
      end;
    end
    else
      if FEditPos > 0 then
      begin
        if FEdit.Text <> '' then
        begin
          EditText := FEdit.Text;
          lv := Values.Insert(FEditPos);
          DoValueEditDone(lv,EditText);

          if EditText <> '' then
            lv.DisplayText := EditText
          else
          begin
            Allow := true;
            DoValueDelete(lv, Allow);
            if Allow then
              Values.Delete(lv.Index);
          end;
        end;

        ItemIndex := FEditPos - 1;
        FEdit.SetEditText('');
        FEdit.Width := 2;
        FEdit.Left := FEdit.Left - 2;
      end;
  end;

  if (Key = VK_RIGHT) and (FEdit.SelStart = Length(FEdit.Text)) then
  begin
    if (ItemIndex < Values.Count) and (ItemIndex >= 0) then
    begin
      if ReadOnly or HasMaxItems then
      begin
        if ItemIndex < Values.Count - 1 then
          ItemIndex := ItemIndex + 1;
      end
      else
      begin
        FEditPos := ItemIndex + 1;
        ItemIndex := -1;
        UpdateEditPos(FEditPos);
        FEdit.SetEditText('');
      end;
    end
    else
    if FEditPos < Values.Count then
    begin
      if FEdit.Text <> '' then
      begin
        EditText := FEdit.Text;
        lv := Values.Insert(FEditPos);
        DoValueEditDone(lv, EditText);
        if EditText <> '' then
          lv.DisplayText := EditText
        else
        begin
          Allow := true;
          DoValueDelete(lv, Allow);
          if Allow then
            Values.Delete(lv.Index);
        end;
      end;

      ItemIndex := FEditPos;

      //UpdateEditPos(FEditPos + 1);

      FEdit.SetEditText('');
      FEdit.Width := 2;
      FEdit.Left := Max(1,FEdit.Left - 2);
    end;
  end;
end;

function IsSeparator(Key: char; Separator: string): boolean;
var
  i: integer;
begin
  Result := false;

  for i := 1 to Length(Separator) do
    if Key = Separator[i] then
    begin
      Result := true;
      break;
    end;
end;


procedure TAdvListEditor.EditKeyPress(Sender: TObject; var Key: Char);
var
  EditText: string;
  lv: TAdvListValue;
  Allow: boolean;
begin
  if Assigned(OnKeypress) then
    OnKeyPress(Self,Key);

  // invisible editor
  if (FEdit.Width <= 2) or ReadOnly then
  begin
    Key := #0;
    Exit;
  end;

  if (Key = #13) and not FEdit.LookupVisible then
  begin
    Key := #0;
  end;

  {
  if (Key = #27) and not FEdit.LookupVisible then
  begin
    Key := #0;
    Exit;
  end;
  }

  if (Key = #8) and (FEdit.Text = '') then
  begin
    Key := #0;
    Exit;
  end;

  if IsSeparator(Key, Separator) then
  begin
    EditText := FEdit.Text;
    FEdit.Width := 2;
    lv := Values.Add;
    DoValueEditDone(lv,EditText);
    if EditText <> '' then
      lv.DisplayText := EditText
    else
    begin
      Allow := true;
      DoValueDelete(lv,Allow);
      if Allow then
        Values.Delete(lv.Index);
    end;

    UpdateEditPos(Values.Count);
    inherited;
    Key := #0;
    FEdit.SetEditText('');
    Exit;
  end
  else
  begin
    inherited;
    EditText := FEdit.Text;
    if Key <> #8 then
      EditText := EditText + Key;

    FEdit.Width := Canvas.TextWidth(EditText) + EDITMARGIN;
    UpdateEditPos(FEditPos);
    Invalidate;
  end;
end;


procedure TAdvListEditor.EditKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Assigned(OnKeyUp) then
    OnKeyUp(Self, Key, Shift);
end;

procedure TAdvListEditor.EditLookupIndexSelect(Sender: TObject; Index: integer;
  var AValue: string);
var
  disp: string;
  lv: TAdvListValue;
  Allow: boolean;
  EP: integer;
begin
  disp := FEdit.Lookup.DisplayList.Strings[Index];

  if FEditPos >= Values.Count then
    lv := Values.Add
  else
    lv := Values.Insert(FEditPos);

  EP := lv.Index;

  lv.DisplayText := disp;
  lv.Value := avalue;
  lv.Tag := Lookup.Items[Index].Tag;
  lv.ImageIndex := integer(FEdit.Lookup.ValueList.Objects[Index]);

  //Values.AddPair(disp, avalue);

  DoValueEditDone(lv, disp);

  if disp <> '' then
    Values[EP].DisplayText := disp
  else
  begin
    Allow := true;
    DoValueDelete(Values[EP],Allow);
    if Allow then
      Values.Delete(EP);
  end;

  FEdit.SetEditText('');
  AValue := '';

  UpdateEditPos(EP + 1);
end;

procedure TAdvListEditor.EditLookupNeedData(Sender: TObject; Value: string;
  List: TStrings; var ItemIndex: integer);
begin
  DoLookupNeedData(Value, List, ItemIndex);
end;

function TAdvListEditor.EditWidth: integer;
begin
  if FEdit.Width <= 2 then
    Result := 0
  else
    Result := FEdit.Width + EDITMARGIN;
end;

procedure TAdvListEditor.StopEdit;
var
  EditText: string;
  lv: TAdvListValue;
  Allow: boolean;
begin
  EditText := FEdit.Text;
  if EditText <> '' then
  begin
    if FEditPos >= Values.Count then
      lv := Values.Add
    else
      lv := Values.Insert(FEditPos);

    lv.Tag := FEditTag;
    lv.ImageIndex := FEditImageIndex;

    DoValueEditDone(lv, EditText);

    if EditText <> '' then
      lv.DisplayText := EditText
    else
    begin
      Allow := true;
      DoValueDelete(lv, Allow);
      if Allow then
        Values.Delete(lv.Index);
    end;
  end;
  FEdit.SetEditText('');
end;

procedure TAdvListEditor.StartEdit(Index: integer);
var
  s: string;
  lv: TAdvListValue;
begin
  s := Values[Index].DisplayText;
  lv := Values[Index];
  DoValueEditStart(lv, s);
  FEditTag := lv.Tag;
  FEditImageIndex := lv.ImageIndex;
  Values.Delete(Index);
  FEdit.Width := Max(10, Canvas.TextWidth(s) + EDITMARGIN);
  FEdit.SetEditText(s);
  UpdateEditPos(Index);
  FEdit.SelectAll;
  FEdit.SetFocus;
end;

function TAdvListEditor.GetEditPos(Index: integer; AWidth: integer): TPoint;
var
  j, dx, dy, tw, th, dt: integer;
begin
  Canvas.Font.Assign(Font);

  th := ItemHeight;

  dt := th - Canvas.TextHeight('gh');

  dx := LEFTOFFSET;
  dy := TOPOFFSET + (dt div 2);

  if BorderStyle = bsSingle then
    inc(dy);

  for j := 0 to Index - 1 do
  begin
    if (j < Values.Count) then
    begin
      tw := Values[j].GetWidth(Canvas) + ITEMMARGIN;

      if dx + tw - ITEMMARGIN + 4 >= AWidth then
      //if dx + tw >= Width then
      begin
        dx := LEFTOFFSET + tw;
        dy := dy + th + LINEMARGIN;
      end
      else
        dx := dx + tw;
    end;
  end;

  if dx + FEdit.Width >= Width then
  begin
    dx := LEFTOFFSET;
    dy := dy + th + LINEMARGIN;
  end;

  Result := Point(dx,dy);
end;

function TAdvListEditor.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvListEditor.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

function TAdvListEditor.HasMaxItems: boolean;
begin
  Result := (MaxItems > 0) and (Values.Count = MaxItems);
end;

procedure TAdvListEditor.HideEdit;
begin
  FEdit.Width := 2;
  UpdateEditPos(Values.Count);
end;

function TAdvListEditor.ItemHeight: integer;
var
  th: integer;
begin
  Canvas.Font.Assign(Font);

  th := Canvas.TextHeight('gh');

  if Assigned(Images) then
    th := Max(Images.Height, th);

  Result := th;
end;

procedure TAdvListEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
end;

procedure TAdvListEditor.ListChanged(Sender: TObject);
begin
  Invalidate;

  if FAutoSize then
    DoAutoSize;

//  if (csDesigning in ComponentState) then
  UpdateEditPos(Values.Count);
end;


procedure TAdvListEditor.Loaded;
begin
  inherited;
  UpdateEditPos(Values.Count);

  if ReadOnly then
  begin
    FEdit.Width := 1;
    FEdit.Left := 1;
  end;

  if FAutoSize then
    DoAutoSize;

  LookupPopup.Font.Size := ScaleFromSmallFontsDimension(LookupPopup.Font.Size);
end;

procedure TAdvListEditor.LookupChanged(Sender: TObject);
var
  i: integer;
begin
  FEdit.Lookup.DisplayList.Clear;
  FEdit.Lookup.ValueList.Clear;

  for i := 0 to FLookup.Count - 1 do
  begin
    FEdit.Lookup.DisplayList.Add(FLookup.Items[i].DisplayText);
    FEdit.Lookup.ValueList.AddObject(FLookup.Items[i].Value, TObject(FLookup.Items[i].ImageIndex));
  end;
end;

procedure TAdvListEditor.LookupPopupChange(Sender: TObject);
begin
  if Assigned(FEdit) and HandleAllocated then
  begin
    FEdit.Lookup.Enabled := LookupPopup.Enabled;
    FEdit.Lookup.Color := FLookupPopup.Color;
    FEdit.Lookup.CaseSensitive := FLookupPopup.CaseSensitive;
    FEdit.Lookup.DisplayCount := FLookupPopup.Count;
    FEdit.Lookup.NumChars := FLookupPopup.FromChar;
    FEdit.Lookup.Font.Assign(FLookupPopup.Font);
    FEdit.Lookup.Spacing := FLookupPopup.Spacing;
    FEdit.Lookup.ValueSeparator := FLookupPopup.ValueSeparator;
  end;
end;
{
  dx := LEFTOFFSET;
  dy := TOPOFFSET;

  for i := 0 to Values.Count - 1 do
  begin
    dw := Values[i].GetWidth(Canvas) + ITEMMARGIN;

    if i = FEditPos then
      dx := dx + EditWidth;

    if dx + dw - ITEMMARGIN + 4 >= Width then
    begin
      dx := LEFTOFFSET;
      dy := dy + th + LINEMARGIN;
    end;

    if (x >= dx) and (x <= dx + dw - 4) and (y > dy) and (y < dy + ITEMHEIGHT) then
    begin
      // hit an item
      Result := i;
      Exit;
    end;

    dx := dx + dw;
  end;
end;
}

procedure TAdvListEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  i,dx,dy,dw,th: integer;
begin
  inherited;

  FDownIndex := -1;

  Canvas.Font.Assign(Font);
  th := ItemHeight;

  if FEdit.Text <> '' then
    StopEdit;

  dx := LEFTOFFSET;
  dy := TOPOFFSET;

  for i := 0 to Values.Count - 1 do
  begin
    dw := Values[i].GetWidth(Canvas) + ITEMMARGIN;

    if (i = FEditPos) then
    begin
      dx := dx + EditWidth;
    end;

    if dx + dw - ITEMMARGIN + 4 >= Width then
    begin
      dx := LEFTOFFSET;
      dy := dy + th + LINEMARGIN;
    end;

    if (x >= dx) and (x <= dx + dw - 4) and (y > dy) and (y < dy + ItemHeight + LINEMARGIN)  then
    begin
      FDownIndex := i;
      if not Focused then
        SetFocus;
      // already selected & edit
      if (i = ItemIndex) and not ReadOnly then
      begin
        ItemIndex := -1;
        StartEdit(i);
      end
      else
      // first selection
      begin
        ItemIndex := i;
        FEdit.Width := 2;
      end;

      // make sure keys continue to be received
      FEdit.SetFocus;
      Exit;
    end;

    // after last item on row
    if (x >= dx) and (dx + dw > Width) and (y > dy) and (y < dy + ItemHeight + LINEMARGIN) and not ReadOnly then
    begin
      UpdateEditPos(i);
      Exit;
    end;

    // before first item
    if (x < LEFTOFFSET) and (y > dy) and (y < dy + ItemHeight) and not ReadOnly then
    begin
      UpdateEditPos(i);
      Exit;
    end;

    if (x > dx + dw - 4) and (x <= dx + dw) and (y > dy) and (y < dy + ItemHeight + LINEMARGIN) and not ReadOnly then
    begin
      UpdateEditPos(i + 1);
      Exit;
    end;

    dx := dx + dw;
  end;

  if (x > dx) and (y > dy) and not ReadOnly then
  begin
    UpdateEditPos(Values.Count);
  end;

  FEdit.SetFocus;
end;

procedure TAdvListEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  inherited;
  i := ValueAtXY(X, Y);
  if i <> FHintItem then
  begin
    FHintItem := i;
    Application.CancelHint;
  end;
end;

procedure TAdvListEditor.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if (ValueAtXY(X,Y) = FDownIndex) and (FDownIndex >= 0) then
    DoValueClick(FDOwnIndex);
end;

procedure TAdvListEditor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil;

  inherited;
end;

procedure TAdvListEditor.Paint;
var
  i: integer;
  dx,dy,dw: integer;
  fh: integer;
  R: TRect;
begin
  inherited;

  Canvas.Font.Assign(Font);

  fh := ItemHeight;

  dx := LEFTOFFSET;
  dy := TOPOFFSET;

  if BorderStyle = bsSingle then
    inc(dy);

  for i := 0 to Values.Count - 1 do
  begin
    dw := Values[i].GetWidth(Canvas) + ITEMMARGIN;

    if (i = FEditPos) and not ReadOnly then
    begin
      if (dx + FEdit.Width >= Width) then
      begin
        dx := LEFTOFFSET;
        dy := dy + fh + LINEMARGIN;
      end;

      dx := dx + EditWidth;
    end;

    if dx + dw - ITEMMARGIN + 4 >= Width then
    begin
      dx := LEFTOFFSET;
      dy := dy + fh + LINEMARGIN;
    end;

    DrawItem(Canvas, Rect(dx, dy, dx + dw - ITEMMARGIN + 4, dy + fh + 2), Values[i], ItemIndex = i);

    dx := dx + dw;
  end;

  if BorderStyle = bsSingle then
  begin
    R := ClientRect;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := FBorderColor;
    Canvas.Rectangle(R);
  end;
end;

procedure TAdvListEditor.Resize;
begin
  inherited;
//  if FAutoSize then
//    DoAutoSize;
end;

procedure TAdvListEditor.SetAppearance(const Value: TAppearance);
begin
  FAppearance.Assign(Value);
end;

procedure TAdvListEditor.SetAutoSizeEx(const Value: boolean);
begin
  if (FAutoSize <> Value) then
  begin
    FAutoSize := Value;
    if Value then
      DoAutoSize;
  end;
end;

procedure TAdvListEditor.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TAdvListEditor.SetBorderStyle(const Value: TBorderStyle);
begin
  FBorderStyle := Value;
end;

procedure TAdvListEditor.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if FAutoSize and HandleAllocated and not (csLoading in ComponentState) then
    AHeight := GetAutoHeight(AWidth);

  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TAdvListEditor.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Invalidate;
  end;
end;

procedure TAdvListEditor.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  Invalidate;
  if FAutoSize then
    DoAutoSize;
end;

procedure TAdvListEditor.SetItemIndex(const Value: integer);
begin
  if (FItemIndex <> Value) then
  begin
    FItemIndex := Value;
    Invalidate;
  end;
end;

procedure TAdvListEditor.SetList(const Value: TAdvListValues);
begin
  FList.Assign(Value);
end;

procedure TAdvListEditor.SetLookup(const Value: TAdvListValues);
begin
  FLookup.Assign(Value);
end;

procedure TAdvListEditor.SetLookupMethod(const Value: TLookupMethod);
begin
  FLookupMethod := Value;
  if Assigned(FEdit) then
  begin
    FEdit.FullTextSearch := (FLookupMethod = lmFull) or (FLookupMethod = lmFullDisplayAndValue);
    FEdit.Lookup.SearchValue := (FLookupMethod = lmFromStartDisplayAndValue) or (FLookupMethod = lmFullDisplayAndValue);
  end;
end;

procedure TAdvListEditor.SetLookupPopup(const Value: TLookupPopup);
begin
  FLookupPopup.Assign(Value);
end;

procedure TAdvListEditor.SetMaxItems(const Value: integer);
begin
  if (FMaxItems <> Value) then
  begin
    FMaxItems := Value;

    if FMaxItems > 0 then
    begin
      while Values.Count > FMaxItems do
        Values.Delete(Values.Count - 1);
    end;
  end;
end;

procedure TAdvListEditor.SetMaxLines(const Value: integer);
begin
  if (Value >= 0) then
  begin
    FMaxLines := Value;
    if (Value > 0) and (AutoSize) then
      DoAutoSize;
  end;
end;

procedure TAdvListEditor.SetMinLines(const Value: integer);
begin
  if (Value >= 0) then
  begin
    FMinLines := Value;
    if (Value > 0) and (AutoSize) then
      DoAutoSize;
  end;
end;

procedure TAdvListEditor.SetReadOnly(const Value: boolean);
begin
  if (FReadOnly <> Value) then
  begin
    FReadOnly := Value;
    Invalidate;
  end;
end;

procedure TAdvListEditor.SetVersion(const Value: string);
begin

end;

procedure TAdvListEditor.UpdateEditPos(Index: integer);
var
  pt: TPoint;
begin
  ItemIndex := -1;
  FEditPos := Index;

  pt := GetEditPos(Index,Width);

  FEdit.Left := pt.X + FEditOffset;
//  FEdit.Left := pt.X - 2;
//  FEdit.Left := pt.X + 2 + (Appearance.Selected.Rounding div 2);
  FEdit.Top := pt.Y + 1;

  if (FEdit.Top + FEdit.Height > Height) then
  begin
    if AutoSize then
    begin
      DoAutoSize;
    end
    else
    begin
      FEditPos := 0;
      pt := GetEditPos(Index,Width);

      FEdit.Left := pt.X;
      FEdit.Top := pt.Y + 1;
    end;
  end;

  if not (csDesigning in ComponentState) then
  begin
    if FEdit.Width = 2 then
      FEdit.Width := 10;

//    if (Index < Values.Count) and (FEdit.Text = '') then
//      FEdit.Width := 10
//    else
//      FEdit.Width := Width - FEdit.Left;
  end;

  if BorderStyle = bsSingle then
  begin
    if FEdit.Left + FEdit.Width >= Width then
      FEdit.Width := Width - FEdit.Left - 1;
  end;

  if MaxItems > 0 then
  begin
    if (Values.Count >= MaxItems) then
      FEdit.Width := 0
    else
    begin
      if FEdit.Width = 0 then
        FEdit.Width := 10;
    end;
  end;

  Invalidate;
end;

function TAdvListEditor.ValueAtXY(X, Y: integer): integer;
var
  i,dx,dy,dw,th: integer;
begin
  Result := -1;

  Canvas.Font.Assign(Font);

  th := ItemHeight;

  dx := LEFTOFFSET;
  dy := TOPOFFSET;

  for i := 0 to Values.Count - 1 do
  begin
    dw := Values[i].GetWidth(Canvas) + ITEMMARGIN;

    if i = FEditPos then
      dx := dx + EditWidth;

    if dx + dw - ITEMMARGIN + 4 >= Width then
    begin
      dx := LEFTOFFSET;
      dy := dy + th + LINEMARGIN;
    end;

    if (x >= dx) and (x <= dx + dw - 4) and (y > dy) and (y < dy + ITEMHEIGHT) then
    begin
      // hit an item
      Result := i;
      Exit;
    end;

    dx := dx + dw;
  end;
end;

function TAdvListEditor.ValueRect(Index: integer): TRect;
var
  i,dx,dy,dw,th: integer;
begin
  Result := Rect(0,0,0,0);

  Canvas.Font.Assign(Font);

  th := ItemHeight;

  dx := LEFTOFFSET;
  dy := TOPOFFSET;

  for i := 0 to Values.Count - 1 do
  begin
    dw := Values[i].GetWidth(Canvas) + ITEMMARGIN;

    if i = FEditPos then
      dx := dx + EditWidth;

    if dx + dw > Width then
    begin
      dx := LEFTOFFSET;
      dy := dy + th + LINEMARGIN;
    end;

    if i = Index then
    begin
      Result := Rect(dx, dy, dx + dw, dy + th + LINEMARGIN);
      break;
    end;

    dx := dx + dw;
  end;
end;

{ TAdvListValues }

function TAdvListValues.Add: TAdvListValue;
begin
  Result := TAdvListValue(inherited Add);
end;

function TAdvListValues.AddPair(DisplayText, Value: string): TAdvListValue;
begin
  Result := AddPair(DisplayText,Value,-1);
end;

function TAdvListValues.AddPair(DisplayText, Value: string;
  ImageIndex: integer): TAdvListValue;
var
  lv: TAdvListValue;
begin
  lv := Add;
  lv.DisplayText := DisplayText;
  lv.Value := Value;
  lv.ImageIndex := ImageIndex;
  Result := lv;
end;

procedure TAdvListValues.Changed;
begin
  if Assigned(OnChange) and (UpdateCount = 0) then
    OnChange(Self);
end;

constructor TAdvListValues.Create(AOwner: TComponent);
begin
  inherited Create(AOwner,TAdvListValue);
end;

destructor TAdvListValues.Destroy;
begin

  inherited;
end;

function TAdvListValues.GetItem(Index: integer): TAdvListValue;
begin
  Result := TAdvListValue(inherited Items[Index]);
end;

function TAdvListValues.Insert(Index: integer): TAdvListValue;
begin
  Result := TAdvListValue(inherited Insert(Index));
end;

procedure TAdvListValues.SetItem(Index: integer; const Value: TAdvListValue);
begin
  inherited Items[Index] := Value;
end;

procedure TAdvListValues.Update(Item: TCollectionItem);
begin
  inherited;
  Changed;
end;

{ TAdvListValue }

procedure TAdvListValue.Assign(Source: TPersistent);
begin
  if (Source is TAdvListValue) then
  begin
    FDisplayText := (Source as TAdvListValue).DisplayText;
    FImageIndex := (Source as TAdvListValue).ImageIndex;
    FValue := (Source as TAdvListValue).Value;
    FTag := (Source as TAdvListValue).Tag;
  end;
end;

procedure TAdvListValue.Changed;
begin
  if Assigned(Collection) then
    (Collection as TAdvListValues).Changed;
end;

constructor TAdvListValue.Create(Collection: TCollection);
begin
  inherited;
  FTag := 0;
  FImageIndex := -1;
end;

function TAdvListValue.GetWidth(Canvas: TCanvas): integer;
var
  s: string;
  imglist: TCustomImageList;
  le: TAdvListEditor;

begin
  le := TAdvListEditor((Collection as TAdvListValues).Owner);

  s := DisplayText;
  if s = '' then
    s := 'ww';

  Result := Canvas.TextWidth(s) + (le.Appearance.Normal.Rounding div 2);

  imglist := le.Images;

  if (FImageIndex >= 0) and Assigned(imglist) then
  begin
    Result := Result + imglist.Width + IMAGEMARGIN;
  end;
end;

procedure TAdvListValue.SetDisplayText(const AValue: string);
begin
  if (FDisplayText <> AValue) then
  begin
    FDisplayText := AValue;
    Changed;
  end;
end;

procedure TAdvListValue.SetImageIndex(const Value: integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TAdvListValue.SetValue(const AValue: string);
begin
  if (FValue <> AValue) then
  begin
    FValue := AValue;
    Changed;
  end;
end;

{ TItemAppearance }

procedure TItemAppearance.Assign(Source: TPersistent);
begin
  if (Source is TItemAppearance) then
  begin
    FBorderColor := (Source as TItemAppearance).BorderColor;
    FColorFrom := (Source as TItemAppearance).ColorFrom;
    FColorTo := (Source as TItemAppearance).ColorTo;
    FRounding := (Source as TItemAppearance).Rounding;
    FTextColor := (Source as TItemAppearance).TextColor;
  end;
end;

constructor TItemAppearance.Create;
begin
  inherited;
  FRounding := 6;
  FTextColor := clBlack;
end;

procedure TItemAppearance.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TItemAppearance.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    DoChange;
  end;
end;

procedure TItemAppearance.SetColorFrom(const Value: TColor);
begin
  if (FColorFrom <> Value) then
  begin
    FColorFrom := Value;
    DoChange;
  end;
end;

procedure TItemAppearance.SetColorTo(const Value: TColor);
begin
  if (FColorTo <> Value) then
  begin
    FColorTo := Value;
    DoChange;
  end;
end;

procedure TItemAppearance.SetRounding(const Value: integer);
begin
  if (FRounding <> Value) then
  begin
    FRounding := Value;
    DoChange;
  end;
end;

procedure TItemAppearance.SetTextColor(const Value: TColor);
begin
  if (FTextColor <> Value) then
  begin
    FTextColor := Value;
    DoChange;
  end;
end;

{ TAppearance }

procedure TAppearance.Assign(Source: TPersistent);
begin
  if (Source is TAppearance) then
  begin
    FNormal.Assign((Source as TAppearance).Normal);
    FSelected.Assign((Source as TAppearance).Selected);
  end;
end;

constructor TAppearance.Create;
begin
  inherited;
  FNormal := TItemAppearance.Create;
  FNormal.OnChange := ItemAppearanceChange;

  FSelected := TItemAppearance.Create;
  FSelected.OnChange := ItemAppearanceChange;
end;

destructor TAppearance.Destroy;
begin
  FNormal.Free;
  FSelected.Free;
  inherited;
end;

procedure TAppearance.ItemAppearanceChange(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAppearance.SetNormal(const Value: TItemAppearance);
begin
  FNormal.Assign(Value);
end;

procedure TAppearance.SetSelected(const Value: TItemAppearance);
begin
  FSelected.Assign(Value);
end;

{ TLookupPopup }

procedure TLookupPopup.Assign(Source: TPersistent);
begin
  if (Source is TLookupPopup) then
  begin
    FColor := (Source as TLookupPopup).Color;
    FCount := (Source as TLookupPopup).Count;
    FCaseSensitive := (Source as TLookupPopup).CaseSensitive;
    FFont.Assign((Source as TLookupPopup).Font);
    FValueSeparator := (Source as TLookupPopup).ValueSeparator;
  end;
end;

procedure TLookupPopup.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TLookupPopup.Create;
begin
  inherited;
  FColor := clWindow;
  FCount := 6;
  FFromChar := 1;
  FCaseSensitive := false;
  FEnabled := true;
  FFont := TFont.Create;
  FFont.Name := 'Arial';
  FFont.Style := [];
  FFont.Size := 8;
  FFont.OnChange := FontChanged;
  FValueSeparator := vsBracket;
end;

destructor TLookupPopup.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TLookupPopup.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TLookupPopup.SetCaseSensitive(const Value: boolean);
begin
  if (FCaseSensitive <> Value) then
  begin
    FCaseSensitive := Value;
    Changed;
  end;
end;

procedure TLookupPopup.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TLookupPopup.SetCount(const Value: integer);
begin
  if (FCount <> Value) then
  begin
    FCount := Value;
    Changed;
  end;
end;

procedure TLookupPopup.SetEnabled(const Value: boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TLookupPopup.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TLookupPopup.SetFromChar(const Value: integer);
begin
  if (Value >= 1) and (FFromChar <> Value) then
  begin
    FFromChar := Value;
    Changed;
  end;
end;

procedure TLookupPopup.SetSpacing(const Value: integer);
begin
  if (FSpacing <> Value) then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

procedure TLookupPopup.SetValueSeparator(const Value: TValueSeparator);
begin
  if (FValueSeparator <> Value) then
  begin
    FValueSeparator := Value;
    Changed;
  end;
end;

end.
