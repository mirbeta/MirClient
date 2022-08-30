{***************************************************************************}
{ TAdvWordCloud component                                                   }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012 - 2014                                        }
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

unit AdvWordCloud;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 2; // Build nr.

  // Version history
  // v1.0.0.0 : First release
  // v1.0.1.0 : New : Functions AdvWordCloud.Words.IndexOfText/AdvWordCloud.Words.IndexOfValue added
  // v1.0.2.0 : New : Tag property added in TWordCategory.
  // v1.0.2.1 : Fixed : Issue with word detection
  // v1.0.2.2 : Fixed : Issue with deleting all categories at designtime
  //          : Fixed : Initialization WordIndex public property

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TDrawState = (dsNormal, dsHover, dsDown, dsSelected);

  TAdvWord = class(TCollectionItem)
  private
    FTag: integer;
    FValue: string;
    FDisplayText: string;
    FImageIndex: integer;
    FSelected: boolean;
    FHint: string;
    procedure SetDisplayText(const AValue: string);
    procedure SetValue(const AValue: string);
    procedure SetImageIndex(const Value: integer);
    procedure SetSelected(const Value: boolean);
  protected
     procedure Changed; virtual;
     function GetWidth(Canvas: TCanvas): integer;
     function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property DisplayText: string read FDisplayText write SetDisplayText;
    property Hint: string read FHint write FHint;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Selected: boolean read FSelected write SetSelected;
    property Tag: integer read FTag write FTag default 0;
    property Value: string read FValue write SetValue;
  end;

  TAdvWords = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItem(Index: integer): TAdvWord;
    procedure SetItem(Index: integer; const Value: TAdvWord);
  protected
    procedure Changed; virtual;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    property Items[Index: integer]: TAdvWord read GetItem write SetItem; default;
    function Add: TAdvWord;
    function Insert(Index: integer): TAdvWord;
    procedure AddPair(DisplayText, Value: string); overload;
    procedure AddPair(DisplayText, Value: string; ImageIndex: integer); overload;
    function IndexOfText(AText: string): integer;
    function IndexOfValue(AValue: string): integer;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TWordAppearance = class(TPersistent)
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
    FNormal: TWordAppearance;
    FDown: TWordAppearance;
    FHover: TWordAppearance;
    FOnChange: TNotifyEvent;
    FSelected: TWordAppearance;
    procedure SetNormal(const Value: TWordAppearance);
    procedure SetDown(const Value: TWordAppearance);
    procedure SetHover(const Value: TWordAppearance);
    procedure WordAppearanceChange(Sender: TObject);
    procedure SetSelected(const Value: TWordAppearance);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Normal: TWordAppearance read FNormal write SetNormal;
    property Down: TWordAppearance read FDown write SetDown;
    property Hover: TWordAppearance read FHover write SetHover;
    property Selected: TWordAppearance read FSelected write SetSelected;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  TWordSelection = (wsNone, wsSingle, wsMulti);

  TWordEvent = procedure(Sender: TObject; Index: integer) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvWordCloud = class(TCustomControl)
  private
    FMouseDown: boolean;
    FAppearance: TAppearance;
    FImages: TCustomImageList;
    FWords: TAdvWords;
    FBorderColor: TColor;
    FBorderStyle: TBorderStyle;
    FHoverIndex: integer;
    FDownIndex: integer;
    FSelection: TWordSelection;
    FHintIndex: integer;
    FWordIndex: integer;
    FFocusIndex: integer;
    FAutoSize: boolean;
    FMaxLines: integer;
    FOnWordSelect: TWordEvent;
    FOnWordUnSelect: TWordEvent;
    FOnWordClick: TWordEvent;
    procedure WMEraseBkGnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetAppearance(const Value: TAppearance);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetWords(const Value: TAdvWords);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetWordIndex(const Value: integer);
    procedure SetAutoSizeEx(const Value: boolean);
    procedure SetMaxLines(const Value: integer);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key:Word;Shift:TShiftSTate); override;
    procedure KeyUp(var Key:Word;Shift:TShiftSTate); override;
    procedure DoAutoSize; virtual;
    procedure DoSelect(const Index: integer); virtual;
    procedure DoUnSelect(const Index: integer); virtual;
    procedure DoClick(const Index: integer); virtual;
    procedure AppearanceChange(Sender: TObject); virtual;
    procedure WordsChange(Sender: TObject); virtual;
    procedure DrawItem(ACanvas: TCanvas; R: TRect; AItem: TAdvWord; State: TDrawState); virtual;
    procedure Paint; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    function GetVersion: string; virtual;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ValuePos(Index: integer): TPoint;
    function ValueAtXY(X,Y: integer): integer;
    function ValueRect(Index: integer): TRect;
    property WordIndex: integer read FWordIndex write SetWordIndex;
  published
    property Align;
    property AlignWithMargins;
    property Anchors;
    property Appearance: TAppearance read FAppearance write SetAppearance;
    property AutoSize: boolean read FAutoSize write SetAutoSizeEx default true;
    property BorderColor: TColor read FBorderColor write SetBorderColor default $00B99D7F;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color;
    property Constraints;
    property Font;

    property Images: TCustomImageList read FImages write SetImages;
    property Margins;
    property MaxLines: integer read FMaxLines write SetMaxLines default 0;
    property Selection: TWordSelection read FSelection write FSelection default wsNone;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Version: string read GetVersion write SetVersion;
    property Visible;
    property Words: TAdvWords read FWords write SetWords;

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
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnWordClick: TWordEvent read FOnWordClick write FOnWordClick;
    property OnWordSelect: TWordEvent read FOnWordSelect write FOnWordSelect;
    property OnWordUnSelect: TWordEvent read FOnWordUnSelect write FOnWordUnSelect;
  end;

  TWordCategory = class(TCollectionItem)
  private
    FCategory: string;
    FWords: TAdvWords;
    FAppearance: TAppearance;
    FColor: TColor;
    FWordIndex: integer;
    FTag: integer;
    procedure SetCategory(const Value: string);
    procedure SetWords(const Value: TAdvWords);
    procedure SetAppearance(const Value: TAppearance);
    procedure SetColor(const Value: TColor);
    procedure SetWordIndex(const Value: integer);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property WordIndex: integer read FWordIndex write SetWordIndex;
  published
    property Appearance: TAppearance read FAppearance write SetAppearance;
    property Color: TColor read FColor write SetColor;
    property Category: string read FCategory write SetCategory;
    property Words: TAdvWords read FWords write SetWords;
    property Tag: integer read FTag write FTag;
  end;

  TWordCategories = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItem(Index: integer): TWordCategory;
    procedure SetItem(Index: integer; const Value: TWordCategory);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: integer]: TWordCategory read GetItem write SetItem; default;
    function Add: TWordCategory;
    function Insert(Index: integer): TWordCategory;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCategoryHeader = class(TPersistent)
  private
    FHeight: integer;
    FColor: TColor;
    FFont: TFont;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetHeight(const Value: integer);
  protected
    procedure FontChanged(Sender: TObject);
    procedure Changed; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clHighlight;
    property Font: TFont read FFont write SetFont;
    property Height: integer read FHeight write SetHeight;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  TCategorySelection = (csClick, csHover);

  TCategoryWordEvent = procedure(Sender: TObject; CategoryIndex, WordIndex: Integer) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvWordCategoryCloud = class(TCustomControl)
  private
    FCategories: TWordCategories;
    FCategoryIndex: integer;
    FDesignTime: boolean;
    FHoverIndex: integer;
    FDownIndex: integer;
    FHintIndex: integer;
    FFocusIndex: Integer;
    FBorderColor: TColor;
    FSelection: TWordSelection;
    FBorderStyle: TBorderStyle;
    FImages: TCustomImageList;
    FMouseDown: boolean;
    FHeader: TCategoryHeader;
    FCategorySelection: TCategorySelection;
    FOnWordClick: TCategoryWordEvent;
    FOnWordSelect: TCategoryWordEvent;
    FOnWordUnSelect: TCategoryWordEvent;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetCategories(const Value: TWordCategories);
    procedure SetCategoryIndex(const Value: integer);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetHeader(const Value: TCategoryHeader);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key:Word;Shift:TShiftSTate); override;
    procedure KeyUp(var Key:Word;Shift:TShiftSTate); override;
    procedure CategoriesChange(Sender: TObject);
    procedure Paint; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DrawItem(ACanvas: TCanvas; R: TRect; AItem: TAdvWord; State: TDrawState); virtual;
    procedure DoSelect(const Index: integer); virtual;
    procedure DoUnSelect(const Index: integer); virtual;
    procedure DoClick(const Index: integer); virtual;
    function GetVersion: string; virtual;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CategoryAtXY(X,Y: integer): integer;
    function ValuePos(Index: integer): TPoint;
    function ValueAtXY(X,Y: integer): integer;
    function ValueRect(Index: integer): TRect;
  published
    property Align;
    property AlignWithMargins;
    property Anchors;
    property BorderColor: TColor read FBorderColor write SetBorderColor default $00B99D7F;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color;
    property Constraints;
    property Categories: TWordCategories read FCategories write SetCategories;
    property CategoryIndex: integer read FCategoryIndex write SetCategoryIndex;
    property CategorySelection: TCategorySelection read FCategorySelection write FCategorySelection default csClick;
    property Font;
    property Header: TCategoryHeader read FHeader write SetHeader;
    property Images: TCustomImageList read FImages write SetImages;
    property Margins;
    property Selection: TWordSelection read FSelection write FSelection;
    property ShowHint;
    property TabStop;
    property TabOrder;
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
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;

    property OnWordClick: TCategoryWordEvent read FOnWordClick write FOnWordClick;
    property OnWordSelect: TCategoryWordEvent read FOnWordSelect write FOnWordSelect;
    property OnWordUnSelect: TCategoryWordEvent read FOnWordUnSelect write FOnWordUnSelect;
  end;

implementation

uses
  Math, AdvGDIP;

const
  ITEMMARGIN = 10;
  LEFTOFFSET = 4;
  TOPOFFSET = 2;
  ITEMHEIGHT = 18;
  LINEMARGIN = 8;
  IMAGEMARGIN = 4;


{ TAdvWords }

function TAdvWords.Add: TAdvWord;
begin
  Result := TAdvWord(inherited Add);
end;

procedure TAdvWords.AddPair(DisplayText, Value: string);
begin
  AddPair(DisplayText,Value,-1);
end;

procedure TAdvWords.AddPair(DisplayText, Value: string;
  ImageIndex: integer);
var
  lv: TAdvWord;
begin
  lv := Add;
  lv.DisplayText := DisplayText;
  lv.Value := Value;
  lv.ImageIndex := ImageIndex;
end;

procedure TAdvWords.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TAdvWords.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner,TAdvWord);
end;

destructor TAdvWords.Destroy;
begin

  inherited;
end;

function TAdvWords.GetItem(Index: integer): TAdvWord;
begin
  Result := TAdvWord(inherited Items[Index]);
end;

function TAdvWords.IndexOfText(AText: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if (Items[i].DisplayText = AText) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TAdvWords.IndexOfValue(AValue: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if (Items[i].Value = AValue) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TAdvWords.Insert(Index: integer): TAdvWord;
begin
  Result := TAdvWord(inherited Insert(Index));
end;

procedure TAdvWords.SetItem(Index: integer; const Value: TAdvWord);
begin
  inherited Items[Index] := Value;
end;

procedure TAdvWords.Update(Item: TCollectionItem);
begin
  inherited;
  Changed;
end;

{ TAdvWord }

procedure TAdvWord.Assign(Source: TPersistent);
begin
  if (Source is TAdvWord) then
  begin
    FDisplayText := (Source as TAdvWord).DisplayText;
    FImageIndex := (Source as TAdvWord).ImageIndex;
    FSelected := (Source as TAdvWord).Selected;
    FTag := (Source as TAdvWord).Tag;
    FValue := (Source as TAdvWord).Value;
    FHint := (Source as TAdvWord).Hint;
    FTag := (Source as TAdvWord).Tag;
  end;
end;

procedure TAdvWord.Changed;
begin
  (Collection as TAdvWords).Changed;
end;

constructor TAdvWord.Create(Collection: TCollection);
begin
  inherited;
  FTag := 0;
  FImageIndex := -1;
end;

function TAdvWord.GetDisplayName: string;
begin
  if DisplayText <> '' then
    Result := DisplayText
  else
    Result := 'Item ' + inttostr(index);

end;

function TAdvWord.GetWidth(Canvas: TCanvas): integer;
var
  s: string;
  imglist: TCustomImageList;
  le: TAdvWordCloud;
  w: TAdvWords;
  wc: TWordCategories;

  app: TAppearance;
begin
  s := DisplayText;
  if s = '' then
    s := 'ww';

  imglist := nil;
  app := nil;

  if (Collection as TAdvWords).Owner is TAdvWordCloud then
  begin
    le := TAdvWordCloud((Collection as TAdvWords).Owner);
    app := le.Appearance;
    imglist := le.Images;
  end;

  if (Collection as TAdvWords).Owner is TWordCategory then
  begin
    w := (Collection as TAdvWords);

    app := TWordCategory(w.Owner).Appearance;

    wc := TWordCategories(TWordCategory(w.Owner).Collection);

    imglist := TAdvWordCategoryCloud(wc.Owner).Images;
  end;

  Result := Canvas.TextWidth(s);

  if Assigned(app) then
    Result := Result + (app.Normal.Rounding div 2);

  if (FImageIndex >= 0) and Assigned(imglist) then
  begin
    Result := Result + imglist.Width + IMAGEMARGIN;
  end;
end;


procedure TAdvWord.SetDisplayText(const AValue: string);
begin
  if (FDisplayText <> AValue) then
  begin
    FDisplayText := AValue;
    Changed;
  end;
end;

procedure TAdvWord.SetImageIndex(const Value: integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TAdvWord.SetSelected(const Value: boolean);
begin
  if (FSelected <> Value) then
  begin
    FSelected := Value;
    Changed;
  end;
end;

procedure TAdvWord.SetValue(const AValue: string);
begin
  if (FValue <> AValue) then
  begin
    FValue := AValue;
    Changed;
  end;
end;


{ TWordAppearance }

procedure TWordAppearance.Assign(Source: TPersistent);
begin
  if (Source is TWordAppearance) then
  begin
    FBorderColor := (Source as TWordAppearance).BorderColor;
    FColorFrom := (Source as TWordAppearance).ColorFrom;
    FColorTo := (Source as TWordAppearance).ColorTo;
    FRounding := (Source as TWordAppearance).Rounding;
    FTextColor := (Source as TWordAppearance).TextColor;
  end;
end;

constructor TWordAppearance.Create;
begin
  inherited;
  FRounding := 6;
  FTextColor := clBlack;
end;

procedure TWordAppearance.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TWordAppearance.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    DoChange;
  end;
end;

procedure TWordAppearance.SetColorFrom(const Value: TColor);
begin
  if (FColorFrom <> Value) then
  begin
    FColorFrom := Value;
    DoChange;
  end;
end;

procedure TWordAppearance.SetColorTo(const Value: TColor);
begin
  if (FColorTo <> Value) then
  begin
    FColorTo := Value;
    DoChange;
  end;
end;

procedure TWordAppearance.SetRounding(const Value: integer);
begin
  if (FRounding <> Value) then
  begin
    FRounding := Value;
    DoChange;
  end;
end;

procedure TWordAppearance.SetTextColor(const Value: TColor);
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
    FDown.Assign((Source as TAppearance).Down);
    FHover.Assign((Source as TAppearance).Hover);
  end;
end;

constructor TAppearance.Create;
begin
  inherited;
  FNormal := TWordAppearance.Create;
  FNormal.OnChange := WordAppearanceChange;

  FDown := TWordAppearance.Create;
  FDown.OnChange := WordAppearanceChange;

  FHover := TWordAppearance.Create;
  FHover.OnChange := WordAppearanceChange;

  FSelected := TWordAppearance.Create;
  FSelected.OnChange := WordAppearanceChange;
end;

destructor TAppearance.Destroy;
begin
  FNormal.Free;
  FDown.Free;
  FHover.Free;
  FSelected.Free;
  inherited;
end;

procedure TAppearance.WordAppearanceChange(Sender: TObject);
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAppearance.SetNormal(const Value: TWordAppearance);
begin
  FNormal.Assign(Value);
end;

procedure TAppearance.SetSelected(const Value: TWordAppearance);
begin
  FSelected.Assign(Value);
end;

procedure TAppearance.SetDown(const Value: TWordAppearance);
begin
  FDown.Assign(Value);
end;

procedure TAppearance.SetHover(const Value: TWordAppearance);
begin
  FHover.Assign(Value);
end;

{ TAdvWordCloud }

procedure TAdvWordCloud.AppearanceChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvWordCloud.CMHintShow(var Msg: TMessage);
var
  PHI: PHintInfo;
  idx: integer;
begin
  PHI := TCMHintShow(Msg).HintInfo;

  idx := ValueAtXY(PHI^.CursorPos.X, PHI^.CursorPos.Y);

  if idx <> -1 then
  begin
    FHintIndex := idx;
    PHI^.HintStr := Words[idx].Hint;
  end;
end;

procedure TAdvWordCloud.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (FHoverIndex <> -1) then
  begin
    FHoverIndex := -1;
    Invalidate;
  end;

end;

constructor TAdvWordCloud.Create(AOwner: TComponent);
begin
  inherited;
  FAppearance := TAppearance.Create;
  FAppearance.OnChange := AppearanceChange;

  FWords := TAdvWords.Create(Self);
  FWords.OnChange := WordsChange;

  FAppearance.Normal.ColorFrom := clWhite;
  FAppearance.Normal.ColorTo := clWhite;
  FAppearance.Normal.BorderColor := clWhite;
  FAppearance.Normal.TextColor := clBlack;

  FAppearance.Down.ColorFrom := RGB(115,163,230);
  FAppearance.Down.ColorTo := RGB(35,110,216);
  FAppearance.Down.BorderColor := RGB(35,110,216);
  FAppearance.Down.TextColor := clWhite;

  FAppearance.Hover.ColorFrom := RGB(220,230,248);
  FAppearance.Hover.ColorTo := RGB(189,207,241);
  FAppearance.Hover.BorderColor := RGB(120,133,215);
  FAppearance.Hover.TextColor := clBlack;

  FAppearance.Selected.ColorFrom := RGB(175,200,237);
  FAppearance.Selected.ColorTo := RGB(100,175,225);
  FAppearance.Selected.BorderColor := RGB(100,175,225);
  FAppearance.Selected.TextColor := clWhite;

  FBorderColor := $00B99D7F;
  FBorderStyle := bsSingle;
  Color := clWhite;
  FHoverIndex := -1;
  FDownIndex := -1;
  FHintIndex := -1;
  FFocusIndex := -1;
  FWordIndex := -1;
  FImages := nil;

  Width := 200;
  Height := 200;
  DoubleBuffered := true;
end;

destructor TAdvWordCloud.Destroy;
begin
  FAppearance.Free;
  FWords.Free;
  inherited;
end;

procedure TAdvWordCloud.DoAutoSize;
var
  pt: TPoint;
  NewHeight: integer;
begin
  pt := ValuePos(Words.Count);
  NewHeight := pt.Y + ItemHeight + LINEMARGIN;

  if (MaxLines > 0) and (NewHeight > (MaxLines + 1) * (ItemHeight + LINEMARGIN)) then
    Exit;

  Height := NewHeight;
end;

procedure TAdvWordCloud.DoClick(const Index: integer);
begin
  if Assigned(OnWordClick) then
    OnWordClick(Self, Index);
end;

procedure TAdvWordCloud.DoEnter;
begin
  inherited;
  Invalidate;
end;

procedure TAdvWordCloud.DoExit;
begin
  inherited;
  Invalidate;
end;

procedure TAdvWordCloud.DoSelect(const Index: integer);
begin
  if Assigned(OnWordSelect) then
    OnWordSelect(Self, Index);
end;

procedure TAdvWordCloud.DoUnSelect(const Index: integer);
begin
  if Assigned(OnWordUnSelect) then
    OnWordUnSelect(Self, Index);
end;

procedure TAdvWordCloud.DrawItem(ACanvas: TCanvas; R: TRect; AItem: TAdvWord;
  State: TDrawState);
var
  gp: TGPGraphics;
  gpBrush: TGPLinearGradientBrush;
  gpPen: TGPPen;
  gprect: TGPRectF;
  gpPath: TGPGraphicsPath;
  clrfrom,clrto,clrbrdr: TColor;
begin
  gp := TGPGraphics.Create(ACanvas.Handle);

  try
    gpRect := MakeRect(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);

    gp.SetSmoothingMode(SmoothingModeAntiAlias);

    gpPath := CreateRoundRectangle(R, Appearance.Normal.Rounding);

    case State of
    dsDown:
      begin
        clrfrom := Appearance.Down.ColorFrom;
        clrto := Appearance.Down.ColorTo;
        clrbrdr := Appearance.Down.BorderColor;
      end;
    dsHover:
      begin
        clrfrom := Appearance.Hover.ColorFrom;
        clrto := Appearance.Hover.ColorTo;
        clrbrdr := Appearance.Hover.BorderColor;
      end;
    dsSelected:
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

  case State of
  dsNormal:
    Canvas.Font.Color := Appearance.Normal.TextColor;
  dsDown:
    Canvas.Font.Color := Appearance.Down.TextColor;
  dsHover:
    Canvas.Font.Color := Appearance.Hover.TextColor;
  dsSelected:
    Canvas.Font.Color := Appearance.Selected.TextColor;
  end;

  if (AItem.ImageIndex >= 0) and Assigned(Images) then
  begin
    Images.Draw(Canvas, R.Left + 2 + (Appearance.Normal.Rounding div 2), R.Top + 2, AItem.ImageIndex);
    R.Left := R.Left + Images.Width + IMAGEMARGIN;
  end;

  Canvas.Brush.Style := bsClear;

  R.Left := R.Left + 2 + (Appearance.Normal.Rounding div 2);

  DrawText(Canvas.Handle, PChar(AItem.DisplayText), Length(AItem.DisplayText), R, DT_VCENTER or DT_SINGLELINE);
end;

function TAdvWordCloud.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) +
    '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvWordCloud.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvWordCloud.KeyDown(var Key: Word; Shift: TShiftSTate);
begin
  inherited;
  if (Key in [VK_UP, VK_LEFT]) then
  begin
    if FFocusIndex > 0 then
      dec(FFocusIndex)
    else
      FFocusIndex := Words.Count - 1;

    Invalidate;
  end;

  if (Key in [VK_DOWN, VK_RIGHT]) then
  begin
    if FFocusIndex < Words.Count - 1 then
      inc(FFocusIndex)
    else
      FFocusIndex := 0;

    Invalidate;
  end;

  if (Key = VK_SPACE) then
  begin
    FDownIndex := FFocusIndex;
    Invalidate;
  end;


  if FFocusIndex < 0 then
  begin
    FFocusIndex := 0;
    Invalidate;
  end;
end;

procedure TAdvWordCloud.KeyUp(var Key: Word; Shift: TShiftSTate);
begin
  inherited;

  if (FDownIndex >= 0) then
  begin
    if Selection = wsSingle then
    begin
      if FWordIndex <> FFocusIndex then
      begin
        FWordIndex := FFocusIndex;
        DoSelect(FWordIndex);
      end
      else
      begin
        FWordIndex := -1;
        DoUnSelect(FFocusIndex);
      end;
    end;
    if Selection = wsMulti then
    begin
      Words[FDownIndex].Selected := not Words[FDownIndex].Selected;
      if Words[FDownIndex].Selected then
        DoSelect(FDownIndex)
      else
        DoUnSelect(FDownIndex);
    end;

    FDownIndex := -1;
    Invalidate;
  end;
end;

procedure TAdvWordCloud.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  idx: integer;
begin
  inherited;

  idx := ValueAtXY(X,Y);

  if (idx <> FDownIndex) then
  begin
    FMouseDown := true;
    FDownIndex := idx;

    if Selection = wsSingle then
    begin
      if (FWordIndex <> idx) then
      begin
        DoUnSelect(FWordIndex);
      end;

      FWordIndex := idx;
      DoSelect(idx);
    end;
    Invalidate;
  end;

  if (Selection = wsMulti) and (idx <> -1) then
  begin
    FMouseDown := true;
    Words[idx].Selected := not Words[idx].Selected;
    if Words[idx].Selected then
      DoSelect(idx)
    else
      DoUnSelect(idx);
  end;
end;

procedure TAdvWordCloud.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  idx: integer;
begin
  inherited;

  idx := ValueAtXY(X,Y);

  if (FHintIndex <> - 1) and (idx <> FHintIndex) then
  begin
    Application.CancelHint;
    FHintIndex := -1;
  end;

  if (idx <> FHoverIndex) then
  begin
    FHoverIndex := idx;
    Invalidate;
  end;
end;

procedure TAdvWordCloud.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  idx: integer;
begin
  inherited;

  idx := ValueAtXY(X,Y);

  if FMouseDown then
  begin
    FMouseDown := false;
    Invalidate;

    if (idx = FDownIndex) then
      DoClick(idx);
  end;

  if (FDownIndex <> -1) then
  begin
    FDownIndex := -1;
    Invalidate;
  end;

end;

procedure TAdvWordCloud.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil;

  inherited;
end;

//------------------------------------------------------------------------------

procedure DrawFocusRect(ACanvas: TCanvas; R: TRect; Clr: TColor);
var
  LB: TLogBrush;
  HPen, HOldPen: THandle;
begin
  ACanvas.Pen.Color := Clr;

  lb.lbColor := ColorToRGB(Clr);
  lb.lbStyle := bs_Solid;

  HPen := ExtCreatePen(PS_COSMETIC or PS_ALTERNATE,1, lb, 0, nil);
  HOldPen := SelectObject(ACanvas.Handle, HPen);

  Windows.MoveToEx(ACanvas.Handle, R.Left, R.Top, nil);
  Windows.LineTo(ACanvas.Handle, R.Right, R.Top);

  Windows.MoveToEx(ACanvas.Handle, R.Right, R.Top, nil);
  Windows.LineTo(ACanvas.Handle, R.Right, R.Bottom);

  Windows.MoveToEx(ACanvas.Handle, R.Right, R.Bottom, nil);
  Windows.LineTo(ACanvas.Handle, R.Left, R.Bottom);

  Windows.MoveToEx(ACanvas.Handle, R.Left, R.Top, nil);
  Windows.LineTo(ACanvas.Handle, R.Left, R.Bottom);

  DeleteObject(SelectObject(ACanvas.Handle,HOldPen));
end;

procedure TAdvWordCloud.Paint;
var
  i: integer;
  dx,dy,dw: integer;
  fh: integer;
  R: TRect;
  state: TDrawState;
begin
  inherited;

  Canvas.Font.Assign(Font);

  fh := ItemHeight;

  dx := LEFTOFFSET;
  dy := TOPOFFSET;

  if BorderStyle = bsSingle then
    inc(dy);

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;

  Canvas.Rectangle(ClientRect);

  for i := 0 to Words.Count - 1 do
  begin
    dw := Words[i].GetWidth(Canvas) + ITEMMARGIN;

    if dx + dw - ITEMMARGIN + 4 >= Width then
    begin
      dx := LEFTOFFSET;
      dy := dy + fh + LINEMARGIN;
    end;

    state := dsNormal;

    if i = FHoverIndex then
      state := dsHover;

    if i = FDownIndex then
      state := dsDown;

    if not ( (state = dsDown) and FMouseDown) then
    begin
      case Selection of
      wsSingle:
        if i = FWordIndex then
          state := dsSelected;
      wsMulti:
        if Words[i].Selected then
          state := dsSelected;
      end;
    end;

    DrawItem(Canvas, Rect(dx, dy, dx + dw - ITEMMARGIN + 4, dy + fh + 2), Words[i], state);

    dx := dx + dw;
  end;

  if (FFocusIndex >= 0) and (GetFocus = Handle) and (Words.Count > 0) then
  begin
    R := ValueRect(FFocusIndex);
    R.Top := R.Top + 4;
    R.Left := R.Left + 4;
    R.Bottom := R.Top + Canvas.TextHeight('gh');
    R.Right := R.Right - ITEMMARGIN;
    DrawFocusRect(Canvas,R,clBlack);
  end;

  if (BorderStyle = bsSingle) then
  begin
    R := ClientRect;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := FBorderColor;
    Canvas.Pen.Style := psSolid;
    Canvas.Rectangle(R);
  end;
end;

procedure TAdvWordCloud.SetAppearance(const Value: TAppearance);
begin
  FAppearance.Assign(Value);
end;

procedure TAdvWordCloud.SetAutoSizeEx(const Value: boolean);
begin
  if (FAutoSize <> Value) then
  begin
    FAutoSize := Value;
    if Value then
      DoAutoSize;
  end;
end;

procedure TAdvWordCloud.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TAdvWordCloud.SetBorderStyle(const Value: TBorderStyle);
begin
  if (FBorderStyle <> Value) then
  begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

procedure TAdvWordCloud.SetImages(const Value: TCustomImageList);
begin
  if (FImages <> Value) then
  begin
    FImages := Value;
    Invalidate;
  end;
end;

procedure TAdvWordCloud.SetMaxLines(const Value: integer);
begin
  FMaxLines := Value;
end;

procedure TAdvWordCloud.SetVersion(const Value: string);
begin

end;

procedure TAdvWordCloud.SetWordIndex(const Value: integer);
begin
  if (FWordIndex <> Value) then
  begin
    FWordIndex := Value;
    Invalidate;
  end;
end;

procedure TAdvWordCloud.SetWords(const Value: TAdvWords);
begin
  FWords.Assign(Value);
end;

function TAdvWordCloud.ValueAtXY(X, Y: integer): integer;
var
  i,dx,dy,dw,th: integer;
begin
  Result := -1;

  Canvas.Font.Assign(Font);

  th := ItemHeight;

  dx := LEFTOFFSET;
  dy := TOPOFFSET;

  for i := 0 to Words.Count - 1 do
  begin
    dw := Words[i].GetWidth(Canvas) + ITEMMARGIN;

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

function TAdvWordCloud.ValuePos(Index: integer): TPoint;
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

    if (j < Words.Count) then
    begin
      tw := Words[j].GetWidth(Canvas) + ITEMMARGIN;

      if dx + tw - ITEMMARGIN + 4 >= Width then
      //if dx + tw >= Width then
      begin
        dx := LEFTOFFSET + tw;
        dy := dy + th + LINEMARGIN;
      end
      else
        dx := dx + tw;
    end;
  end;

  Result := Point(dx,dy);
end;

function TAdvWordCloud.ValueRect(Index: integer): TRect;
var
  i,dx,dy,dw,th: integer;
begin
  Result := Rect(0,0,0,0);

  Canvas.Font.Assign(Font);

  th := ItemHeight;

  dx := LEFTOFFSET;
  dy := TOPOFFSET;

  for i := 0 to Words.Count - 1 do
  begin
    dw := Words[i].GetWidth(Canvas) + ITEMMARGIN;

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

procedure TAdvWordCloud.WMEraseBkGnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TAdvWordCloud.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS;
end;

procedure TAdvWordCloud.WordsChange(Sender: TObject);
begin
  Invalidate;
end;

{ TWordCategory }

procedure TWordCategory.Assign(Source: TPersistent);
begin
  if (Source is TWordCategory) then
  begin
    FAppearance.Assign((Source as TWordCategory).Appearance);
    FColor := (Source as TWordCategory).Color;
    FCategory := (Source as TWordCategory).Category;
    FWords.Assign((Source as TWordCategory).Words);
    FTag := (Source as TWordCategory).Tag;
  end;
end;

constructor TWordCategory.Create(Collection: TCollection);
begin
  inherited;
  FWords := TAdvWords.Create(Self);
  FAppearance := TAppearance.Create;

  FAppearance.Normal.ColorFrom := clWhite;
  FAppearance.Normal.ColorTo := clWhite;
  FAppearance.Normal.BorderColor := clWhite;
  FAppearance.Normal.TextColor := clBlack;

  FAppearance.Down.ColorFrom := RGB(115,163,230);
  FAppearance.Down.ColorTo := RGB(35,110,216);
  FAppearance.Down.BorderColor := RGB(35,110,216);
  FAppearance.Down.TextColor := clWhite;

  FAppearance.Hover.ColorFrom := RGB(220,230,248);
  FAppearance.Hover.ColorTo := RGB(189,207,241);
  FAppearance.Hover.BorderColor := RGB(120,133,215);
  FAppearance.Hover.TextColor := clBlack;

  FAppearance.Selected.ColorFrom := RGB(175,200,237);
  FAppearance.Selected.ColorTo := RGB(100,175,225);
  FAppearance.Selected.BorderColor := RGB(100,175,225);
  FAppearance.Selected.TextColor := clWhite;

  FColor := clWhite;
  FWordIndex := -1;
end;

destructor TWordCategory.Destroy;
begin
  FWords.Free;
  FAppearance.Free;
  inherited;
end;

function TWordCategory.GetDisplayName: string;
begin
  if Category <> '' then
    Result := Category
  else
    Result := 'Category ' + inttostr(Index);
end;

procedure TWordCategory.SetAppearance(const Value: TAppearance);
begin
  FAppearance.Assign(Value);
end;

procedure TWordCategory.SetCategory(const Value: string);
begin
  if (FCategory <> Value) then
  begin
    FCategory := Value;
    Changed(true);
  end;
end;

procedure TWordCategory.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed(true);
  end;
end;

procedure TWordCategory.SetWordIndex(const Value: integer);
begin
  if (FWordIndex <> Value) and (Value < Words.Count) then
  begin
    FWordIndex := Value;
  end;
end;

procedure TWordCategory.SetWords(const Value: TAdvWords);
begin
  FWords.Assign(Value);
end;

{ TAdvWordCategoryCloud }

procedure TAdvWordCategoryCloud.CategoriesChange(Sender: TObject);
begin
  Invalidate;
end;

function TAdvWordCategoryCloud.CategoryAtXY(X, Y: integer): integer;
var
  i: integer;
  CR, DR: TRect;
begin
  Result := -1;

  CR := ClientRect;
  DR := CR;
  DR.Left := LEFTOFFSET;

  if (Y >= 0) and (Y < FHeader.Height) then
  begin
    Canvas.Font.Assign(FHeader.Font);

    for i := 0 to Categories.Count - 1 do
    begin
      DrawText(Canvas.Handle, PChar(Categories[i].Category), Length(Categories[i].Category), DR, DT_VCENTER or DT_SINGLELINE or DT_CALCRECT);

      if (X >= DR.Left) and (X <= DR.Right) then
      begin
        Result := i;
        Break;
      end;

      DR.Left := DR.Right + ITEMMARGIN;
      DR.Right := Width;
      DR.Top := CR.Top;
      DR.Bottom := CR.Bottom;
    end;
  end;
end;

procedure TAdvWordCategoryCloud.CMHintShow(var Msg: TMessage);
var
  PHI: PHintInfo;
  idx: integer;
begin
  PHI := TCMHintShow(Msg).HintInfo;

  idx := ValueAtXY(PHI^.CursorPos.X, PHI^.CursorPos.Y);

  if (idx <> -1) then
  begin
    FHintIndex := idx;
    PHI^.HintStr := Categories[CategoryIndex].Words[idx].Hint;
  end;
end;

procedure TAdvWordCategoryCloud.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (FHoverIndex <> -1) then
  begin
    FHoverIndex := -1;
    Invalidate;
  end;

end;

constructor TAdvWordCategoryCloud.Create(AOwner: TComponent);
begin
  inherited;
  FCategories := TWordCategories.Create(Self);
  FCategories.OnChange := CategoriesChange;
  FHeader := TCategoryHeader.Create;
  FHeader.OnChange := CategoriesChange;

  Width := 200;
  Height := 200;
  DoubleBuffered := true;
  FCategoryIndex := 0;
  FBorderStyle := bsSingle;
  FBorderColor := $00B99D7F;
  FDownIndex := -1;
  FHoverIndex := -1;
  FHintIndex := -1;

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime and (FCategories.Count = 0) then
  begin
    FCategories.Add.Category := 'Category 0';
    FCategories[0].Words.Add.DisplayText := 'Item 0';
  end;
end;

destructor TAdvWordCategoryCloud.Destroy;
begin
  FCategories.Free;
  FHeader.Free;
  inherited;
end;

procedure TAdvWordCategoryCloud.DoClick(const Index: integer);
begin
  if Assigned(FOnWordClick) then
    FOnWordClick(Self, CategoryIndex, Index);
end;

procedure TAdvWordCategoryCloud.DoEnter;
begin
  inherited;
  Invalidate;
end;

procedure TAdvWordCategoryCloud.DoExit;
begin
  inherited;
  Invalidate;
end;

procedure TAdvWordCategoryCloud.DoSelect(const Index: integer);
begin
  if Assigned(FOnWordSelect) then
    FOnWordSelect(Self, CategoryIndex, Index);
end;

procedure TAdvWordCategoryCloud.DoUnSelect(const Index: integer);
begin
  if Assigned(FOnWordUnSelect) then
    FOnWordUnSelect(Self, CategoryIndex, Index);
end;

procedure TAdvWordCategoryCloud.DrawItem(ACanvas: TCanvas; R: TRect;
  AItem: TAdvWord; State: TDrawState);
var
  gp: TGPGraphics;
  gpBrush: TGPLinearGradientBrush;
  gpPen: TGPPen;
  gprect: TGPRectF;
  gpPath: TGPGraphicsPath;
  clrfrom,clrto,clrbrdr: TColor;
  Appearance: TAppearance;
begin
  gp := TGPGraphics.Create(ACanvas.Handle);

  try
    gpRect := MakeRect(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);

    gp.SetSmoothingMode(SmoothingModeAntiAlias);

    Appearance := Categories[CategoryIndex].Appearance;

    gpPath := CreateRoundRectangle(R, Appearance.Normal.Rounding);

    case State of
    dsDown:
      begin
        clrfrom := Appearance.Down.ColorFrom;
        clrto := Appearance.Down.ColorTo;
        clrbrdr := Appearance.Down.BorderColor;
      end;
    dsHover:
      begin
        clrfrom := Appearance.Hover.ColorFrom;
        clrto := Appearance.Hover.ColorTo;
        clrbrdr := Appearance.Hover.BorderColor;
      end;
    dsSelected:
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

  case State of
  dsNormal:
    Canvas.Font.Color := Appearance.Normal.TextColor;
  dsDown:
    Canvas.Font.Color := Appearance.Down.TextColor;
  dsHover:
    Canvas.Font.Color := Appearance.Hover.TextColor;
  dsSelected:
    Canvas.Font.Color := Appearance.Selected.TextColor;
  end;

  if (AItem.ImageIndex >= 0) and Assigned(Images) then
  begin
    Images.Draw(Canvas, R.Left + 2 + (Appearance.Normal.Rounding div 2), R.Top + 2, AItem.ImageIndex);
    R.Left := R.Left + Images.Width + IMAGEMARGIN;
  end;

  Canvas.Brush.Style := bsClear;

  R.Left := R.Left + 2 + (Appearance.Normal.Rounding div 2);

  DrawText(Canvas.Handle, PChar(AItem.DisplayText), Length(AItem.DisplayText), R, DT_VCENTER or DT_SINGLELINE);
end;

function TAdvWordCategoryCloud.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) +
    '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvWordCategoryCloud.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvWordCategoryCloud.KeyDown(var Key: Word; Shift: TShiftSTate);
begin
  inherited;

  if Categories.Count = 0 then
    Exit;

  if Key = VK_PRIOR then
  begin
    if CategoryIndex < Categories.Count - 1 then
      CategoryIndex := CategoryIndex + 1
    else
      CategoryIndex := 0;
  end;

  if Key = VK_NEXT then
  begin
    if CategoryIndex > 0 then
      CategoryIndex := CategoryIndex - 1
    else
      CategoryIndex := Categories.Count - 1;
  end;

  if (Key in [VK_UP, VK_LEFT]) then
  begin
    if FFocusIndex > 0 then
      dec(FFocusIndex)
    else
      FFocusIndex := Categories[CategoryIndex].Words.Count - 1;

    if FFocusIndex >= Categories[CategoryIndex].Words.Count then
      FFocusIndex := Categories[CategoryIndex].Words.Count -1;

    Invalidate;
  end;
  if (Key in [VK_DOWN, VK_RIGHT]) then
  begin
    if FFocusIndex < Categories[CategoryIndex].Words.Count - 1 then
      inc(FFocusIndex)
    else
      FFocusIndex := 0;
    Invalidate;
  end;

  if (Key = VK_SPACE) then
  begin
    FDownIndex := FFocusIndex;
    Invalidate;
  end;

  if FFocusIndex < 0 then
  begin
    FFocusIndex := 0;
    Invalidate;
  end;

end;

procedure TAdvWordCategoryCloud.KeyUp(var Key: Word; Shift: TShiftSTate);
begin
  inherited;

  if Categories.Count = 0 then
    Exit;

  if (FDownIndex >= 0) then
  begin
    if Selection = wsSingle then
    begin
      if Categories[CategoryIndex].WordIndex <> FFocusIndex then
      begin
        Categories[CategoryIndex].WordIndex := FFocusIndex;
        DoSelect(Categories[CategoryIndex].WordIndex);
      end
      else
      begin
        Categories[CategoryIndex].WordIndex := -1;
        DoUnSelect(FFocusIndex);
      end;
    end;
    if Selection = wsMulti then
    begin
      Categories[CategoryIndex].Words[FDownIndex].Selected := not Categories[CategoryIndex].Words[FDownIndex].Selected;
      if Categories[CategoryIndex].Words[FDownIndex].Selected then
        DoSelect(FDownIndex)
      else
        DoUnSelect(FDownIndex);
    end;

    FDownIndex := -1;
    Invalidate;
  end;

end;

procedure TAdvWordCategoryCloud.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  idx: integer;
begin
  inherited;

  if TabStop and (GetFocus <> Handle) then
  begin
    SetFocus;
    Invalidate;
  end;


  if CategorySelection = csClick then
  begin
    idx := CategoryAtXY(X,Y);
    if idx <> -1 then
    begin
      CategoryIndex := idx;
      FDownIndex := -1;
      Invalidate;
    end;
  end;

  idx := ValueAtXY(X,Y);

  if (idx <> FDownIndex) then
  begin
    FMouseDown := true;
    FDownIndex := idx;

    if Selection = wsSingle then
    begin
      if (Categories[CategoryIndex].WordIndex <> idx) then
      begin
        DoUnSelect(Categories[CategoryIndex].WordIndex);
      end;

      Categories[CategoryIndex].WordIndex := idx;
      DoSelect(idx);
    end;
    Invalidate;
  end;

  if (Selection = wsMulti) and (idx <> -1) then
  begin
    FMouseDown := true;
    Categories[CategoryIndex].Words[idx].Selected := not Categories[CategoryIndex].Words[idx].Selected;
    if Categories[CategoryIndex].Words[idx].Selected then
      DoSelect(idx)
    else
      DoUnSelect(idx);
  end;
end;

procedure TAdvWordCategoryCloud.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  idx: integer;
begin
  inherited;

  if CategorySelection = csHover then
  begin
    idx := CategoryAtXY(X,Y);
    if idx <> -1 then
    begin
      CategoryIndex := idx;
      FDownIndex := -1;
      Invalidate;
    end;
  end;

  idx := ValueAtXY(X,Y);

  if (FHintIndex <> - 1) and (idx <> FHintIndex) then
  begin
    Application.CancelHint;
    FHintIndex := -1;
  end;

  if (idx <> FHoverIndex) then
  begin
    FHoverIndex := idx;
    Invalidate;
  end;

end;

procedure TAdvWordCategoryCloud.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  idx: integer;
begin
  inherited;

  idx := ValueAtXY(X,Y);

  if FMouseDown then
  begin
    FMouseDown := false;
    Invalidate;

    if (idx = FDownIndex) then
      DoClick(idx);
  end;

  if (FDownIndex <> -1) then
  begin
    FDownIndex := -1;
    Invalidate;
  end;
end;

procedure TAdvWordCategoryCloud.Paint;
var
  i, tri: integer;
  dx,dy,dw: integer;
  fh: integer;
  DR,CR,R: TRect;
  state: TDrawState;
  gp: TGPGraphics;
  gpbrush: TGPBrush;
  gppath: TGPGraphicsPath;
  pts: array[0..2] of TGPPointF;
begin
  inherited;

  if (CategoryIndex >= Categories.Count) then
    CategoryIndex := Max(0,Categories.Count - 1);

  CR := ClientRect;

  fh := ItemHeight;

  dx := LEFTOFFSET;
  dy := TOPOFFSET + FHeader.Height;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := FHeader.Color;
  Canvas.Pen.Color := FHeader.Color;

  CR.Bottom := CR.Top + FHeader.Height;
  Canvas.Rectangle(CR);

  DR := CR;
  DR.Left := dx;

  tri := dx;

  Canvas.Font.Assign(FHeader.Font);

  for i := 0 to Categories.Count - 1 do
  begin
    DrawText(Canvas.Handle, PChar(Categories[i].Category), Length(Categories[i].Category), DR, DT_VCENTER or DT_SINGLELINE);
    DrawText(Canvas.Handle, PChar(Categories[i].Category), Length(Categories[i].Category), DR, DT_VCENTER or DT_SINGLELINE or DT_CALCRECT);

    if i = CategoryIndex then
      tri := DR.Left + ITEMMARGIN div 2;

    DR.Left := DR.Right + ITEMMARGIN;
    DR.Right := Width;
    DR.Top := CR.Top;
    DR.Bottom := CR.Bottom;
  end;

  Canvas.Font.Assign(Font);

  CR := ClientRect;

  if BorderStyle = bsSingle then
    inc(dy);

  CR.Top := CR.Top + FHeader.Height;

  Canvas.Brush.Style := bsSolid;

  if (Categories.Count > 0) and (CategoryIndex >= 0) and (CategoryIndex < Categories.Count) then
    Canvas.Brush.Color := Categories[CategoryIndex].Color
  else
    Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Canvas.Brush.Color;

  Canvas.Rectangle(CR);

  if (Categories.Count > 0) and (CategoryIndex >= 0) and (CategoryIndex < Categories.Count) then
  begin
    for i := 0 to Categories[CategoryIndex].Words.Count - 1 do
    begin
      dw := Categories[CategoryIndex].Words[i].GetWidth(Canvas) + ITEMMARGIN;

      if dx + dw - ITEMMARGIN + 6 >= Width then
      begin
        dx := LEFTOFFSET;
        dy := dy + fh + LINEMARGIN;
      end;

      state := dsNormal;

      if i = FHoverIndex then
        state := dsHover;

      if i = FDownIndex then
        state := dsDown;

      if not ( (state = dsDown) and FMouseDown) then
      begin
        case Selection of
        wsSingle:
          if i = Categories[CategoryIndex].WordIndex then
            state := dsSelected;
        wsMulti:
          if Categories[CategoryIndex].Words[i].Selected then
            state := dsSelected;
        end;
      end;

      DrawItem(Canvas, Rect(dx, dy, dx + dw - ITEMMARGIN + 4, dy + fh + 2), Categories[CategoryIndex].Words[i], state);

      dx := dx + dw;
    end;

    gp := TGPGraphics.Create(Canvas.Handle);
    gpbrush := TGPSolidBrush.Create(MakeColor(255,Categories[CategoryIndex].Color));
    gppath :=  TGPGraphicsPath.Create;

    try
      gp.SetSmoothingMode(SmoothingModeAntiAlias);

      pts[0].X := tri;
      pts[0].Y := FHeader.Height + 1;
      pts[1].X := tri + 3;
      pts[1].Y := FHeader.Height - 6 + 1;
      pts[2].X := tri + 6;
      pts[2].Y := FHeader.Height + 1;

      gppath.AddPolygon(PGPPointF(@pts),3);
      gp.FillPath(gpbrush,gppath);

    finally
      gpbrush.Free;
      gppath.Free;
      gp.Free;
    end;

    if (FFocusIndex >= 0) and (GetFocus = Handle) and (Categories[CategoryIndex].Words.Count > 0) then
    begin
      R := ValueRect(FFocusIndex);
      R.Top := R.Top + 4;
      R.Left := R.Left + 4;
      R.Bottom := R.Top + Canvas.TextHeight('gh');
      R.Right := R.Right - ITEMMARGIN;
      DrawFocusRect(Canvas,R,clBlack);
    end;
  end;

  if BorderStyle = bsSingle then
  begin
    R := ClientRect;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := FBorderColor;
    Canvas.Pen.Style := psSolid;
    Canvas.Rectangle(R);
  end;
end;

procedure TAdvWordCategoryCloud.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TAdvWordCategoryCloud.SetBorderStyle(const Value: TBorderStyle);
begin
  if (FBorderStyle <> Value) then
  begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

procedure TAdvWordCategoryCloud.SetCategories(const Value: TWordCategories);
begin
  FCategories.Assign(Value);
end;

procedure TAdvWordCategoryCloud.SetCategoryIndex(const Value: integer);
begin
  if (Value < Categories.Count) and (Value >= 0) then
  begin
    if (Value <> FCategoryIndex) then
    begin
      FCategoryIndex := Value;
      Invalidate;
    end;
  end;
end;

procedure TAdvWordCategoryCloud.SetHeader(const Value: TCategoryHeader);
begin
  FHeader.Assign(Value);
end;

procedure TAdvWordCategoryCloud.SetImages(const Value: TCustomImageList);
begin
  if (FImages <> Value) then
  begin
    FImages := Value;
    Invalidate;
  end;
end;

procedure TAdvWordCategoryCloud.SetVersion(const Value: string);
begin

end;

function TAdvWordCategoryCloud.ValueAtXY(X, Y: integer): integer;
var
  i,dx,dy,dw,th: integer;
begin
  Result := -1;

  if Categories.Count = 0 then
    Exit;

  Canvas.Font.Assign(Font);

  th := ItemHeight;

  dx := LEFTOFFSET;
  dy := TOPOFFSET + FHeader.Height;

  for i := 0 to Categories[CategoryIndex].Words.Count - 1 do
  begin
    dw := Categories[CategoryIndex].Words[i].GetWidth(Canvas) + ITEMMARGIN;

    if dx + dw - ITEMMARGIN + 6 >= Width then
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

function TAdvWordCategoryCloud.ValuePos(Index: integer): TPoint;
var
  j, dx, dy, tw, th, dt: integer;
begin
  Canvas.Font.Assign(Font);

  th := ItemHeight;

  dt := th - Canvas.TextHeight('gh');

  dx := LEFTOFFSET;
  dy := TOPOFFSET + FHeader.Height + (dt div 2);

  if BorderStyle = bsSingle then
    inc(dy);

  for j := 0 to Index - 1 do
  begin

    if (j < Categories[CategoryIndex].Words.Count) then
    begin
      tw := Categories[CategoryIndex].Words[j].GetWidth(Canvas) + ITEMMARGIN;

      if dx + tw - ITEMMARGIN + 6 >= Width then
      begin
        dx := LEFTOFFSET + tw;
        dy := dy + th + LINEMARGIN;
      end
      else
        dx := dx + tw;
    end;
  end;

  Result := Point(dx,dy);
end;

function TAdvWordCategoryCloud.ValueRect(Index: integer): TRect;
var
  i,dx,dy,dw,th: integer;
begin
  Result := Rect(0,0,0,0);

  Canvas.Font.Assign(Font);

  th := ItemHeight;

  dx := LEFTOFFSET;
  dy := TOPOFFSET + FHeader.Height;

  for i := 0 to Categories[CategoryIndex].Words.Count - 1 do
  begin
    dw := Categories[CategoryIndex].Words[i].GetWidth(Canvas) + ITEMMARGIN;

    if dx + dw - ITEMMARGIN + 6 >= Width then
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

procedure TAdvWordCategoryCloud.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS;
end;

{ TWordCategories }

function TWordCategories.Add: TWordCategory;
begin
  Result := TWordCategory(inherited Add);
end;

constructor TWordCategories.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TWordCategory);
end;

function TWordCategories.GetItem(Index: integer): TWordCategory;
begin
  Result := TWordCategory(inherited Items[Index]);
end;

function TWordCategories.Insert(Index: integer): TWordCategory;
begin
  Result := TWordCategory(inherited Insert(Index));
end;

procedure TWordCategories.SetItem(Index: integer; const Value: TWordCategory);
begin
  inherited Items[Index] := Value;
end;

procedure TWordCategories.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TCategoryHeader }

procedure TCategoryHeader.Assign(Source: TPersistent);
begin
  if (Source is TCategoryHeader) then
  begin
    FColor := (Source as TCategoryHeader).Color;
    FFont.Assign((Source as TCategoryHeader).Font);
    FHeight := (Source as TCategoryHeader).Height;
  end;
end;

procedure TCategoryHeader.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TCategoryHeader.Create;
begin
  inherited;
  FFont := TFont.Create;
  FHeight := 22;
  FColor := clHighlight;
  FFont.Color := clHighlightText;
  FFont.OnChange := FontChanged;
end;

destructor TCategoryHeader.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TCategoryHeader.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TCategoryHeader.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TCategoryHeader.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TCategoryHeader.SetHeight(const Value: integer);
begin
  if (FHeight <> Value) then
  begin
    FHeight := Value;
    Changed;
  end;
end;

end.
