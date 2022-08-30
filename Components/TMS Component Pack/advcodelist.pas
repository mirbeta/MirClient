{***************************************************************************}
{ TAdvMemo component                                                        }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2014                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of TMS software.                                    }
{***************************************************************************}

{$I TMSDEFS.INC}

unit AdvCodeList;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, Graphics, AdvMemo,
  Forms, AdvCodeHint, ImgList, Clipbrd, Dialogs, AdvCodeListDD, ActiveX
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF};

const
  MAJ_VER = 3; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 0; // Build nr.
  DATE_VER = 'Mar, 2014'; // Month version

  // version history
  // 1.6.1.1 : Fixed issues with VCL.NET
  // 1.7.0.0 : ScrollBar issue fixed
  // 1.7.0.1 : Assignment in TCodeBlock added for use in frames
  // 2.0.0.1 : Fixed issue with hints for TAdvCodeList on secondary forms
  // 2.0.0.2 : Fixed issue with ClipboardViewer setting when window handle is recreated
  // 2.0.1.0 : New property ClipboardAppend added
  //         : Fixed issue with method CodeBlocks.Insert();
  // 2.1.0.0 : New : OLE drag & drop support added
  // 3.0.0.0 : Build with TAdvMemo v3.0
  //         : New : UseStyler property added
  // 3.0.0.1 : Fixed : Issue with drawing text with tabs when no syntaxstyler is assigned
  // 3.0.0.2 : Fixed : Issue with custom hint class
  // 3.0.0.3 : Fixed : OnBlockClick triggered when new item is selected with keyboard
  // 3.0.1.0 : New : OnAfterBlockDelete event added
  //         : New : OnBeforeBlockInsertFromClipboard event added
  //         : New : ItemStyle property added
  // 3.0.2.0 : New : SetItemSize() method added
  //         : New : CodeBlockCaptionSelectColor/CodeBlockCaptionSelectTextColor added
  //         : Fixed : Issue with updating item size when deleting blocks

type
  TBorderWidth = 0..20;

  TAdvCodeList = class;

  TCodeBlock = class(TCollectionItem)
  private
    FImageIndex: Integer;
    FCode: TStringList;
    FTag: Integer;
    FCaption: string;
    procedure SetCode(const Value: TStringList);
    procedure SetImageIndex(const Value: Integer);
    procedure CodeChanged(Sender: TObject);
    procedure SetCaption(const Value: string);
  protected
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: string read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Code: TStringList read FCode write SetCode;
    property Tag: Integer read FTag write FTag default 0;
  end;

  TCodeBlocks = class(TCollection)
  private
    FOwner: TAdvCodeList;
    function GetItem(Index: Integer): TCodeBlock;
    procedure SetItem(Index: Integer; const Value: TCodeBlock);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvCodeList);
    function Add(Text: string): TCodeBlock;
    function Insert(Index: Integer; Text: string): TCodeBlock;
    procedure Clear;
    property Items[Index: Integer]: TCodeBlock read GetItem write SetItem; default;
    property CodeList: TAdvCodeList read FOwner;
  end;

  TItemStyle = (isFixedHeight, isVariableHeight);

  TBlockEvent = procedure(Sender: TObject; ACodeBlock: TCodeBlock) of object;
  TBlockAllowEvent = procedure(Sender: TObject; ACodeBlock: TCodeBlock; var Allow:Boolean) of object;
  TBlockIndexEvent = procedure(Sender: TObject; Index: integer) of object;
  TBlockAllowInsertEvent = procedure(Sender: TObject; AText: string; var Allow: boolean) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvCodeList = class(TCustomListBox)
  private
    FHintBlock: integer;
    FShowSelection: Boolean;
    FSelectionColor: TColor;
    FCaseSensitive: Boolean;
    FSearching: Boolean;
    FTempdelimiters: string;
    InternalStyles: TAdvCustomMemoStyler;
    FtmpNoStart,FtmpNo,FtmpNoHex:String;
    FStyle: TStyle;
    FBlockCount: integer;
    FCodeBorderColor: TColor;
    FCodeBorderSelectColor: TColor;
    FCodeBlockColor: TColor;
    FCodeBlockColorTo: TColor;
    FCodeBlockSelectColor: TColor;
    FCodeBlockSelectColorTo: TColor;
    FCodeBlockCaptionColor: TColor;
    FCodeBlockCaptionSelectColor: TColor;
    FCodeBlockCaptionTextColor: TColor;
    FCodeBlockCaptionSelectTextColor: TColor;
    FCodeBorderWidth: TBorderWidth;
    FCodeIndent: TBorderWidth;
    FImages: TImageList;
    FCodeBlocks: TCodeBlocks;
    FClipChainHandle: THandle;
    FClipboardView: Boolean;
    FClipboardAppend: Boolean;
    FOnBlockDelete: TBlockAllowEvent;
    FOnBlockClick: TBlockEvent;
    FOnBlockRightClick: TBlockEvent;
    FOnBlockInsertFromClipboard: TBlockEvent;
    FOnBlockDblClick: TBlockEvent;
    FOleDropTarget: Boolean;
    FOleDropSource: Boolean;
    FOleDropTargetAssigned: Boolean;
    FPtDown: TPoint;
    FMouseDown: boolean;
    FIsDragging: boolean;
    FIsDropping: boolean;
    FOnBlockDropped: TBlockEvent;
    FUseStyler: boolean;
    FOldIndex: integer;
    FHintWndClass: THintWindowClass;
    FOnAfterBlockDelete: TBlockIndexEvent;
    FOnBeforeBlockInsertFromClipboard: TBlockAllowInsertEvent;
    FItemStyle: TItemStyle;
    procedure SetInternalStyles(const Value: TAdvCustomMemoStyler);
    procedure DrawCustomLine(ACanvas: TCanvas; LineNo, SubLineNo: Integer; var style: TStyle;DM: TDrawMode; PR: TRect);
    procedure DrawBorders;
    procedure ExtractURL(s: string; var urls: TStringList);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    function GetBlock(index: integer): String;
    procedure SetBlock(Index: Integer; Value: String);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMDrawClipboard(var Msg: TMessage); message WM_DRAWCLIPBOARD;
    procedure WMChangeCBChain(var Msg: TMessage); message WM_CHANGECBCHAIN;
    procedure WMDestroy(var Msg: TWMDestroy); message WM_DESTROY;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;

    procedure SetCodeBlockColor(const Value: TColor);
    procedure SetCodeBlockColorTo(const Value: TColor);
    procedure SetCodeBlockSelectColor(const Value: TColor);
    procedure SetCodeBlockSelectColorTo(const Value: TColor);
    procedure SetCodeBorderColor(const Value: TColor);
    procedure SetCodeBorderSelectColor(const Value: TColor);
    procedure SetVersion(const Value: string);
    function GetVersion: string;
    function GetVersionNr: Integer;
    procedure SetCodeBlockCaptionColor(const Value: TColor);
    procedure SetCodeBlockCaptionTextColor(const Value: TColor);
    procedure SetOleDropSource(const Value: Boolean);
    procedure SetOleDropTarget(const Value: Boolean);
    procedure SetUseStyler(const Value: boolean);
    procedure SetItemStyle(const Value: TItemStyle);
    procedure SetCodeBlockCaptionSelectColor(const Value: TColor);
    procedure SetCodeBlockCaptionSelectTextColor(const Value: TColor);
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure KeyDown(var KeyCode: Word; Shift: TShiftState); override;
    procedure KeyUp(var KeyCode: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure DrawItemEx(ACanvas: TCanvas; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure AddBlock(Source: String);
    property BlockCount: integer read FBlockCount;
    property Blocks[Index: Integer]: String read GetBlock write SetBlock;
    procedure Loaded; override;
    procedure Resize; override;
    procedure CreateWnd; override;
    procedure ReMeasure;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetItemSize(Index, Size: integer);
    procedure DeleteBlock;
    procedure SetCodeBlocks(const Value: TCodeBlocks);
  published
    property SyntaxStyles: TAdvCustomMemoStyler read InternalStyles write SetInternalStyles;
    property Images: TImageList read FImages write FImages;
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property ClipboardView: Boolean read FClipboardView write FClipboardView default false;
    property ClipboardAppend: Boolean read FClipboardAppend write FClipboardAppend default true; 
    property CodeBlocks: TCodeBlocks read FCodeBlocks write SetCodeBlocks;
    property Color;
    property CodeBlockColor: TColor read FCodeBlockColor write SetCodeBlockColor;
    property CodeBlockColorTo: TColor read FCodeBlockColorTo write SetCodeBlockColorTo;
    property CodeBlockSelectColor: TColor read FCodeBlockSelectColor write SetCodeBlockSelectColor;
    property CodeBlockSelectColorTo: TColor read FCodeBlockSelectColorTo write SetCodeBlockSelectColorTo;
    property CodeBlockCaptionColor: TColor read FCodeBlockCaptionColor write SetCodeBlockCaptionColor default clHighLight;
    property CodeBlockCaptionSelectColor: TColor read FCodeBlockCaptionSelectColor write SetCodeBlockCaptionSelectColor default clNone;
    property CodeBlockCaptionTextColor: TColor read FCodeBlockCaptionTextColor write SetCodeBlockCaptionTextColor default clHighLightText;
    property CodeBlockCaptionSelectTextColor: TColor read FCodeBlockCaptionSelectTextColor write SetCodeBlockCaptionSelectTextColor default clNone;
    property CodeBorderColor: TColor read FCodeBorderColor write SetCodeBorderColor;
    property CodeBorderSelectColor: TColor read FCodeBorderSelectColor write SetCodeBorderSelectColor;    
    property CodeBorderWidth: TBorderWidth read FCodeBorderWidth write FCodeBorderWidth;
    property CodeIndent: TBorderWidth read FCodeIndent write FCodeIndent;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property ItemStyle: TItemStyle read FItemStyle write SetItemStyle default isFixedHeight;
    property MultiSelect;
    property OleDropTarget: Boolean read FOleDropTarget write SetOleDropTarget default False;
    property OleDropSource: Boolean read FOleDropSource write SetOleDropSource default False;
    property ParentBiDiMode;
    property ParentColor;
    //property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ShowSelection: Boolean read FShowSelection write FShowSelection default true;
    property SelectionColor: TColor read FSelectionColor write FSelectionColor default clWhite;
    property Sorted;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property UseStyler: boolean read FUseStyler write SetUseStyler;
    property Version: string read GetVersion write SetVersion;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnAfterBlockDelete: TBlockIndexEvent read FOnAfterBlockDelete write FOnAfterBlockDelete;
    property OnBlockDelete: TBlockAllowEvent read FOnBlockDelete write FOnBlockDelete;
    property OnBlockClick: TBlockEvent read FOnBlockClick write FOnBlockClick;
    property OnBlockDblClick: TBlockEvent read FOnBlockDblClick write FOnBlockDblClick;
    property OnBlockRightClick: TBlockEvent read FOnBlockRightClick write FOnBlockRightClick;
    property OnBeforeBlockInsertFromClipboard: TBlockAllowInsertEvent read FOnBeforeBlockInsertFromClipboard write FOnBeforeBlockInsertFromClipboard;
    property OnBlockInsertFromClipboard: TBlockEvent read FOnBlockInsertFromClipboard write FOnBlockInsertFromClipboard;
    property OnBlockDropped: TBlockEvent read FOnBlockDropped write FOnBlockDropped;
  end;

  TCodeListDropTarget = class(TAdvCodeListDropTarget)
  private
    FCodeList: TAdvCodeList;
  public
    constructor Create(ACodeList: TAdvCodeList);
    procedure DropText(pt: tpoint; s: string); override;
    procedure DragMouseMove(pt: tpoint; var allow: Boolean); override;
  end;


implementation

procedure DrawGradient(Canvas: TCanvas; FromColor,ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr,startr,endr: Integer;
  diffg,startg,endg: Integer;
  diffb,startb,endb: Integer;
  rstepr,rstepg,rstepb,rstepw: Real;
  i,stepw: Word;

begin
  if Steps = 0 then
    Steps := 1;

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with Canvas do
  begin
    for i := 0 to steps-1 do
    begin
      endr := startr + Round(rstepr*i);
      endg := startg + Round(rstepg*i);
      endb := startb + Round(rstepb*i);
      stepw := Round(i*rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
        Rectangle(R.Left + stepw,R.Top,R.Left + stepw + Round(rstepw)+1,R.Bottom)
      else
        Rectangle(R.Left,R.Top + stepw,R.Right,R.Top + stepw + Round(rstepw)+1);
    end;
  end;
end;


function VarPos(sub,s:string; var vp: Integer): Integer;
begin
  vp := pos(sub,s);
  Result := vp;
end;

constructor TCodeBlock.Create(Collection: TCollection);
var
  l: TAdvCodeList;
begin
  inherited;
  FCode := TStringList.Create;
  FCode.OnChange := CodeChanged;

  l := TCodeBlocks(Collection).FOwner;
  l.Items.Add('');
  FImageIndex := -1;
end;

destructor TCodeBlock.Destroy;
var
  l: TAdvCodeList;
begin
  FCode.Free;

  l := TCodeBlocks(Collection).FOwner;

  if not (csDestroying in l.ComponentState) then
     l.Items.Delete(0);

  inherited;
end;

procedure TCodeBlock.Assign(Source: TPersistent);
begin
  if Source is TCodeBlock then
  begin
    FImageIndex := TCodeBlock(Source).FImageIndex;
    FCode.Assign(TCodeBlock(Source).FCode);
    FTag := TCodeBlock(Source).FTag;
    FCaption := TCodeBlock(Source).FCaption;
  end;
end;

procedure TCodeBlock.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    TCodeBlocks(Collection).CodeList.Invalidate;
  end;
end;

procedure TCodeBlock.SetCode(const Value: TStringList);
begin
  FCode.Assign(Value);
end;

procedure TCodeBlock.SetImageIndex(const Value: Integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    TCodeBlocks(Collection).CodeList.Invalidate;
  end;
end;

procedure TCodeBlock.CodeChanged(Sender: TObject);
begin
  TCodeBlocks(Collection).CodeList.Invalidate;
end;

//------------------------------
constructor TCodeBlocks.Create(AOwner: TAdvCodeList);
begin
  inherited Create(TCodeBlock);
  FOwner := AOwner;
end;

function TCodeBlocks.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


function TCodeBlocks.GetItem(Index: Integer): TCodeBlock;
begin
  Result := TCodeBlock(inherited Items[Index]);
end;

procedure TCodeBlocks.SetItem(Index: Integer; const Value: TCodeBlock);
begin
  inherited Items[Index] := Value;
end;

function TCodeBlocks.Add(Text: String): TCodeBlock;
var
  s: string;
  vp: Integer;
begin
  Result := TCodeBlock(inherited Add);

  s := Text;
  while varpos(#13#10, s, vp) > 0 do
  begin
    Result.Code.Add(copy(s,1,vp-1));
    System.Delete(s,1,vp+1);
  end;

  if length(s) > 0 then
    Result.Code.Add(s);

//  FOwner.Items.Add('');
end;


procedure TCodeBlocks.Clear;
begin
  inherited Clear;
  FOwner.Items.Clear;
end;

function TCodeBlocks.Insert(Index: Integer; Text: string): TCodeBlock;
var
  s: string;
  vp: Integer;
begin
  Result := TCodeBlock(inherited Insert(Index));

  s := Text;
  while varpos(#13#10, s, vp) > 0 do
  begin
    Result.Code.Add(copy(s,1,vp-1));
    System.Delete(s,1,vp+1);
  end;

  if length(s) > 0 then
    Result.Code.Add(s);

 // FOwner.Items.Add('');
end;


//---------------------------- TAdvCodeList ------------------------------------

constructor TAdvCodeList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited Style := lbOwnerDrawFixed;

  FCaseSensitive := false;
  FSearching := false;
  FUseStyler := true;

  FStyle.index := -1;
  FStyle.isComment := 0;
  FStyle.isBracket := False;
  FStyle.isnumber := False;
  FStyle.iskeyWord := false;
  FStyle.isdelimiter := False;
  FStyle.isURL := False;
  FStyle.index := -1;
  FStyle.EndBracket := #0;

  FBlockCount := 0;
  FShowSelection := true;
  FSelectionColor := clWhite;

  FCodeBorderSelectColor := clBlue;
  FCodeBorderColor := clSilver;
  FCodeBlockColor := clWhite;
  FCodeBlockColorTo := clInfoBk;
  FCodeBlockSelectColor := clWhite;
  FCodeBlockSelectColorTo := $00CFF0FE;
  FCodeBlockCaptionColor := clHighLight;
  FCodeBlockCaptionTextColor := clHighlightText;

  FCodeBorderWidth := 2;
  FCodeIndent := 2;

  ItemHeight := 48;

  FCodeBlocks := TCodeBlocks.Create(self);
  FClipChainHandle := 0;
  FClipboardView := false;
  FClipboardAppend := true;
  Ctl3D := False;
  ParentCtl3D := False;
  FMouseDown := False;
  FIsDragging := False;
  FCodeBlockCaptionSelectColor := clNone;
  FCodeBlockCaptionSelectTextColor := clNone;

end;

procedure TAdvCodeList.CreateWnd;
begin
  inherited;
  FClipChainHandle := SetClipboardViewer(Self.Handle);
end;

destructor TAdvCodeList.Destroy;
begin
  FCodeBlocks.Free;
  inherited;
end;

procedure TAdvCodeList.AddBlock(Source: String);
begin
  CodeBlocks.Add(Source);
end;

procedure TAdvCodeList.CMMouseEnter(var Msg: TMessage);
begin
  inherited;

  if ShowHint then
  begin
    // install custom hint class
    FHintWndClass := HintWindowClass;
    HintWindowClass := TCustomHintWindow;
  end;
end;

procedure TAdvCodeList.CMMouseLeave(var Msg: TMessage);
begin
  inherited;

  // restore default hint class
  if ShowHint then
    HintWindowClass := FHintWndClass;
end;

procedure TAdvCodeList.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
  rct1, rct2: TRect;
  th: integer;
  t: integer;
  bmp: TBitmap;
begin
  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);

    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    State := State - [odFocused];

    if not FShowSelection then
      State := State - [odSelected];

    bmp := TBitmap.Create;

    bmp.Width := rcItem.Right - rcItem.Left;
    bmp.Height := rcItem.Bottom - rcItem.Top;
    bmp.Canvas.Font.Assign(Font);

    t := rcItem.Top;
    rcItem.Top := 0;
    rcItem.Bottom := bmp.Height;

    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      bmp.Canvas.Brush.Color := FSelectionColor;

      if (FCodeBorderWidth > 0) then
      begin
        rct2 := rcItem;

        rct2.Bottom := rct2.Bottom - FCodeBorderWidth;
        rct2.Top := rct2.Top + 1;
        rct2.Left := rct2.Left + 4;
        rct2.Right := rct2.Right - 1;

        DrawGradient(bmp.Canvas,ColorToRGB(FCodeBlockSelectColor),ColorToRGB(FCodeBlockSelectColorTo),64, rct2, false);

        bmp.Canvas.Pen.Color:= FCodeBorderSelectColor;
        bmp.Canvas.Pen.Width:= FCodeBorderWidth;
        bmp.Canvas.Brush.Style := bsClear;
        bmp.Canvas.RoundRect(rcItem.left+2,rcItem.Top,rcItem.Right-1,rcItem.Bottom-1,8,8);
      end;
    end
    else
    begin
      bmp.Canvas.Brush.Color := Color;
      rct2 := rcItem;

      if (FCodeborderwidth > 0) and (CodeBlocks.Count > 0) then
      begin
        rct2:= rcItem;
        rct2.Top := rct2.Top + 1;
        rct2.Bottom := rct2.Bottom - FCodeBorderWidth;
        rct2.Left := rct2.Left + 4;
        rct2.Right := rct2.Right - 1;

        DrawGradient(bmp.Canvas,ColorToRGB(FCodeBlockColor),ColorToRGB(FCodeBlockColorTo),64, rct2, false);

        bmp.Canvas.Pen.Color:= FCodeBorderColor;
        bmp.Canvas.Pen.Width:= FCodeBorderWidth;
        bmp.Canvas.Brush.Style := bsClear;

        bmp.Canvas.RoundRect(rcItem.left+2,rcItem.Top,rcItem.Right-1,rcItem.Bottom-1,8,8);
       end;
    end;

    rct1 := rcItem;
    rct1.Top:= rct1.Top + 1;
    rct1.Left:= rct1.Left + FCodeIndent + 2;
    rct1.Right := rct1.Right - 2;

    if Integer(itemID) >= 0 then
    begin
      if (FCodeBlocks[itemID].Caption <> '') then
      begin
        bmp.Canvas.Pen.Color := FCodeBorderColor;
        bmp.Canvas.Pen.Width := 0;
        bmp.Canvas.Pen.Style := psClear;
        bmp.Canvas.Brush.Style := bsSolid;

        if (odSelected in State) and (FCodeBlockCaptionSelectColor <> clNone) then
          bmp.Canvas.Brush.Color := FCodeBlockCaptionSelectColor
        else
          bmp.Canvas.Brush.Color := FCodeBlockCaptionColor;

        bmp.Canvas.Font.Assign(Font);
        th := bmp.Canvas.TextHeight('gh');
        bmp.Canvas.RoundRect(rcItem.left+2,rcItem.Top + 1,rcItem.Right-1,rcItem.Top + th + 4,8,8);
        bmp.Canvas.Rectangle(rcItem.Left + 2, rcItem.Top + 8,rcItem.Right-1,rcItem.Top + th + 4 );

        if (odSelected in State) and (FCodeBlockCaptionSelectTextColor <> clNone) then
          bmp.Canvas.Font.Color := FCodeBlockCaptionSelectTextColor
        else
          bmp.Canvas.Font.Color := FCodeBlockCaptionTextColor;

        DrawText(bmp.Canvas.Handle, PChar(FCodeBlocks[itemID].Caption), Length(FCodeBlocks[itemID].Caption), rct1, DT_SINGLELINE or DT_LEFT);
        rct1.Top := rct1.Top + th + 4;
        bmp.Canvas.Brush.Style := bsClear;
        bmp.Canvas.Pen.Style := psSolid;
      end;

      DrawItemEx(bmp.Canvas, itemID, rct1, State)
    end
    else
      bmp.Canvas.FillRect(rct1);

    Canvas.Draw(rcItem.Left,t, bmp);

    bmp.Free;

    if (odFocused in State) then
      DrawFocusRect(hDC, rct1);

    Canvas.Handle := 0;
  end;
end;

procedure TAdvCodeList.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin

end;

procedure TAdvCodeList.DrawItemEx(ACanvas: TCanvas; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  allb: integer;
  TotalLinesCount, MaxLines, VisibleLines, SubIndex: integer;
  Offset: Integer;
  SL: TStringList;
  s:string;
  DrwR: TRect;

begin
  TotalLinesCount := FCodeBlocks.Items[Index].Code.Count;

  if ItemStyle = isFixedHeight then
    MaxLines := ItemHeight div 14
  else
    MaxLines := 100;

  if TotalLinesCount > MaxLines then
    VisibleLines := MaxLines - 2
  else
    VisibleLines := TotalLinesCount - 1;

  FStyle.isComment := 0;
  FStyle.isBracket := false;
  FStyle.iskeyWord := false;
  FStyle.isnumber := false;
  FStyle.isdelimiter := false;
  FStyle.isURL := false;

  if Assigned(InternalStyles) and UseStyler then
  begin
    for allb := 0 to InternalStyles.AllStyles.Count - 1 do
    begin
      if InternalStyles.AllStyles.Items[allb].StyleType <> stSymbol then
        Continue;
      FTempdelimiters := FTempdelimiters + InternalStyles.AllStyles.Items[allb].Symbols;
    end;

    FtmpNoStart := UpperCase(InternalStyles.NumericChars + InternalStyles.HexIdentifier);
    FtmpNo := UpperCase(InternalStyles.NumericChars) + 'E';
    FtmpNoHex := Uppercase(InternalStyles.HexIdentifier);

    //------ Images
    if Assigned(Images) then
    begin
      FImages.DrawingStyle := dsTransparent;
      Images.Draw(ACanvas,ARect.Left + CodeIndent,ARect.Top +1 ,CodeBlocks[Index].ImageIndex);
      ARect.Left := ARect.Left + Images.Width + 4;
    end;
    //------

    for SubIndex := 0 to VisibleLines do
    begin
      DrawCustomLine(ACanvas, Index, Subindex, FStyle, dmScreen, ARect);
      ARect.Top := ARect.Top + 14;
    end;
    if TotalLinesCount > MaxLines then
    begin
      ACanvas.Font.Color := clBlack;
      ACanvas.Font.Style := [fsBold];
      ACanvas.Brush.Style := bsClear;
      ACanvas.TextOut(ARect.Left, ARect.Top, '...');
    end;
  end
  else
  begin
    with ACanvas do
    begin
      Offset := 2;          { provide default offset }
      //SL := TStringList(Items.Objects[Index]);
      SL := FCodeBlocks.Items[Index].Code;
      for SubIndex := 0 to VisibleLines do
      begin
        if SL.Count > SubIndex then
        begin
          s := SL[SubIndex];
          DrwR := Rect(ARect.Left + Offset, ARect.Top, ARect.Right, ARect.Top + ARect.Top + 14);
          DrawText(ACanvas.Handle,PChar(S),Length(s),DrwR, DT_EXPANDTABS or DT_LEFT or DT_END_ELLIPSIS);
        end;
        ARect.Top := ARect.Top + 14;
      end;
      if TotalLinesCount > MaxLines then
      begin
        ACanvas.Font.Color := clBlack;
        ACanvas.Font.Style := [fsBold];
        ACanvas.Brush.Style := bsClear;
        ACanvas.TextOut(ARect.Left, ARect.Top, '...');
      end;
    end;
  end;

  {
  ACanvas.Pen.Color := clRed;
  ACanvas.Pen.Width := 2;
  ACanvas.Pen.Style := psSolid;
  ACanvas.MoveTo(rect.Left, rect.Top);
  ACanvas.LineTo(rect.Right , rect.Bottom);
  }
end;

//--------------------------------------------------------------
//        DRAW LINE
//--------------------------------------------------------------

procedure TAdvCodeList.DrawCustomLine(ACanvas: TCanvas; LineNo, SubLineNo: Integer; var style: TStyle;DM: TDrawMode; PR: TRect);
var
  rct0, rct1, rct, lineRct: TRect;
  LineSelStart, LineSelEnd, posln, i: integer;
  urls: TStringList;
  S, S1, S2, S3: string;
  xSelStartX, xSelStartY, xSelEndX, xSelEndY: integer;
  isinlinecomment:boolean;
  backupstyle:Tstyle;
  backupstring:string;
  LineCanvas: TCanvas;
  lit: char;
  BackColor: TColor;

  function Equal(s1, s2: string): boolean;
  begin
    if FCaseSensitive then
      Result := s1 = s2
    else
      Result := AnsiLowerCase(s1) = AnsiLowerCase(s2);
  end;

  //--------- FIND LINE SELECTION -------------
  procedure FindLineSelection(Selpart: string);
  var
    len: integer;
  begin
    s1 := '';
    s2 := '';
    s3 := '';

    if not Focused and not FSearching then
    begin
      s1 := Selpart;
      Exit;
    end;

    if (lineno < xSelStartY) or (lineno > xSelEndY) then
    begin // outside selection lines (vertically)
      s1 := Selpart;
      Exit;
    end;

    if (xSelStartY < LineNo) and (LineNo < xSelEndY) then
    begin // inside multiple selection
      s2 := Selpart;
      Exit;
    end;

    len := length(Selpart);
    LineSelStart := 0;
    LineSelEnd := 0;

    if (xSelStartY = xSelEndY) then // single line selection
    begin
      if xSelStartX = xSelEndX then
      begin // nothing is selected
        s1 := Selpart;
        exit;
      end;
      if xSelStartX >= posln + len then // selection didn't start
      begin
        s1 := Selpart;
        Exit;
      end;
      if xSelEndX <= posln then // selection ended
      begin
        s3 := Selpart;
        Exit;
      end;
      LineSelStart := xSelStartX - posln;
      LineSelEnd := xSelEndX - posln;
    end
    else
    begin// selection on 2 or more lines
      if (xSelStartY = LineNo) then
      begin
        LineSelStart := xSelStartX - posln;
        LineSelEnd := len;
      end;
      if (xSelEndY = LineNo) then
      begin
        LineSelEnd := xSelEndX - posln;;
      end;
    end;

    if LineSelEnd > len then LineSelEnd := len;
    if LineSelEnd < 0 then LineSelEnd := 0;
    if LineSelStart < 0 then LineSelStart := 0;
    if LineSelStart > len then LineSelStart := len;

    S1 := Copy(Selpart, 1, LineSelStart);
    S2 := Copy(Selpart, LineSelStart + 1, LineSelEnd - LineSelStart);
    S3 := Copy(Selpart, LineSelEnd + 1, len - LineSelEnd);
  end;

  //------------- DRAW PART ---------------------
  procedure DrawPart(Part: string; var Drawstyle: TStyle);
  var
    len, selcol, brushcol: integer;

    procedure LoadFromItemStyle;
    begin
      with LineCanvas do
      begin
        try
          Font.Color := InternalStyles.AllStyles.Items[DrawStyle.index].Font.Color;
          Font.Style := InternalStyles.AllStyles.Items[DrawStyle.index].Font.Style;
          Brush.Color := InternalStyles.AllStyles.Items[DrawStyle.index].BGColor;
        except
          on Exception do
          begin
            Font.Color := Self.Font.Color;
            Font.Style := Self.Font.Style;
            Brush.Color := BackColor; //Self.BkColor;
          end;
        end;
      end;
    end;

  begin
    len := Length(Part);

    if len > 0 then
    begin
      with LineCanvas do
      begin
        Font.Color := Self.Font.Color;
        Font.Style := Self.Font.Style;
        Brush.Color := BackColor;// Self.BkColor;
        begin
          if (DrawStyle.isComment > 0) and (not DrawStyle.isURL) and UseStyler then
          begin
            Font.Color := InternalStyles.CommentStyle.TextColor;
            Font.Style := InternalStyles.CommentStyle.Style;
            Brush.Color := InternalStyles.CommentStyle.BkColor;
          end
          else
          begin
            if (DrawStyle.isBracket) and (not DrawStyle.isURL) and UseStyler then
              LoadFromItemStyle
            else
            begin
              if DrawStyle.IsNumber and UseStyler then
              begin
                Font.Color := InternalStyles.NumberStyle.TextColor;
                Font.Style := InternalStyles.NumberStyle.Style;
                Brush.Color := InternalStyles.NumberStyle.BkColor;
              end;
              if DrawStyle.isdelimiter then loadfromitemstyle;
              if DrawStyle.iskeyWord then loadfromitemstyle;
              if DrawStyle.isURL then
              begin
                Font.Color := clBlack;// FUrlStyle.FTextColor;
                //Font.Style := FUrlStyle.Style;
                Brush.Color := clBlue;// FUrlStyle.FBkColor;
              end;
            end;
          end;
        end;

        if part <> '' then
        begin
          FindLineSelection(part);
          selcol := LineCanvas.Font.Color;
          brushcol := LineCanvas.Brush.Color;
          LineCanvas.Brush.Style:= bsClear;
          if s1 <> '' then
          begin
            DrawText(LineCanvas.Handle, PChar(s1), length(s1), rct1,
              DT_LEFT or DT_SINGLELINE or DT_NOPREFIX{ or DT_NOCLIP});

            rct1.Left := rct1.Left + LineCanvas.TextWidth(s1);
          end;
          if s2 <> '' then
          begin
            DrawText(LineCanvas.Handle, PChar(s2), length(s2), rct1,
              DT_LEFT or DT_SINGLELINE or DT_NOPREFIX{ or DT_NOCLIP});

              rct1.Left := rct1.Left + LineCanvas.TextWidth(s2);
          end;
          if s3 <> '' then
          begin
            LineCanvas.Font.Color := selcol;
            LineCanvas.Brush.Color := brushcol;
            DrawText(LineCanvas.Handle, PChar(s3), length(s3), rct1,
              DT_LEFT or DT_SINGLELINE or DT_NOPREFIX{ or DT_NOCLIP});
            rct1.Left := rct1.Left + LineCanvas.TextWidth(s3);;
          end;
          Inc(posln, length(Part));
        end;
      end;
    end;
  end;

  procedure BufferingDraw(part:string;var bufstyle: TStyle);

    function egalstyle(stl1,stl2:Tstyle): Boolean;
    begin
      Result :=
      (stl1.isComment       = stl2.isComment) and
      (stl1.isBracket       = stl2.isBracket) and
      (stl1.isnumber        = stl2.isnumber)  and
      (stl1.iskeyWord       = stl2.iskeyWord) and
      (stl1.isdelimiter     = stl2.isdelimiter) and
      (stl1.isURL           = stl2.isURL) and
      (stl1.EndBracket      = stl2.EndBracket) and
      (stl1.index           = stl2.index);
    end;

    procedure ResetPartStyle;
    begin
      bufstyle.isnumber := False;
      bufstyle.iskeyWord := False;
      bufstyle.isdelimiter := False;
      bufstyle.isURL := False;
    end;

  begin
    if egalstyle(bufstyle,backupstyle)  then
    begin
      backupstring := backupstring + part;
    end
    else
    begin
      DrawPart(backupstring,backupstyle);
      backupstyle := bufstyle;
      backupstring := part;
    end;
    resetPartStyle;
  end;

  //------------- DRAW SEGMENTS ---------------------
  procedure DrawSegments(S: string; var rct: TRect;
    var SegmentStyle: Tstyle);
  var
    i, j, len, toStart, toEnd, Innr, lc, rc: integer;
    done, WasPoint: boolean;
    validno: boolean;
    part: string;
    numsallowed:string;
  begin
    s := string(PChar(s));
    if not (Assigned(InternalStyles) and UseStyler) then
    begin
      BufferingDraw(s, SegmentStyle);
      Exit;
    end;

    toStart := 1;
    validno := True;
    done := false;
    while S <> '' do
    begin
      Len := Length(S);
      if (len = 0) or (tostart > len) then
        Exit;

      if not done then
      begin
        validno := (toStart = 1) or (s[toStart] = #32) or
          ((AnsiPos(S[toStart], FTempDelimiters) > 0) or (FTempDelimiters = ''));
      end;

      done := False;

      // Parse for multi-line comments
      if (not SegmentStyle.isBracket) then
      if (InternalStyles.MultiCommentLeft <> '') and
        (InternalStyles.MultiCommentRight <> '') and UseStyler then
      begin
        if SegmentStyle.isComment > 0 then
        begin
          rc := AnsiPos(InternalStyles.MultiCommentRight, s);
          if (rc > 0) then
          begin
            BufferingDraw(copy(s, 1,
              rc + length(InternalStyles.MultiCommentRight) - 1), SegmentStyle);
            Delete(s, 1, rc + length(InternalStyles.MultiCommentRight) - 1);
            decz(SegmentStyle.isComment);
            len := length(s);
            if len = 0 then
              Exit;
          end
          else
          begin
            BufferingDraw(s, SegmentStyle);
            Exit;
          end;
        end
        else
        begin
          // rc := ansipos(InternalStyles.LineComment, s);
          // For canceling the multi-line comment

          lc := AnsiPos(InternalStyles.MultiCommentLeft, s);

          if (lc = tostart) {and ((lc < rc) or (rc = 0))} then
//          if (lc > 0) and ((lc < rc) or (rc = 0)) and (not SegmentStyle.isBracket) then
          begin
            //part := copy(s, 1, lc - 1);
            //BufferingDraw(part, SegmentStyle);
            Delete(s, 1, (lc - 1) + length(InternalStyles.MultiCommentLeft));
            inc(SegmentStyle.isComment);
            BufferingDraw(InternalStyles.MultiCommentLeft, SegmentStyle);
            len := length(s);
            if len = 0 then
              Exit;
            done := True;
          end
        end;
      end;

      if not done then
      begin
        // line comment
        if (SegmentStyle.isComment = 0)  then
        begin
          if (AnsiPos(InternalStyles.LineComment, s) = tostart) and (not SegmentStyle.isBracket) then
          begin
            part := copy(s, tostart, len - tostart + 1);
            inc(SegmentStyle.isComment);
            BufferingDraw(part, SegmentStyle);
            isinlinecomment := True;
            Exit;
          end;

          // parse for bracket
          if (SegmentStyle.isBracket) and (SegmentStyle.EndBracket <> #0) then
          begin
            // literal output

            if s[tostart] = lit then
            begin
              BufferingDraw(s[tostart], SegmentStyle);
              Delete(s, tostart, 1);
              len := length(s);
              if tostart > len then
                Exit;

              BufferingDraw(s[tostart], SegmentStyle);
              Delete(s, tostart, 1);
              len := length(s);
              if tostart > len then
                Exit;
              done := True;
              Continue;
            end;

            // end of bracket string detected here
            if s[tostart] = SegmentStyle.EndBracket then
            begin
              BufferingDraw(s[tostart], SegmentStyle);
              Delete(s, tostart, 1);
              SegmentStyle.isBracket := False;
              validno := False;
              done := True;
              Continue;
            end
            else
            begin
              BufferingDraw(s[tostart], SegmentStyle);
              inc(tostart);
              len := length(s);
              if tostart > len then
                Exit;
              done := True;
            end;
          end
          else
          begin
            SegmentStyle.EndBracket := #0;

            for lc := 0 to InternalStyles.AllStyles.Count - 1 do
            begin
              if InternalStyles.AllStyles.Items[lc].StyleType <> stBracket then
                Continue;

              SegmentStyle.EndBracket :=
                InternalStyles.AllStyles.Items[lc].BracketEnd;

              SegmentStyle.index := lc;
              if (SegmentStyle.EndBracket <> #0) and
                (s[toStart] = SegmentStyle.EndBracket) then
              begin
                SegmentStyle.isBracket := True;
                SegmentStyle.EndBracket := InternalStyles.AllStyles.Items[lc].BracketEnd;
                Break;
              end;
            end;

            if SegmentStyle.isBracket then
            begin
              BufferingDraw(s[toStart], SegmentStyle);
              Delete(s, toStart, 1);
              Continue;
            end;
          end;
        end;
      end; //End if not done

      len := length(s);
      if (Len = 0) or (toStart > len) then
        Exit;

      if not done then
        for i := 0 to InternalStyles.AllStyles.Count - 1 do
        begin
          if InternalStyles.AllStyles.Items[i].StyleType <> stSymbol then
            Continue;

          if (toStart <= len) and
             (AnsiPos(S[toStart], InternalStyles.AllStyles.Items[i].Symbols) > 0) then
          begin
            SegmentStyle.isDelimiter := True;
            SegmentStyle.index := i;
            BufferingDraw(s[toStart], SegmentStyle);
            Delete(s, toStart, 1);
            validno := True;
            Len := Length(S);
            done := True;
            Break;
          end;
        end;

      if done then
        Continue;

      toEnd := tostart;
      if (len = 0) or (tostart > Len) then
        Exit;

      if validno then
        if (AnsiPos(UpCase(S[tostart]),FtmpNoStart) > 0) then
        begin
          if pos(FtmpNoHex,Uppercase(s)) = toStart then
          begin
            numsallowed := FtmpNo + 'ABCDEF';
            toEnd := toEnd + length(ftmpnohex);
          end
          else
            numsallowed := FtmpNo;

          WasPoint := False;
          Innr := toStart;

          while ((toEnd <= Len) and (AnsiPos(UpCase(S[toEnd]),numsallowed) > 0))  do
          begin
            if UpperCase(copy(s,tostart,toend)) = FtmpNoHex then
              numsallowed := FtmpNo + 'ABCDEF';

            if S[toEnd] = '.' then
            begin
              if WasPoint then
              begin
                toEnd := Innr;
                Break;
              end;
              WasPoint := True;
              Innr := toEnd;
            end;
            Inc(toEnd);
          end;

          Dec(toEnd);

          if (tostart <= toend) then
          begin
            SegmentStyle.isDelimiter := False;
            SegmentStyle.isNumber := True;
            part := copy(s, tostart, toend - tostart + 1);
            Delete(s, tostart, toend - tostart + 1);
            BufferingDraw(part, SegmentStyle);
            validno := False;
            done := True;
          end;
        end;
      if done then continue;

      Len := Length(S);
      if (len = 0) or (tostart > Len) then
        Exit;

      toend := tostart;

      while (toend <= Len) and (S[toend] <> #32) and
        (AnsiPos(S[toend], FTempdelimiters) = 0) do
          Inc(toend);

      part := Copy(S, toStart, toEnd - toStart);
      if (part <> '') and (validno) then
        for i := 0 to InternalStyles.AllStyles.Count - 1 do
        begin
          if InternalStyles.AllStyles.Items[i].StyleType = stKeyword then
          begin
            if done then
              Break;

            for j := 0 to InternalStyles.AllStyles.Items[i].KeyWords.Count - 1 do
              if Equal(part, InternalStyles.AllStyles.Items[i].KeyWords.Strings[j]) then
              begin
                SegmentStyle.iskeyWord := True;
                SegmentStyle.index := i;

                if InternalStyles.CustomDraw then
                begin
                  BufferingDraw(backupstring,SegmentStyle);

                  InternalStyles.DrawKeyword(LineCanvas,part,rct1);
                  backupstring := '';
                end
                else
                  BufferingDraw(part, SegmentStyle);

                Delete(s, toStart, toend - tostart);
                done := True;
                Break;
              end;
          end;
        end;

      if done then
        Continue;

      if not done then
      begin
        BufferingDraw(s[toStart], SegmentStyle);
      end;
      inc(toStart);
    end;
  end;

begin
  rct0 := PR;
  LineRct := PR;
  rct1 := PR;

  LineCanvas := ACanvas;
  s := '';
  if Assigned(InternalStyles) then
     s := InternalStyles.Literal;

  if length(s) > 0 then
    lit := s[1]
  else
    lit := #0;

  S := FCodeBlocks.Items[LineNo].Code.Strings[SubLineNo];
  BackColor:= clWhite;

  rct := rct0;

  posln := 0;

  urls := TStringList.Create;
  backupstyle := style;
  ExtractURL(s, urls);
  isinlinecomment := False;

  for i := 0 to urls.Count - 1 do
  begin
    style.isURL := False;
    DrawSegments(urls.Strings[i], rct1, style);
  end;

  urls.Free;

  DrawPart(BackupString,BackupStyle);

  if isinlinecomment then
    style.isComment := 0;
end;

procedure TAdvCodeList.ExtractURL(s: string; var urls: TStringList);
begin
  if not Assigned(urls) then
    Exit;

  urls.Clear;
  urls.Add(s);
end;

procedure TAdvCodeList.KeyDown(var KeyCode: word; Shift: TShiftState);
begin
  inherited;
  if (KeyCode = VK_DELETE) and (ItemIndex <> -1) and (CodeBlocks.Count > 0) then
    DeleteBlock;

  FOldIndex := ItemIndex;
end;

procedure TAdvCodeList.KeyUp(var KeyCode: Word; Shift: TShiftState);
begin
  inherited;
  if (FOldIndex <> ItemIndex) and (ItemIndex >= 0) and Assigned(OnBlockClick) then
    OnBlockClick(Self, CodeBlocks[ItemIndex]);
  FOldIndex := ItemIndex;
end;

procedure TAdvCodeList.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var
  a: integer;
  P: TPoint;
begin
  inherited;

  FPtDown.X := X;
  FPtDown.Y := Y;

  P.X := X;
  P.Y := Y;
  a := ItemAtPos(P, True);

  if (a >= 0) then
  begin
    if (Button = mbLeft) and Assigned(FOnBlockClick) then
      FOnBlockClick(Self, CodeBlocks[a]);
    if (Button = mbRight) and Assigned(FOnBlockRightClick) then
      FOnBlockRightClick(Self, CodeBlocks[a]);
    FMouseDown := true;
  end;
end;

procedure TAdvCodeList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  a: integer;
  P: TPoint;
  s: string;
  dwEffects: Integer;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  P.X := X;
  P.Y := Y;
  a := ItemAtPos(P, True);

  if (a >= 0) then
  begin
    if InternalStyles <> nil then
      Hint := 'BEGIN' + CodeBlocks.Items[a].Code.Text
    else
      Hint := CodeBlocks.Items[a].Code.Text;

    if a <> FHintBlock then
    begin
      FHintBlock:= a;
      Application.CancelHint;
      SetActiveStyler(self.SyntaxStyles);
      Application.ActivateHint(P);
    end;
  end
  else
  begin
    hint:= '';
  end;

  if ((Abs(FPtDown.X - X) > 3) or (Abs(FPtDown.Y - Y) > 3)) and (a >= 0) and
     FOleDropSource and FMouseDown and not FIsDragging and not FIsDropping then
  begin
    FIsDragging := true;
    s := CodeBlocks.Items[a].Code.Text;
    StartTextDoDragDrop(s, '', DROPEFFECT_COPY or DROPEFFECT_MOVE, dwEffects);
    FIsDragging := false;
    FMouseDown := false;
    FIsDropping := false;
  end;
end;

procedure TAdvCodeList.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FMouseDown := false;
  FPtDown.X := $FFFF;
  FPtDown.Y := $FFFF;
end;

function TAdvCodeList.GetBlock(index: integer): String;
begin
  if (index >= FBlockCount) or (index < 0) then
    raise exception.Create('Invalid Block Index '+IntToStr(Index));
  Result:= TStringList(Items.Objects[index]).Strings[0];
end;

procedure TAdvCodeList.SetBlock(index: integer; Value: String);
begin
  if (index >= FBlockCount) or (index < 0) then
    raise exception.Create('Invalid Block Index '+IntToStr(Index));
  TStringList(Items.Objects[index]).Strings[0] := Value;
  Invalidate;
end;

procedure TAdvCodeList.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;

  if (AOperation = opRemove) and (AComponent = InternalStyles) then
    InternalStyles := nil;  
  inherited;
end;

procedure TAdvCodeList.DrawBorders;
var
  DC: HDC;
  OldPen: HPen;
  ARect: TRect;
begin
  DC := GetWindowDC(Handle);
  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    OldPen := SelectObject(DC,CreatePen( PS_SOLID,1,ColorToRGB(clGray)));

    MovetoEx(DC,ARect.Left ,ARect.Top ,nil);
    LineTo(DC,ARect.Right -1 ,ARect.Top );
    LineTo(DC,ARect.Right -1 ,ARect.Bottom - 1);
    LineTo(DC,ARect.Left,ARect.Bottom -1 );
    LineTo(DC,ARect.Left,ARect.Top );

    DeleteObject(SelectObject(DC,OldPen));
  finally
    ReleaseDC(Handle,DC);
  end;
end;

procedure TAdvCodeList.WMNCPaint(var Message: TMessage);
begin
  inherited;
  if BorderStyle = bsSingle then
    DrawBorders;
end;


procedure TAdvCodeList.WndProc(var Message: TMessage);
begin
  inherited;
  if Message.Msg = WM_DESTROY then
  begin
    if FOleDropTargetAssigned then
    begin
      RevokeDragDrop(self.Handle);
      FOleDropTargetAssigned := false;
    end;
  end;

  if not (csDestroying in ComponentState) then
  begin
    if Message.Msg = WM_NCPAINT then
    begin
      if BorderStyle = bsSingle then
        DrawBorders;
    end;
  end;
end;

procedure TAdvCodeList.WMChangeCBChain(var Msg: TMessage);
begin
  FClipChainHandle := Msg.LParam;
end;

procedure TAdvCodeList.WMDestroy(var Msg: TWMDestroy);
begin
  if FClipChainHandle <> 0 then
    ChangeClipboardChain(Handle, FClipChainHandle);
  FClipChainHandle := 0;
  inherited;
end;

procedure TAdvCodeList.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
begin
{$IFDEF DELPHI_UNICODE}
  inherited;
  Exit;
{$ENDIF}

  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      Perform(WM_ERASEBKGND, MemDC, MemDC);
      Message.DC := MemDC;
      WMPaint(Message);
      Message.DC := 0;
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;


procedure TAdvCodeList.WMDrawClipboard(var Msg: TMessage);
var
  newIndex: Integer;
  allow: boolean;
  clipstr: string;
begin
  SendMessage(FClipChainHandle, WM_DRAWCLIPBOARD,0,0);

  // Only do this when property EnableClipboardView is true
  if FClipboardView and not (csDesigning in ComponentState) then
  begin
    try
      Clipboard.Open;
      try
        if Clipboard.HasFormat(CF_TEXT) then
        begin
          clipstr := Clipboard.AsText;
          Allow := true;
          if Assigned(OnBeforeBlockInsertFromClipboard) then
            OnBeforeBlockInsertFromClipboard(Self, clipstr, Allow);

          if Allow then
          begin
            newIndex := 0;

            if FClipboardAppend then
            begin
              if Allow then
              begin
                CodeBlocks.Add(clipstr);
                newIndex := CodeBlocks.Count - 1;
              end;
            end
            else
              CodeBlocks.Insert(0, clipstr);

            Remeasure;

            if Assigned(FOnBlockInsertFromClipboard) then
              FOnBlockInsertFromClipboard(Self, CodeBlocks[newIndex]);
          end;

        end;
      finally
        Clipboard.Close;
      end;
    except
    // do nothing when opening clipboard fails
    end;
  end;
end;

procedure TAdvCodeList.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  P: TPoint;
  a: integer;
begin
  inherited;

  P.X := Message.XPos;
  P.Y := Message.YPos;
  a := ItemAtPos(P, True);

  if (a >= 0) then
  begin
    if Assigned(OnBlockDblClick) then
      OnBlockDblClick(Self, CodeBlocks[a]);
  end;
end;

procedure TAdvCodeList.SetCodeBlockCaptionColor(const Value: TColor);
begin
  if (FCodeBlockCaptionColor <> Value) then
  begin
    FCodeBlockCaptionColor := Value;
    Invalidate;
  end;
end;

procedure TAdvCodeList.SetCodeBlockCaptionSelectColor(const Value: TColor);
begin
  if (FCodeBlockCaptionSelectColor <> Value) then
  begin
    FCodeBlockCaptionSelectColor := Value;
    Invalidate;
  end;
end;

procedure TAdvCodeList.SetCodeBlockCaptionTextColor(const Value: TColor);
begin
  if (FCodeBlockCaptionTextColor <> Value) then
  begin
    FCodeBlockCaptionTextColor := Value;
    Invalidate;
  end;
end;

procedure TAdvCodeList.SetCodeBlockCaptionSelectTextColor(const Value: TColor);
begin
  if (FCodeBlockCaptionTextColor <> Value) then
  begin
    FCodeBlockCaptionTextColor := Value;
    Invalidate;
  end;
end;

procedure TAdvCodeList.SetCodeBlockColor(const Value: TColor);
begin
  if (FCodeBlockColor <> Value) then
  begin
    FCodeBlockColor := Value;
    Invalidate;
  end;
end;

procedure TAdvCodeList.SetCodeBlockColorTo(const Value: TColor);
begin
  if (FCodeBlockColorTo <> Value) then
  begin
    FCodeBlockColorTo := Value;
    Invalidate;
  end;
end;

procedure TAdvCodeList.SetCodeBlockSelectColor(const Value: TColor);
begin
  if (FCodeBlockSelectColor <> Value) then
  begin
    FCodeBlockSelectColor := Value;
    Invalidate;
  end;
end;

procedure TAdvCodeList.SetCodeBlockSelectColorTo(const Value: TColor);
begin
  if (FCodeBlockSelectColorTo <> Value) then
  begin
    FCodeBlockSelectColorTo := Value;
    Invalidate;
  end;
end;

procedure TAdvCodeList.SetCodeBorderColor(const Value: TColor);
begin
  FCodeBorderColor := Value;
  Invalidate;
end;

procedure TAdvCodeList.SetCodeBorderSelectColor(const Value: TColor);
begin
  FCodeBorderSelectColor := Value;
  Invalidate;
end;

procedure TAdvCodeList.SetInternalStyles(const Value: TAdvCustomMemoStyler);
begin
  InternalStyles := Value;
  SetActiveStyler(Value);
end;

procedure TAdvCodeList.SetItemSize(Index, Size: integer);
begin
 SendMessage(Handle,LB_SETITEMHEIGHT,Index, Size);
end;

procedure TAdvCodeList.SetItemStyle(const Value: TItemStyle);
begin
  FItemStyle := Value;
  case FItemStyle of
    isFixedHeight: inherited Style := lbOwnerDrawFixed;
    isVariableHeight: inherited Style := lbOwnerDrawVariable;
  end;
end;

procedure TAdvCodeList.SetOleDropSource(const Value: Boolean);
begin
  FOleDropSource := Value;
end;

procedure TAdvCodeList.SetOleDropTarget(const Value: Boolean);
begin
  FOleDropTarget := Value;

  if not (csDesigning in ComponentState) then
  begin
    if FOleDropTarget then
    begin
      FOleDropTargetAssigned := RegisterDragDrop(self.Handle, TCodeListDropTarget.Create(self)) = s_OK;
    end
    else
      if FOleDropTargetAssigned then RevokeDragDrop(self.Handle);
  end;
end;
procedure TAdvCodeList.SetUseStyler(const Value: boolean);
begin
  if (FUseStyler <> Value) then
  begin
    FUseStyler := Value;
    Invalidate;
  end;
end;


procedure TAdvCodeList.Loaded;
begin
  inherited;

  if not (csDesigning in ComponentState) and ShowHint then
  begin
    //HintWindowClass := TCustomHintWindow;
    //with Application do
    //begin
    //  ShowHint := not ShowHint;
    //  ShowHint := not ShowHint;
    //end;
  end;
end;

procedure TAdvCodeList.ReMeasure;
var
  i, Height: integer;
begin
  if ItemStyle = isFixedHeight then
    Exit;

  for i := 0 to Items.Count - 1 do
  begin
    Height := ItemHeight;
    if Assigned(OnMeasureItem) then
      OnMeasureItem(Self, i, Height);
    SendMessage(Handle,LB_SETITEMHEIGHT,i,Height);
  end;
end;

procedure TAdvCodeList.Resize;
begin
  inherited;
  Invalidate;
end;

procedure TAdvCodeList.DeleteBlock;
var
  Allow: Boolean;
begin
  // Attempt Code Delete...
  // Is Delete Event Handler Assigned ?
  if Assigned(OnBlockDelete) then
  begin
    // Yes...
    Allow := true;
    OnBlockDelete(Self, CodeBlocks.Items[ItemIndex], Allow);
    if Allow then
      CodeBlocks.Delete(ItemIndex);
  end
  else
    CodeBlocks.Delete(ItemIndex);

  Remeasure;

  if Assigned(OnAfterBlockDelete) then
    OnAfterBlockDelete(Self, ItemIndex);
end;

function TAdvCodeList.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

function TAdvCodeList.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

procedure TAdvCodeList.SetVersion(const Value: string);
begin

end;

procedure TAdvCodeList.SetCodeBlocks(const Value: TCodeBlocks);
begin
  FCodeBlocks.Assign(Value);
  Remeasure;
end;


{ TCodeListDropTarget }

constructor TCodeListDropTarget.Create(ACodeList: TAdvCodeList);
begin
  inherited Create;
  FCodeList := ACodeList;
end;

procedure TCodeListDropTarget.DragMouseMove(pt: TPoint; var allow: Boolean);
begin
  inherited;
  FCodeList.FIsDropping := true;
end;

procedure TCodeListDropTarget.DropText(pt: tpoint; s: string);
var
  cb: TCodeBlock;
begin
  cb := FCodeList.CodeBlocks.Add(s);
  if Assigned(FCodeList.OnBlockDropped) then
    FCodeList.OnBlockDropped(Self, cb);

  FCodeList.FIsDropping := false;
end;


end.
