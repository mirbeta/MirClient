{************************************************************************}
{ THTMLTreeView component                                                }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ Copyright © 1999-2015                                                  }
{   TMS Software                                                         }
{   Email : info@tmssoftware.com                                         }
{   Web : http://www.tmssoftware.com                                     }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{************************************************************************}

unit htmltv;

{$I TMSDEFS.INC}
{$R HTMLTV.RES}
{$DEFINE REMOVEDRAW}

interface

uses
   Windows, Classes, ComCtrls, Messages, Controls, Graphics, Forms, Dialogs, AdvGradient, AdvStyleIF
   , HTMLXPVS,PictureContainer, ImgList
   , Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.1.0.1 : added support to switch from csTheme to csWinXP automatically on non XP machines
  // v1.1.0.2 : fix for drawing with Windows XP visual themes enabled
  // v1.1.0.3 : improved OnAnchorEnter event triggering
  // v1.1.0.4 : Fixed issue with HotTrack = true
  // v1.1.1.0 : Exposed MultiSelectStyle property
  // v1.1.1.1 : Fixed painting issue with HotTrack = true
  // v1.1.1.2 : Fixed painting issue on dbl click to open node
  // v1.1.1.3 : Fixed painting issue with images & checkbox
  // v1.1.2.0 : New : function NodeText(node: TTreeNode): string added to return text of node without HTML tags
  // v1.2.0.0 : New : SelectionColors property added, Office2007, Windows Vista, Windows 7 color styles
  //          : New : TAdvFormStyler / ITMSStyle interface compatible
  //          : Improved : DFM property persistence
  // v1.2.1.0 : New : support for customizing bullets in HTML UL lists
  // v1.3.0.0 : New : support for Office 2010 color styles added
  // v1.3.1.0 : New : GlyphRadio / GlyphRadioSelected added
  // v1.3.1.1 : Fixed : Access violation in very specific circumstances when setting the selected node
  // v1.3.1.2 : Fixed : Issue with state images & checkbox items
  // v1.3.1.3 : Fixed : Issue with space char during editing
  // v1.3.1.4 : Fixed : Issue with selection of nodes with short text & image + checkbox
  // v1.3.2.0 : New : Property AutoChildCheck added
  // v1.3.2.1 : Fixed : Issue with ItemHeight & reparenting
  // v1.4.0.0 : New : Support for PNG images via images in associated PictureContainer
  // v1.4.0.1 : Improved : Painting of state images when checkboxes are used
  // v1.4.0.2 : Fixed : Issue with OnCustomDraw
  // v1.4.0.3 : Fixed : Issue with use of state images & hyperlinks
  // v1.4.1.0 : New : WordWrap property added
  //          : Fixed : Issue with SelectionNFColor
  //          : Fixed : Issue with readonly behavior of checkboxes & radiobuttons
  // v1.4.1.1 : Fixed : Issue with width calculation


type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TAnchorClick = procedure(Sender:TObject;Node:TTreeNode;anchor:string) of object;

  TCheckBoxClick = procedure(Sender:TObject;Node:TTreeNode;Check: Boolean) of object;

  TRadioClick = procedure(Sender: TObject; Node: TTreeNode) of object;

  TControlStyle = (csClassic,csFlat,csWinXP,csBorland,csTMS,csGlyph,csTheme);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THTMLTreeview = class(TCustomTreeView, ITMSStyle)
  private
    { Private declarations }
    FLastHintNode : TTreeNode;
    FHintColor : TColor;
    FHTMLHint: Boolean;
    FIndent: Integer;
    FOldCursor: Integer;
    FOldScrollPos: Integer;
    FURLColor: TColor;
    FSelectionFontColor: TColor;
    FItemHeight: Integer;
    FOldAnchor: string;
    FAnchorClick: TAnchorClick;
    FAnchorEnter: TAnchorClick;
    FAnchorExit: TAnchorClick;
    FImages:TImageList;
    FOnCheckBoxClick: TCheckBoxClick;
    FControlStyle: TControlStyle;
    FControlColor: TColor;
    FGlyphUnChecked: TBitmap;
    FGlyphChecked: TBitmap;
    FGlyphRadio: TBitmap;
    FGlyphRadioSelected: TBitmap;
    FIsWinXP: Boolean;
    FOnRadioClick: TRadioClick;
    FSelectionNFFontColor: TColor;
    FSelectionNFColor: TColor;
    FUpdateCount: Integer;
    FImageCache: THTMLPictureCache;
    FSelectionColors: TGradientStyle;
    FAutoChildCheck: boolean;
    FWordWrap: boolean;
    FReadOnlyEx: boolean;
    procedure CMDesignHitTest(var message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CNNotify(var message: TWMNotify); message CN_NOTIFY;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMRButtonDown(var message: TWMLButtonDown); message WM_RBUTTONDOWN;
    procedure WMLButtonDown(var message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var message: TWMLButtonDown); message WM_LBUTTONUP;
    procedure WMMouseMove(var message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMHScroll(var message:TMessage); message WM_HSCROLL;
    procedure WMSize(var message:TMessage); message WM_SIZE;
    function GetItemHeight: integer;
    procedure SetItemHeight(const Value: integer);
    procedure SetSelectionFontColor(const Value: TColor);
    procedure SetURLColor(const Value: TColor);
    procedure SetHintColor(const Value: TColor);
    procedure SetImages(const Value: TImageList);
    procedure SetControlStyle(const Value: TControlStyle);
    procedure SetControlColor(const Value: TColor);
    procedure SetGlyphChecked(const Value: TBitmap);
    procedure SetGlyphUnChecked(const Value: TBitmap);
    procedure SetSelectionNFColor(const Value: TColor);
    procedure SetSelectionNFFontColor(const Value: TColor);
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure ShowH(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    function GetStringExtent(s: string): integer;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetSelectionColors(const Value: TGradientStyle);
    procedure SetGlyphRadio(const Value: TBitmap);
    procedure SetGlyphRadioSelected(const Value: TBitmap);
    procedure SetWordWrap(const Value: boolean);
    function GetReadOnlyEx: boolean;
    procedure SetReadOnlyEx(const Value: boolean);
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure Expand(Node: TTreeNode); override;
    procedure KeyPress(var Key: Char); override;
    procedure DrawCheck(ACanvas: TCanvas; R: TRect; State: Boolean);
    procedure DrawRadio(ACanvas: TCanvas; R:TRect;State: Boolean);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetNodeCheck(Node: TTreeNode; Check: Boolean);
    procedure RemoveNodeCheck(Node: TTreeNode);
    function GetNodeCheck(Node: TTreeNode; var Check: Boolean): Boolean;
    procedure SetRadioButton(Node: TTreeNode; Check: Boolean);
    procedure RemoveRadioButton(Node: TTreeNode);
    function GetRadioButton(Node: TTreeNode; var Check: Boolean): Boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
    function NodeText(Node: TTreeNode): string;
    procedure SetComponentStyle(AStyle: TTMSStyle);
  published
   { Published declarations }
    property AutoChildCheck: boolean read FAutoChildCheck write FAutoChildCheck default false;
    property GlyphChecked: TBitmap read FGlyphChecked write SetGlyphChecked;
    property GlyphUnChecked: TBitmap read FGlyphUnChecked write SetGlyphUnChecked;
    property GlyphRadio: TBitmap read FGlyphRadio write SetGlyphRadio;
    property GlyphRadioSelected: TBitmap read FGlyphRadioSelected write SetGlyphRadioSelected;
    property ControlStyle: TControlStyle read FControlStyle write SetControlStyle default csClassic;
    property ControlColor: TColor read FControlColor write SetControlColor default clRed;
    property HTMLImages: TImageList read FImages write SetImages;

    property HTMLHint: Boolean read FHTMLHint write FHTMLHint default False;

    property ItemHeight: Integer read GetItemHeight write SetItemHeight;

    property ReadOnly: boolean read GetReadOnlyEx write SetReadOnlyEx default false;

    property SelectionColors: TGradientStyle read FSelectionColors write SetSelectionColors;
    property SelectionFontColor: TColor read FSelectionFontColor write SetSelectionFontColor default clHighLightText;

    property SelectionNFColor: TColor read FSelectionNFColor write SetSelectionNFColor;
    property SelectionNFFontColor: TColor read FSelectionNFFontColor write SetSelectionNFFontColor;

    property URLColor: TColor read fURLColor write SetURLColor default clBlue;
    property HintColor: TColor read FHintColor write SetHintColor default clNone;
    property OnAnchorClick: TAnchorClick read FAnchorClick write FAnchorClick;
    property OnAnchorEnter: TAnchorClick read FAnchorEnter write FAnchorEnter;
    property OnAnchorExit: TAnchorClick read FAnchorExit write FAnchorExit;
    property OnCheckBoxClick: TCheckBoxClick read FOnCheckBoxClick write FOnCheckBoxClick;
    property OnRadioClick: TRadioClick read FOnRadioClick write FOnRadioClick;

    property Align;
    property Anchors;
    property AutoExpand;
    property BiDiMode;
    property BorderWidth;
    property ChangeDelay;
    property Constraints;
    property DragKind;
    property HotTrack;
    property ParentBiDiMode;
    property RowSelect;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnEndDock;
    property OnStartDock;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    //property Images;
    property Indent;
    property Items;
    property MultiSelect;
    property MultiSelectStyle;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RightClickSelect;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property StateImages;
    {$IFDEF DELPHIXE4_LVL}
    property StyleElements;
    {$ENDIF}
    property TabOrder;
    property TabStop default True;
    property Visible;
    property WordWrap: boolean read FWordWrap write SetWordWrap default false;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsing;
    property OnCollapsed;
    property OnCompare;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanding;
    property OnExpanded;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property Version: string read GetVersion write SetVersion; 
  end;


implementation

uses
  Commctrl,
  ShellApi, SysUtils;


{$I HTMLENGO.PAS}

procedure THTMLTreeview.CMDesignHitTest(var message: TCMDesignHitTest);
begin
  if htOnItem in GetHitTestInfoAt(message.XPos,message.YPos) then
    message.Result := 1
  else
    message.Result := 0;
end;

procedure THTMLTreeview.DrawRadio(ACanvas: TCanvas; R:TRect;State: Boolean);
var
  DrawState: Integer;
  DrawRect: TRect;
  BMP: TBitmap;
  HTheme: THandle;
  OldColor: TColor;
begin
  case ControlStyle of
  csClassic,csFlat:
    begin
      DrawState := DFCS_BUTTONRADIO;

      if State then
        DrawState := DrawState or DFCS_CHECKED;

      if ControlStyle = csFlat then
        DrawState := DrawState or DFCS_FLAT;

      DrawRect.Left := R.Left + (R.Right - R.Left - 12) div 2;
      DrawRect.Top:= R.Top + (R.Bottom - R.Top - 12) div 2;
      DrawRect.Right := DrawRect.Left + 12;
      DrawRect.Bottom := DrawRect.Top + 12;

      DrawFrameControl(ACanvas.Handle,DrawRect,DFC_BUTTON,DrawState);
    end;
  csTMS:
    begin
      Bmp := TBitmap.Create;
      try
        if State then
          Bmp.LoadFromResourceName(hinstance,'TVRAD01')
        else
          Bmp.LoadFromResourceName(hinstance,'TVRAD02');

        Bmp.Transparent := True;
        Bmp.TransparentMode := tmAuto;

        ACanvas.Draw(R.Left,R.Top,bmp);
      finally
        Bmp.free;
      end;
    end;
  csGlyph:
    begin
      if State and not GlyphRadioSelected.Empty then
      begin
        GlyphRadioSelected.Transparent := True;
        GlyphRadioSelected.TransparentMode := tmAuto;
        ACanvas.Draw(R.Left,R.Top,GlyphRadioSelected);
      end;

      if not State and not GlyphRadio.Empty then
      begin
        GlyphRadio.Transparent := True;
        GlyphRadio.TransparentMode := tmAuto;
        ACanvas.Draw(R.Left,R.Top,GlyphRadio);
      end;
    end;
  csTheme:
    begin
      if FIsWinXP then
      begin
        HTheme := OpenThemeData(Self.Handle,'button');

        r := Rect(R.Left, R.Top, R.Left + 14, R.Top + 14);

        if State then
          DrawThemeBackground(HTheme,ACanvas.Handle, BP_RADIOBUTTON,RBS_CHECKEDNORMAL,@r,nil)
        else
          DrawThemeBackground(HTheme,ACanvas.Handle, BP_RADIOBUTTON,RBS_UNCHECKEDNORMAL,@r,nil);
        CloseThemeData(HTheme);
      end;
    end;
  csWinXP:
    begin
      Bmp := TBitmap.Create;
      try
        if State then
          Bmp.LoadFromResourceName(hinstance,'TVRAD03')
        else
          Bmp.LoadFromResourceName(hinstance,'TVRAD04');

        Bmp.Transparent := True;
        Bmp.TransparentMode := tmAuto;

        ACanvas.Draw(R.Left,R.Top,bmp);
      finally
        Bmp.free;
      end;
    end;

  csBorland:
    begin
      OldColor := ACanvas.Brush.Color;

      ACanvas.Brush.Color := clBtnFace;

      ACanvas.Polygon([Point(r.Left + 2,r.Top + 8),
                      Point(r.Left + 8,r.Top + 2),
                      Point(r.Left + 14,r.Top + 8),
                      Point(r.Left + 8,r.Top + 14)]);

      if State then
        ACanvas.Pen.Color := clGray
      else
        ACanvas.Pen.Color := clWhite;

      ACanvas.MoveTo(r.Left + 8,r.Top + 14);
      ACanvas.LineTo(r.Left + 2,r.Top + 8);
      ACanvas.LineTo(r.Left + 8,r.Top + 2);

      if State then
        ACanvas.Pen.Color := clWhite
      else
        ACanvas.Pen.Color := clGray;

      ACanvas.LineTo(r.Left + 14,r.Top + 8);
      ACanvas.LineTo(r.Left + 8,r.Top + 14);

      ACanvas.Brush.Color := clRed;
      ACanvas.Pen.Color := clRed;

      if State then
        ACanvas.Polygon([Point(r.Left + 6,r.Top + 8),
                        Point(r.Left + 8,r.Top + 6),
                        Point(r.Left + 10,r.Top + 8),
                        Point(r.Left + 8,r.Top + 10)]);

      ACanvas.Brush.Color := OldColor;

    end;
  end;
end;



procedure THTMLTreeview.DrawCheck(ACanvas: TCanvas; R:TRect;State: Boolean);
var
  DrawState: Integer;
  DrawRect: TRect;
  BMP: TBitmap;
  HTheme: THandle;
begin
  case ControlStyle of
  csClassic,csFlat:
    begin
      if State then
        DrawState := DFCS_BUTTONCHECK or DFCS_CHECKED
      else
        DrawState := DFCS_BUTTONCHECK;

      if ControlStyle = csFlat then
        DrawState := DrawState or DFCS_FLAT;

      DrawRect.Left := R.Left + (R.Right - R.Left - 14) div 2;
      DrawRect.Top:= R.Top + (R.Bottom - R.Top - 14) div 2;
      DrawRect.Right := DrawRect.Left + 14;
      DrawRect.Bottom := DrawRect.Top + 14;

      DrawFrameControl(ACanvas.Handle,DrawRect,DFC_BUTTON,DrawState);
    end;
  csTMS:
    begin
      Bmp := TBitmap.Create;
      try
        if State then
          Bmp.LoadFromResourceName(hinstance,'TVCHK01')
        else
          Bmp.LoadFromResourceName(hinstance,'TVCHK02');

        Bmp.Transparent := True;
        Bmp.TransparentMode := tmAuto;

        ACanvas.Draw(R.Left,R.Top,bmp);
      finally
        Bmp.free;
      end;
    end;
  csGlyph:
    begin
      if State and not GlyphChecked.Empty then
      begin
        GlyphChecked.Transparent := True;
        GlyphChecked.TransparentMode := tmAuto;
        ACanvas.Draw(R.Left,R.Top,GlyphChecked);
      end;

      if not State and not GlyphUnChecked.Empty then
      begin
        GlyphUnChecked.Transparent := True;
        GlyphUnChecked.TransparentMode := tmAuto;
        ACanvas.Draw(R.Left,R.Top,GlyphUnChecked);
      end;
    end;
  csTheme:
    begin
      if FIsWinXP then
      begin
        HTheme := OpenThemeData(Self.Handle,'button');

        r := Rect(R.Left, R.Top, R.Left + 14, R.Top + 14);

        if State then
          DrawThemeBackground(HTheme,ACanvas.Handle, BP_CHECKBOX,CBS_CHECKEDNORMAL,@r,nil)
        else
          DrawThemeBackground(HTheme,ACanvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDNORMAL,@r,nil);
          CloseThemeData(HTheme);
      end;
    end;
  csWinXP:
    begin
      Bmp := TBitmap.Create;
      try
        if State then
          Bmp.LoadFromResourceName(hinstance,'TVCHK03')
        else
          Bmp.LoadFromResourceName(hinstance,'TVCHK04');

        Bmp.Transparent := True;
        Bmp.TransparentMode := tmAuto;

        ACanvas.Draw(R.Left,R.Top,bmp);
      finally
        Bmp.free;
      end;
    end;

  csBorland:
    begin
      if Enabled then
        ACanvas.Brush.Color := clBtnFace
      else
        ACanvas.Brush.Color := clBtnShadow;

      ACanvas.Pen.Color := clBtnFace;
      ACanvas.Rectangle(R.Left,R.Top,R.Right,R.Bottom);
      ACanvas.Pen.Color := clBtnHighLight;
      ACanvas.MoveTo(R.Left,R.Bottom);
      ACanvas.LineTo(R.Left,R.Top);
      ACanvas.LineTo(R.Right,R.Top);
      ACanvas.Pen.Color := clBtnShadow;
      ACanvas.LineTo(R.Right,R.Bottom);
      ACanvas.LineTo(R.Left,R.Bottom);

      if State then
      begin
        ACanvas.Pen.Color := FControlColor;
        ACanvas.Pen.Width := 1;
        Dec(R.Top);
        Dec(R.Bottom);
        ACanvas.MoveTo(R.Left + 2,R.Top + 14 div 2 + 1);
        ACanvas.LineTo(R.Left + 2,R.Bottom - 1);
        ACanvas.MoveTo(R.Left + 3,R.Top + 14 div 2);
        ACanvas.LineTo(R.Left + 3,R.Bottom - 2);
        ACanvas.MoveTo(R.Left + 2,R.Bottom - 1);
        ACanvas.LineTo(R.Right - 2,R.Top + 3);
        ACanvas.MoveTo(R.Left + 3,R.Bottom - 1);
        ACanvas.LineTo(R.Right - 1,R.Top + 3);
      end;
    end;
  end;
end;

procedure THTMLTreeview.CNNotify(var message: TWMNotify);
var
  TVcd: TNMTVCustomDraw;
  TVdi: TTVDISPINFO;
  canvas: TCanvas;
  a,s,fa: string;
  xsize,ysize,ml,hl,imgs: Integer;
  tn: TTreenode;
  r,cr,hr,nr: TRect;
  urlcol,clr,bclr: TColor;
  DefaultDraw,code: Boolean;
  DrawState: TCustomDrawState;
  hasChk: boolean;

begin
  if message.NMHdr^.code = TVN_GETDISPINFO then
  begin
    TVDi := PTVDispInfo(pointer(message.nmhdr))^;

    if (tvdi.item.mask and TVIF_TEXT = TVIF_TEXT) then
    begin
      tn := Items.GetNode(tvdi.item.hitem);

      if Assigned(tn) then
      begin
        s := HTMLStrip(tn.Text);

        // Compensate for checkbox or image
       if (tn.ImageIndex > 0) and not Dragging then
          s := s + 'wwwwwwww';

        if WordWrap then
          StrpLCopy(tvdi.item.pszText,s,5)
        else
          StrpLCopy(tvdi.item.pszText,s,255);
      end;

      tvdi.item.Mask := tvdi.item.Mask or TVIF_DI_SETITEM;
      message.Result := 0;
      Exit;
    end;
  end;

  if message.NMHdr^.code = NM_CUSTOMDRAW then
  begin
    FIndent := TreeView_GetIndent(self.Handle);

    TVcd := PNMTVCustomDraw(Pointer(message.NMHdr))^;

    case TVcd.nmcd.dwDrawStage of
    CDDS_PREPAINT:
      message.Result := CDRF_NOTIFYITEMDRAW or CDRF_NOTIFYPOSTPAINT;
    CDDS_ITEMPREPAINT:
      begin
        if  (TVcd.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED) then
        begin
          TVcd.nmcd.uItemState := TVcd.nmcd.uItemState and (not CDIS_SELECTED);
          SetTextColor(TVcd.nmcd.hdc,ColorToRGB(Color));
          SetBkColor(TVcd.nmcd.hdc,ColorToRGB(Color));
          TVcd.clrTextBk := ColorToRGB(Color);
          TVcd.clrText := ColorToRGB(Color);
        end
        else
        begin
          SetTextColor(TVcd.nmcd.hdc,ColorToRGB(Color));
          SetBkColor(TVcd.nmcd.hdc,ColorToRGB(Color));
        end;
        message.Result := CDRF_NOTIFYPOSTPAINT;
      end;
    CDDS_ITEMPOSTPAINT:
      begin
        tn := Items.GetNode(HTReeItem(TVcd.nmcd.dwitemSpec));

        if Assigned(tn) then
        begin
          code := true;
          TreeView_GetItemRect(handle, HTREEITEM(TVcd.nmcd.dwItemSpec), nr, code);

          // invalid rectangle
          TVcd.nmcd.rc.Top := nr.Top;
          if (nr.Right = 0) or (nr.Bottom = 0) then
            Exit;
        end;

        // invalid rectangle
        if (TVcd.nmcd.rc.Right = 0) or (TVcd.nmcd.rc.Bottom = 0) then
          Exit;

        if Assigned(tn) then
        begin
          if ShowRoot then
            TVcd.nmcd.rc.left := TVcd.nmcd.rc.left + FIndent*(tn.level + 1) - GetScrollPos(Handle,SB_HORZ)
          else
            TVcd.nmcd.rc.left := TVcd.nmcd.rc.left + FIndent*(tn.level) - GetScrollPos(Handle,SB_HORZ);
        end;

        Canvas := TCanvas.Create;
        Canvas.Handle := TVcd.nmcd.hdc;
        Canvas.Font.Assign(Font);

        clr := Color;
        bclr := Color;

        DefaultDraw := True;
        DrawState := [];
        if  (TVcd.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED) then
          DrawState := [cdsSelected];

        if  (TVcd.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED) then
          DrawState := DrawState + [cdsFocused];

        if Assigned(tn) and Assigned(OnCustomDrawItem) then
        begin
          Self.Canvas.Brush.Color := Color;
          Self.Canvas.Pen.Color := Color;
          OnCustomDrawItem(Self,tn,DrawState,DefaultDraw);
          clr := Self.Canvas.Pen.Color;
          bclr := Self.Canvas.Brush.Color;
        end;

        if DefaultDraw and (FUpdateCount = 0) and Assigned(tn) then
        begin
          if WordWrap then
            TVcd.nmcd.rc.Right := Width;

          HTMLDrawEx(Canvas,tn.Text,TVcd.nmcd.rc,TImageList(FImages),0,0,-1,-1,1,False,True,False,False,False,False,WordWrap,
            1.0,clBlue,clNone,clNone,clGray,a,s,fa,XSize,YSize,hl,ml,hr,FImageCache,nil,0);

          hasChk := (tn.ImageIndex in [1,2,3,4]);

          r := TVcd.nmcd.rc;
          if (YSize < r.Bottom - r.Top) then
            r.Top := r.Top+((r.Bottom - r.Top - ysize) shr 1);

          if tn.ImageIndex > 0 then
            imgs := 16
          else
            imgs := 0;

          Canvas.Pen.Color := clr;
          Canvas.Brush.Color := bclr;

          if  (TVcd.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED) then
          begin
            hr := TVcd.nmcd.rc;

            hr.Right := hr.Left + XSize + imgs + 4;

            if Assigned(StateImages) and (tn.StateIndex >= 0) and (tn.StateIndex < StateImages.Count) then
            begin
              with hr do
                Canvas.Rectangle(Left,Top,Right,Bottom);
            end;

            if GetFocus = Handle then
            begin
              Canvas.Font.Color := FSelectionFontColor;
            end
            else
            begin
              Canvas.Brush.Color := FSelectionNFColor;
              Canvas.Pen.Color := FSelectionNFColor;
              Canvas.Font.Color := FSelectionNFFontColor;
            end;

            if Assigned(StateImages) and (tn.StateIndex >= 0) and (tn.StateIndex < StateImages.Count) then
            begin
              hr.Right := hr.Right + StateImages.Width + 4;
              if hasChk then
                hr.Left := hr.Left + StateImages.Width + 4
              else
                hr.Left := hr.Left + StateImages.Width + 2;
            end;

            if (GetFocus = Handle) or HideSelection then
              SelectionColors.Draw(Canvas, hr)
            else
            begin
              Canvas.Rectangle(hr);
            end;

            if  (TVcd.nmcd.uItemState and CDIS_FOCUS = CDIS_FOCUS) then
            begin
              Canvas.Pen.Color := clWhite;
              Canvas.Brush.Color := clWhite;
              Canvas.DrawFocusRect(hr);
            end;

            hr.Left := hr.Right;
            hr.Right := TVcd.nmcd.rc.Right;

            Canvas.Pen.Color := clr;
            Canvas.Brush.Color := bclr;

            with hr do
              Canvas.Rectangle(Left,Top,Right,Bottom);

            TVcd.nmcd.rc := r;
            TVcd.nmcd.rc.Left := TVcd.nmcd.rc.Left + XSize + 4;
          end
          else
          begin
            Canvas.Pen.Color := clr;
            Canvas.Brush.Color := bclr;

            with TVcd.nmcd.rc do
              Canvas.Rectangle(Left,Top,Right,Bottom);
          end;

          TVcd.nmcd.rc := r;
          TVcd.nmcd.rc.Left := TVcd.nmcd.rc.Left + 2;

          urlcol := FURLColor;

          if  (TVcd.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED) then
          begin
            if GetFocus = Handle then
            begin
              urlcol := FSelectionFontColor;
            end
            else
            begin
              Canvas.Brush.Color := FSelectionNFColor;
              Canvas.Pen.Color := FSelectionNFColor;
              urlcol := FSelectionNFFontColor;
            end;
          end;

          r := TVcd.nmcd.rc;

          if (tn.ImageIndex in [1,2]) then
          begin
            cr := Rect(r.Left,r.Top,r.Left + 14,r.Top + 14);
            DrawCheck(Canvas,cr,tn.ImageIndex = 2);
            r.Left := r.Left + 16;
          end;

          if (tn.ImageIndex in [3,4]) then
          begin
            cr := Rect(r.Left,r.Top,r.Left + 14,r.Top + 14);
            DrawRadio(Canvas,cr,tn.ImageIndex = 4);
            r.Left := r.Left + 16;
          end;

          if Assigned(StateImages) and (tn.StateIndex < StateImages.Count) and (tn.StateIndex >= 0) then
          begin
            if hasChk then
            begin
              StateImages.Draw(Canvas,r.Left + 2, r.Top  - 2, tn.StateIndex);
              r.Left := r.Left + StateImages.Width + 6;
            end
            else
            begin
              StateImages.Draw(Canvas,r.Left + 0, r.Top  - 2, tn.StateIndex);
              r.Left := r.Left + StateImages.Width + 6;
            end;
          end;

          r.Right := Width;
          HTMLDrawEx(Canvas,tn.Text,r,TImageList(FImages),0,0,-1,-1,1,False,False,False,
            ((TVcd.nmcd.uItemState and CDIS_SELECTED) = CDIS_SELECTED),False,False,WordWrap,1.0,URLCol,clNone,clNone,clGray,a,s,fa,xsize,ysize,hl,ml,hr,FImageCache,nil,0);
        end;

        Canvas.Free;
        message.Result := 0;
      end;
      else
        message.Result := 0;
    end;
  end
  else
    inherited;
end;


procedure THTMLTreeview.WMRButtonDown(var message: TWMRButtonDown);
var
  Node: TTreeNode;
begin

  if RightClickSelect then
  begin
    Node := GetNodeAt(message.XPos,message.YPos);
    if Assigned(Node) then
      Node.Selected := True;
  end;
  inherited;
end;

procedure THTMLTreeview.WMLButtonUp(var message: TWMLButtonDown);
begin
  inherited;
end;

procedure THTMLTreeview.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
//  Invalidate;
end;

procedure THTMLTreeview.WMLButtonDown(var message: TWMLButtonDown);
var
  Node,PN,SN : TTreeNode;
  r,hr: TRect;
  s,a,fa,hs:string;
  xsize,ysize,ml,hl,i: Integer;
  chk: boolean;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  Node := GetNodeAt(message.XPos,message.YPos);

  if Assigned(Node) then
  begin
    r := Node.Displayrect(True);

    if Assigned(StateImages) and (Node.StateIndex >= 0) then
      r.Left := r.Left - StateImages.Width - 20;

    if (Node.ImageIndex in [1,2]) and not FReadOnlyEx and
       (Message.XPos < R.Left + 16) and (Message.XPos > R.Left) then
    begin
      if Node.ImageIndex = 1 then
        Node.ImageIndex := 2
      else
        Node.ImageIndex := 1;

      chk := Node.ImageIndex = 2;

      if Assigned(FOnCheckBoxClick) then
        FOnCheckBoxClick(Self,Node,chk);

      //
      sn := Node.getFirstChild;
      while Assigned(sn) and AutoChildCheck do
      begin
        SetNodeCheck(sn,chk);
        sn :=sn.getNextSibling;
      end;

      Message.Result := 1;
      Exit;
    end;

    if (Node.ImageIndex = 3) and Assigned(Node.Parent) and not FReadOnlyEx and
       (Message.XPos < R.Left + 16) and (Message.XPos > R.Left) then
    begin
      PN := Node.Parent;

      for i := 1 to PN.Count do
      begin
        if (PN.Item[i - 1].Level = Node.Level) then
        begin
          if (PN.Item[i - 1].ImageIndex = 4) then
            PN.Item[i - 1].ImageIndex := 3;
        end;
      end;

      Node.ImageIndex := 4;

      if Assigned(FOnRadioClick) then
        FOnRadioClick(Self,Node);
    end;

    if (Node.ImageIndex >= 0) then
      r.Left := r.Left + 16
    else
    if ((Node.StateIndex = 0) and Assigned(StateImages)) then
    begin
      r.Left := r.Left + StateImages.Width;
      r.Right := Width;
    end;

    Canvas.Font.Assign(Font);

    hs := Node.Text + ' ';

    HTMLDrawEx(Canvas,hs,r,TImageList(FImages),0,0,0,0,1,True,True,False,
      False,False,False,False,1.0,clBlue,clNone,clNone,clGray,a,s,fa,xsize,ysize,hl,ml,hr,FImageCache,nil,0);

    if (ysize < r.Bottom - r.Top) then
      r.Top := r.Top + ((r.Bottom - r.Top - ysize) shr 1);

    a := '';

    HTMLDrawEx(Canvas,hs,r,TImageList(FImages),message.XPos,message.Ypos,0,0,1,True,True,False,
      False,False,False,False,1.0,clBlue,clNone,clNone,clGray,a,s,fa,xsize,ysize,hl,ml,hr,FImageCache,nil,0);

    if (a <> '') then
    begin
      if Assigned(FAnchorClick) then
        FAnchorClick(self,Node,a);
    end;
  end;

  if AutoExpand then
    Invalidate;
end;

procedure THTMLTreeView.WMHScroll(var message:TMessage);
begin
  inherited;
  if FOldScrollPos <> GetScrollPos(handle,SB_HORZ) then
  begin
    Invalidate;
    FOldScrollPos := GetScrollPos(handle,SB_HORZ);
  end;
end;


procedure THTMLTreeview.WMMouseMove(var message: TWMMouseMove);
var
  Node : TTreeNode;
  r,hr: TRect;
  s,a,fa,hs: string;
  xsize,ysize,ml,hl: Integer;
begin
  Node := GetNodeAt(message.XPos,message.YPos);

  if Assigned(Node) then
  begin
    hs := Node.Text + ' ';

    r := Node.DisplayRect(true);
    a := '';

    if HotTrack then
      Selected := Node;

    Canvas.Font.Assign(Font);

    if (Node.ImageIndex >= 0) then
      r.Left := r.Left + 16
    else
    if ((Node.StateIndex = 0) and Assigned(StateImages)) then
    begin
      r.Left := r.Left + StateImages.Width;
      r.Right := Width;
    end;

    HTMLDrawEx(Canvas,hs,r,TImageList(FImages),Message.Xpos,Message.Ypos,-1,-1,1,True,True,False,
      False,False,False,False,1.0,clBlue,clNone,clNone,clGray,a,s,fa,xsize,ysize,hl,ml,hr,FImageCache,nil,0);

    a := '';

    if (ysize < r.Bottom - r.Top) then
      r.top := r.top+((r.bottom-r.top-ysize) shr 1);

    HTMLDrawEx(Canvas,hs,r,TImageList(FImages),Message.Xpos,Message.Ypos,-1,-1,1,True,True,False,
      False,False,False,False,1.0,clBlue,clNone,clNone,clGray,a,s,fa,xsize,ysize,hl,ml,hr,FImageCache,nil,0);

    if (a <> '') then // new anchor
    begin
      if (self.Cursor <> crHandPoint) then
      begin
        FOldCursor := self.Cursor;
        Self.Cursor := crHandPoint;
      end;

      if (a <> FOldAnchor) then
      begin
        if Assigned(FAnchorExit) then
           FAnchorExit(Self,Node,FOldAnchor);

        FOldAnchor := a;

        if Assigned(FAnchorEnter) then
          FAnchorEnter(Self,Node,a);
      end;    
    end
    else
    begin
      if (self.Cursor = crHandPoint) then
      begin
        Self.Cursor := FOldCursor;
        if Assigned(FAnchorExit) and (FOldAnchor <> '') then
           FAnchorExit(Self,Node,FOldAnchor);
        FOldAnchor := '';   
      end;
    end;
  end
  else
    if (self.Cursor = crHandPoint) then
    begin
      self.Cursor := FOldCursor;
      if Assigned(FAnchorExit) and (FOldAnchor <> '') then
        FAnchorExit(Self,Node,FOldAnchor);
      FOldAnchor := '';
    end;

  inherited;
end;

constructor THTMLTreeview.Create(AOwner: TComponent);
var
  VerInfo: TOSVersioninfo;

begin
  inherited Create(AOwner);
  Tooltips := False;
  FURLColor := clBlue;
  FSelectionFontColor := clHighLightText;
  inherited ReadOnly := true;
  FReadOnlyEx := false;

  FSelectionNFColor := clSilver;
  FSelectionNFFontColor := clBlack;

  FItemHeight := 16;
  FOldScrollPos := -1;
  FControlColor := clRed;
  FGlyphChecked := TBitmap.Create;
  FGlyphUnChecked := TBitmap.Create;
  FGlyphRadio := TBitmap.Create;
  FGlyphRadioSelected := TBitmap.Create;
  FOldAnchor := '';

  FHintColor := ClNone;

  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);

  GetVersionEx(verinfo);

  FIsWinXP := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));

  if FIsWinXP then
    FIsWinXP := FIsWinXP and IsThemeActive;

  FImageCache := THTMLPictureCache.Create;
  FSelectionColors := TGradientStyle.Create;
  DoubleBuffered := true;
end;


function THTMLTreeview.GetItemHeight: Integer;
begin
  Result := TreeView_GetItemHeight(self.Handle);
end;

procedure THTMLTreeview.SetItemHeight(const Value: integer);
begin
  if (Value <> FItemHeight) then
  begin
    FItemHeight := value;
    TreeView_SetItemHeight(self.Handle,FItemHeight);
  end;
end;

procedure THTMLTreeview.SetSelectionColors(const Value: TGradientStyle);
begin
  FSelectionColors.Assign(Value);
  Invalidate;  
end;

procedure THTMLTreeview.SetSelectionFontColor(const Value: tcolor);
begin
  if (FSelectionFontColor <> Value) then
  begin
    FSelectionFontColor := Value;
    Invalidate;
  end;
end;

procedure THTMLTreeview.SetURLColor(const Value: tcolor);
begin
  if (FURLColor <> Value) then
  begin
    FURLColor := Value;
    Invalidate;
  end;
end;

procedure THTMLTreeview.SetHintColor(const Value: tcolor);
begin
  FHintColor := Value;
end;

procedure THTMLTreeview.Loaded;
begin
  inherited;
  Images := StateImages;
  FOldCursor := self.Cursor;

  if not (csDesigning in ComponentState) then
  begin
    if not FIsWinXP and (ControlStyle = csTheme) then
      ControlStyle := csWinXP;
  end;
end;

procedure THTMLTreeview.CreateWnd;
var
  oldheight: integer;
begin
  inherited;
  oldheight := FItemHeight;
  FItemHeight := 0; // force an update of ItemHeight always
  ItemHeight := oldheight;
end;

procedure THTMLTreeview.SetImages(const Value: TImageList);
begin
  FImages := Value;
  Invalidate;
end;

function THTMLTreeview.NodeText(Node: TTreeNode): string;
begin
  Result := HTMLStrip(Node.Text);
end;

procedure THTMLTreeView.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;
  inherited;
end;

procedure THTMLTreeview.WMSize(var message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure THTMLTreeview.KeyPress(var Key: Char);
var
  PN: TTreeNode;
  i: Integer;
begin
  if IsEditing then
    Exit;

  if Assigned(Selected) and (Key = #32) then
  begin
    Key := #0;
    if Selected.ImageIndex in [1,2] then
    begin
      if Selected.ImageIndex = 1 then
        Selected.ImageIndex := 2
      else
        Selected.ImageIndex := 1;


      if Assigned(FOnCheckBoxClick) then
        FOnCheckBoxClick(Self,Selected,Selected.ImageIndex = 2);
    end;
    if Selected.ImageIndex = 3 then
    begin
      if Assigned(Selected.Parent) then
      begin
        PN := Selected.Parent;

        for i := 1 to PN.Count do
        begin
          if (PN.Item[i - 1].Level = Selected.Level) then
          begin
            if (PN.Item[i - 1].ImageIndex = 4) then
              PN.Item[i - 1].ImageIndex := 3;

          end;
        end;
        Selected.ImageIndex := 4;


        if Assigned(FOnRadioClick) then
          FOnRadioClick(Self,Selected);
      end;
    end;
    Exit;
  end;
  inherited;
end;

function THTMLTreeview.GetNodeCheck(Node: TTreeNode;
  var Check: Boolean): Boolean;
begin
  Result := False;
  if Node.ImageIndex in [1,2] then
  begin
    Check := Node.ImageIndex = 2;
    Result := True;
  end
end;

procedure THTMLTreeview.SetNodeCheck(Node: TTreeNode; Check: Boolean);
begin
  if Check then
    Node.ImageIndex := 2
  else
    Node.ImageIndex := 1;

end;

procedure THTMLTreeview.RemoveNodeCheck(Node: TTreeNode);
begin
  Node.ImageIndex := 0;

end;

procedure THTMLTreeview.SetControlStyle(const Value: TControlStyle);
begin
  FControlStyle := Value;
  Invalidate;
end;

procedure THTMLTreeview.SetComponentStyle(AStyle: TTMSStyle);
begin
  SelectionColors.SetStyle(AStyle);
  SelectionFontColor := clHighlightText;
  if not (AStyle in [tsCustom, tsWindowsXP]) then
    SelectionFontColor := clBlack;
end;

procedure THTMLTreeview.SetControlColor(const Value: TColor);
begin
  FControlColor := Value;
  Invalidate;
end;

procedure THTMLTreeview.SetGlyphChecked(const Value: TBitmap);
begin
  FGlyphChecked.Assign(Value);
end;

procedure THTMLTreeview.SetGlyphRadio(const Value: TBitmap);
begin
  FGlyphRadio.Assign(Value);
end;

procedure THTMLTreeview.SetGlyphRadioSelected(const Value: TBitmap);
begin
  FGlyphRadioSelected.Assign(Value);
end;

procedure THTMLTreeview.SetGlyphUnChecked(const Value: TBitmap);
begin
  FGlyphUnChecked.Assign(Value);
end;

destructor THTMLTreeview.Destroy;
begin
  FImageCache.Free;
  FGlyphChecked.Free;
  FGlyphUnChecked.Free;
  FGlyphRadio.Free;
  FGlyphRadioSelected.Free;
  FSelectionColors.Free;
  inherited;
end;

function THTMLTreeview.GetRadioButton(Node: TTreeNode;
  var Check: Boolean): Boolean;
begin
  Result := False;
  if Node.ImageIndex in [3,4] then
  begin
    Check := Node.ImageIndex = 4;
    Result := True;
  end

end;

function THTMLTreeview.GetReadOnlyEx: boolean;
begin
  Result := FReadOnlyEx;
end;

procedure THTMLTreeview.RemoveRadioButton(Node: TTreeNode);
begin
  Node.ImageIndex := 0;
end;

procedure THTMLTreeview.SetRadioButton(Node: TTreeNode; Check: Boolean);
begin
  if Check then
    Node.ImageIndex := 4
  else
    Node.ImageIndex := 3;

end;

procedure THTMLTreeview.SetReadOnlyEx(const Value: boolean);
begin
  FReadOnlyEx := Value;
  inherited ReadOnly := true;
end;

procedure THTMLTreeview.SetSelectionNFColor(const Value: TColor);
begin
  FSelectionNFColor := Value;
  Invalidate;
end;

procedure THTMLTreeview.SetSelectionNFFontColor(const Value: TColor);
begin
  FSelectionNFFontColor := Value;
  Invalidate;
end;

procedure THTMLTreeview.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure THTMLTreeview.EndUpdate;
begin
  if (FUpdateCount > 0) then
  begin
    dec(FUpdateCount);
    if FUpdateCount = 0 then
      Invalidate;
  end;
end;


procedure THTMLTreeview.Expand(Node: TTreeNode);
begin
  inherited;

  if HotTrack then
    Invalidate;
end;

function THTMLTreeview.GetStringExtent(s: string): integer;
var
  dwExtent: DWORD;
  hDCListBox: HDC;
  hFontOld, hFontNew: HFONT;
  tm: TTextMetric;
  Size: TSize;
begin
  hDCListBox := GetDC(Handle);
  hFontNew := SendMessage(Handle, WM_GETFONT, 0, 0);
  hFontOld := SelectObject(hDCListBox, hFontNew);
  GetTextMetrics(hDCListBox, tm);

  GetTextExtentPoint32(hDCListBox, PChar(s), Length(s), Size);

  dwExtent := Size.cx + tm.tmAveCharWidth;

  SelectObject(hDCListBox, hFontOld);
  ReleaseDC(Handle, hDCListBox);

  GetStringExtent := LOWORD(dwExtent);
end;

procedure THTMLTreeview.ShowH(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
var
  hintpos: tpoint;
  idx: integer;
  Node: TTreeNode;
  itemPos : integer;
  widthText : integer;
  cp: TPoint;
begin
  if (Hintinfo.Hintcontrol = self) then
  begin
    if (FHintColor <> clNone) then
      Hintinfo.Hintcolor := FHintColor;

    {search over which thing mouse is}
    with (hintinfo.HintControl as tcustomtreeview) do
    begin
      idx := sendmessage(self.handle, lb_itemfrompoint, 0, makelparam(hintinfo.cursorpos.x, hintinfo.cursorpos.y));

      if (idx < items.count) then
      begin
        //FLastHintPos.Y := idx - SendMessage(Handle,LB_GETTOPINDEX,0,0);
        //ShowMessage('LastHintPos :' + inttostr(FLastHintPos.Y) + ' idx :' + inttostr(idx));
        //FLastHintPos.x := 0;

        //Node := GetNodeAt(self.calcCursorPos.X, Self.calcCursorPos.Y);
     		GetCursorPos(cp);
        cp := ScreenToClient(cp);
        Node := GetNodeAt(cp.X,cp.Y);

        FLastHintNode := Node;

        if Assigned(Node) then
        begin
        widthText := GetStringExtent(Node.Text);

        if (ShowRoot) then
          widthText := widthText + FIndent * (Node.Level + 1)
        else
          widthText := widthText + FIndent * (Node.Level);

        if widthText > self.width then
        begin
          if (HTMLHint) then
            hintstr := Node.Text
          else
            hintStr := htmlStrip(Node.Text);

          if ShowRoot then
            hintPos.X := FIndent *(Node.Level + 1) - GetScrollPos(Handle,SB_HORZ)
          else
            hintPos.X := FIndent *(Node.level) - GetScrollPos(Handle,SB_HORZ);

          //itemPos := Self.CalcCursorPos.Y div itemHeight;
          itemPos := cp.Y div itemHeight;
          hintPos.Y := itemHeight * itemPos;
          hintinfo.hintpos := self.ClientToScreen(hintpos);
        end
        else
          hintstr := Self.Hint;
        end
        else
          hintstr := self.Hint;
      end
      else
        hintstr := self.Hint;
    end;
  end;
end;

procedure THTMLTreeview.CMHintShow(var Msg: TMessage);
var
  CanShow: Boolean;
  hi: PHintInfo;

begin
  CanShow := True;
  hi := PHintInfo(Msg.LParam);
  ShowH(hi.HintStr, CanShow, hi^);

  Msg.Result := Ord(not CanShow);
end;


procedure THTMLTreeview.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  node : TTreeNode;
begin
  (*
  if (FLastHintPos.Y >= 0) then
  begin
    idx := y div itemheight;
    if (idx <> FLastHintPos.y) then
    begin
      Application.CancelHint;
      FLastHintPos := Point(-1, -1);
    end;
  end;
  inherited MouseMove(Shift, X, Y);
  *)
  if (FLastHintNode <> nil) then
  begin
    node := GetNodeAt(x,y);
    if (Node <> FLastHintNode) then
    begin
      Application.CancelHint;
      FLastHintNode := nil;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

function THTMLTreeview.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function THTMLTreeview.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure THTMLTreeview.SetVersion(const Value: string);
begin

end;


procedure THTMLTreeview.SetWordWrap(const Value: boolean);
begin
  FWordWrap := Value;
  Invalidate;
end;

{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}
  

end.

