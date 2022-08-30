{**************************************************************************}
{ TParamTreeview component                                                 }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ Copyright © 2001 - 2014                                                  }
{  TMS Software                                                            }
{  Email : info@tmssoftware.com                                            }
{  Web : http://www.tmssoftware.com                                        }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit paramtreeview;

{$I TMSDEFS.INC}
{$DEFINE PARAMS}

interface

uses
  Windows, Classes, Comctrls, Messages, Controls, Graphics, Menus, StdCtrls,
  Spin, Forms, ParHTML, PictureContainer, Dialogs, Types;


const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 3; // Release nr.
  BLD_VER = 10; // Build nr.

  // version history
  // 1.3.0.1 : improved mask editor property handling
  // 1.3.0.2 : fix for parameter handling on multiline nodes
  // 1.3.0.3 : fix for context menu selection in inplace editors
  //         : fix for & character editing in EDIT types
  // 1.3.0.4 : fix for issue with WordWrap = false
  // 1.3.0.5 : fix issue with inplace editors wider than control width
  // 1.3.1.0 : improved positioning of directory select dialog on multimonitor machines
  // 1.3.1.1 : Fixed : issue with hotkey handling for other controls during param edit
  // 1.3.3.0 : Fixed issue with spinedit
  // 1.3.3.1 : Fixed painting issue with mousewheel scrolling
  // 1.3.3.2 : Fixed runtime creation issue
  // 1.3.3.3 : Improved : repainting of hovered items
  // 1.3.3.4 : Fixed : Issue with use in combination with TAdvMainMenu
  // 1.3.3.5 : Improved : Date format compatibility for inplace date picker
  // 1.3.3.6 : Fixed : Issue with mouse anchor sensitivity
  // 1.3.3.7 : Fixed : Item background painting on XE3 with theming enabled
  // 1.3.3.8 : Improved : Editing of very long parameters
  // 1.3.3.9 : Fixed : Issue with mask editing
  // 1.3.3.10: Fixed : Issue with OnParamEditDone for DIR editor type

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}
  
  TParamTreeViewClickEvent = procedure(Sender:TObject;ANode: TTreeNode; href: string;var value: string) of object;
  TParamTreeViewPopupEvent = procedure(Sender:TObject;ANode: TTreeNode; href: string;values:TStringlist;var DoPopup: Boolean) of object;
  TParamTreeViewSelectEvent = procedure(Sender:TObject;ANode: TTreeNode; href,value: string) of object;
  TParamTreeViewChangedEvent = procedure(Sender:TObject;ANode: TTreeNode; href,oldvalue, newvalue: string) of object;
  TParamTreeViewHintEvent = procedure(Sender:TObject; ANode: TTreeNode; href: string; var hintvalue: string; var showhint: Boolean) of object;

  TParamCustomEditEvent = procedure(Sender: TObject; Node: TTreeNode; href, value, props: string; EditRect: TRect) of object;

  TParamTreeviewCustomShowEvent = procedure(Sender:TObject; ANode: TTreeNode; href:string; var value:string;ARect:TRect) of object;

  TParamTreeViewEditEvent = procedure(Sender:TObject;ANode: TTreeNode; href: string;var value: string) of object;

  TParamTreeview = class;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TParamTreeview = class(TCustomTreeView)
  private
    FVersion : String;
    { Private declarations }
    FIndent: Integer;
    FOldCursor: Integer;
    FOldScrollPos: Integer;
    FParamColor: TColor;
    FSelectionColor: TColor;
    FSelectionFontColor: TColor;
    FItemHeight: Integer;
    FImages: TImageList;
    FParamPopup: TPopupMenu;
    FParamList: TPopupListBox;
    FParamDatePicker: TPopupDatePicker;
    FParamSpinEdit: TPopupSpinEdit;
    FParamEdit: TPopupEdit;
    FParamMaskEdit: TPopupMaskEdit;
    FOldParam: string;
    FOnParamChanged: TParamTreeViewChangedEvent;
    FOnParamClick: TParamTreeViewClickEvent;
    FOnParamHint: TParamTreeViewHintEvent;
    FOnParamPopup: TParamTreeViewPopupEvent;
    FOnParamList: TParamTreeViewPopupEvent;
    FOnParamSelect: TParamTreeViewSelectEvent;
    FOnParamEnter: TParamTreeViewSelectEvent;
    FOnParamExit: TParamTreeViewSelectEvent;
    FParamListSorted: Boolean;
    FShowSelection: Boolean;
    FOnParamPrepare: TParamTreeViewClickEvent;
    FParamHint: Boolean;
    FShadowColor: TColor;
    FShadowOffset: Integer;
    FUpdateCount: Integer;
    FWordWrap: Boolean;
    FMouseDown: Boolean;
    FContainer: TPictureContainer;
    FCurrCtrlID: string;
    FCurrCtrlRect: TRect;
    FCurrCtrlDown: TRect;
    FHoverNode: TTreeNode;
    FHoverHyperLink: Integer;
    FCurrHoverRect: TRect;
    FImageCache: THTMLPictureCache;
    FHover: Boolean;
    FHoverColor: TColor;
    FHoverFontColor: TColor;
    FEditAutoSize: Boolean;
    FLineSpacing: Integer;
    FOnParamEditStart: TParamTreeViewEditEvent;
    FOnParamEditDone: TParamTreeViewEditEvent;
    FEmptyParam: string;
    FOldAnchor: string;
    FOldIndex: Integer;
    FFocusLink: Integer;
    FNumHyperLinks: Integer;
    FEditValue: string;
    FEditPos: TPoint;
    FIsEditing: Boolean;
    FOnParamQuery: TParamTreeViewEditEvent;
    FOnParamCustomEdit: TParamCustomEditEvent;
    FAdvanceOnReturn: Boolean;
    FStopMouseProcessing: Boolean;
    FShowFocusBorder: Boolean;
    procedure CMHintShow(Var Msg: TMessage); message CM_HINTSHOW;
    procedure CMDesignHitTest(var message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure CNNotify(var message: TWMNotify); message CN_NOTIFY;
    procedure WMLButtonDown(var message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var message: TWMLButtonDown); message WM_LBUTTONUP;
    procedure WMMouseMove(var message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMHScroll(var message: TMessage); message WM_HSCROLL;
    procedure WMSize(var message: TMessage); message WM_SIZE;
    function GetItemHeight: Integer;
    procedure SetItemHeight(const Value: Integer);
    procedure SetSelectionColor(const Value: TColor);
    procedure SetSelectionFontColor(const Value: TColor);
    procedure SetParamColor(const Value: TColor);
    procedure SetImages(const Value: TImageList);
    procedure SetShowSelection(const Value: Boolean);
    function GetNodeParameter(Node: TTreeNode; HRef: String): string;
    procedure SetNodeParameter(Node: TTreeNode; HRef: String; const Value: string);
    function IsParam(x,y:Integer;GetFocusRect: Boolean;var Node: TTreeNode; var hr,cr: TRect; var CID,CT,CV: string): string;
    function HTMLPrep(s:string):string;
    function InvHTMLPrep(s:string):string;
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: Integer);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetLineSpacing(const Value: Integer);
    function GetParamItemRefCount(Item: Integer): Integer;
    function GetParamNodeRefCount(Node: TTreeNode): Integer;
    function GetParamItemRefs(Item, Index: Integer): string;
    function GetParamRefCount: Integer;
    function GetParamRefs(Index: Integer): string;
    procedure StartParamEdit(param:string; Node: TTreeNode; hr: TRect);
    procedure StartParamDir(Node: TTreeNode; param,curdir:string; hr: TRect);
    function GetParamRect(href: string): TRect;
    function GetParamNodeIndex(Node: TTreeNode; href: string): Integer;
    function GetParamRefNode(href: string): TTreeNode;
    function GetParameter(href: string): string;
    procedure SetParameter(href: string; const Value: string);
    procedure SetHover(const Value: Boolean);
    function GetParamIndex(href: string): Integer;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    { Protected declarations }
    function GetVersionNr: Integer; virtual;
    procedure HandlePopup(Sender:TObject); virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;

    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure WndProc(var Message: TMessage); override;
    procedure UpdateParam(Param,Value:string);
    procedure PrepareParam(Param:string; var Value:string);
    procedure ControlUpdate(Sender: TObject; Param,Text:string);
    procedure AdvanceEdit(Sender: TObject);
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Change(Node: TTreeNode); override;
    procedure Expand(Node: TTreeNode); override;    
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    property NodeParameter[Node: TTreeNode; HRef: String]:string read GetNodeParameter write SetNodeParameter;
    property ParamRefCount: Integer read GetParamRefCount;
    property ParamRefs[Index: Integer]:string read GetParamRefs;
    property ParamRefNode[href: string]: TTreeNode read GetParamRefNode;
    property ParamNodeRefCount[Item: Integer]: Integer read GetParamItemRefCount;
    property ParamNodeRefs[Item,Index: Integer]:string read GetParamItemRefs;
    property ParamNodeIndex[Node: TTreeNode; href: string]: Integer read GetParamNodeIndex;
    property ParamIndex[href: string]: Integer read GetParamIndex;
    property Parameter[href: string]: string read GetParameter write SetParameter;
    procedure EditParam(href: string);
    function GetParamInfo(Node: TTreeNode; HRef:string; var AValue, AClass, AProp, AHint: string): Boolean;
    property DateTimePicker: TPopupDatePicker read FParamDatePicker;
    property SpinEdit: TPopupSpinEdit read FParamSpinEdit;
    property Editor: TPopupEdit read FParamEdit;
    property MaskEditor: TPopupMaskEdit read FParamMaskEdit;
    property ListBox: TPopupListBox read FParamList;
    property StopMouseProcessing: Boolean read FStopMouseProcessing write FStopMouseProcessing;
  published
//    property Version : string read FVersion write FVersion;
    { Published declarations }
    { new introduced properties }
    property AdvanceOnReturn: Boolean read FAdvanceOnReturn write FAdvanceOnReturn;
    property EditAutoSize: Boolean read FEditAutoSize write FEditAutoSize;
    property EmptyParam: string read FEmptyParam write FEmptyParam;
    property HTMLImages: TImageList read FImages write SetImages;
    property Hover: Boolean read FHover write SetHover default True;
    property HoverColor: TColor read FHoverColor write FHoverColor default clGreen;
    property HoverFontColor: TColor read FHoverFontColor write FHoverFontColor default clWhite;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing default 0;
    property SelectionColor: TColor read fSelectionColor write SetSelectionColor;
    property SelectionFontColor: TColor read fSelectionFontColor write SetSelectionFontColor;
    property ShowSelection: Boolean read FShowSelection write SetShowSelection default False;
    property ParamColor: TColor read FParamColor write SetParamColor default clGreen;
    property ParamHint: Boolean read FParamHint write FParamHint;
    property ShadowColor: TColor read FShadowColor write SetShadowColor;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset;
    property ShowFocusBorder: Boolean read FShowFocusBorder write FShowFocusBorder default true;
    property Version: string read GetVersion write SetVersion;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;

    { new introduced events }
    property OnParamPrepare: TParamTreeViewClickEvent read FOnParamPrepare write FOnParamPrepare;
    property OnParamClick: TParamTreeViewClickEvent read FOnParamClick write FOnParamClick;
    property OnParamPopup: TParamTreeViewPopupEvent read FOnParamPopup write FOnParamPopup;
    property OnParamList: TParamTreeViewPopupEvent read FOnParamList write FOnParamList;
    property OnParamSelect: TParamTreeViewSelectEvent read FOnParamSelect write FOnParamSelect;
    property OnParamChanged: TParamTreeViewChangedEvent read FOnParamChanged write FOnParamChanged;
    property OnParamHint: TParamTreeViewHintEvent read FOnParamHint write FOnParamHint;
    property OnParamEnter: TParamTreeViewSelectEvent read FOnParamEnter write FOnParamEnter;
    property OnParamExit: TParamTreeViewSelectEvent read FOnParamExit write FOnParamExit;
    property OnParamEditStart: TParamTreeViewEditEvent read FOnParamEditStart write FOnParamEditStart;
    property OnParamEditDone: TParamTreeViewEditEvent read FOnParamEditDone write FOnParamEditDone;
    property OnParamCustomEdit: TParamCustomEditEvent read FOnParamCustomEdit write FOnParamCustomEdit;
    property OnParamQuery: TParamTreeViewEditEvent read FOnParamQuery write FOnParamQuery;
    { reintroduced properties }
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
    property ParamListSorted: Boolean read FParamListSorted write FParamListSorted;
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
    property TabOrder;
    property TabStop default True;
    property ToolTips;
    property Visible;
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
  end;



implementation
uses
  CommCtrl, Shellapi, SysUtils, ShlObj, ActiveX, ImgList, Math
  {$IFDEF DELPHI6_LVL}
  , Variants
  {$ENDIF}
  ;


{$IFDEF VER100}
const
  NM_CUSTOMDRAW            = NM_FIRST-12;

  CDDS_PREPAINT           = $00000001;
  CDDS_POSTPAINT          = $00000002;
  CDDS_PREERASE           = $00000003;
  CDDS_POSTERASE          = $00000004;
  CDDS_ITEM               = $00010000;
  CDDS_ITEMPREPAINT       = CDDS_ITEM or CDDS_PREPAINT;
  CDDS_ITEMPOSTPAINT      = CDDS_ITEM or CDDS_POSTPAINT;
  CDDS_ITEMPREERASE       = CDDS_ITEM or CDDS_PREERASE;
  CDDS_ITEMPOSTERASE      = CDDS_ITEM or CDDS_POSTERASE;
  CDDS_SUBITEM            = $00020000;

  // itemState flags
  CDIS_SELECTED       = $0001;
  CDIS_GRAYED         = $0002;
  CDIS_DISABLED       = $0004;
  CDIS_CHECKED        = $0008;
  CDIS_FOCUS          = $0010;
  CDIS_DEFAULT        = $0020;
  CDIS_HOT            = $0040;
  CDIS_MARKED         = $0080;
  CDIS_INDETERMINATE  = $0100;

  CDRF_DODEFAULT          = $00000000;
  CDRF_NEWFONT            = $00000002;
  CDRF_SKIPDEFAULT        = $00000004;
  CDRF_NOTIFYPOSTPAINT    = $00000010;
  CDRF_NOTIFYITEMDRAW     = $00000020;
  CDRF_NOTIFYSUBITEMDRAW  = $00000020;  // flags are the same, we can distinguish by context
  CDRF_NOTIFYPOSTERASE    = $00000040;

  TVM_GETITEMHEIGHT         = TV_FIRST + 28;
  TVM_SETITEMHEIGHT         = TV_FIRST + 27;

type
  tagNMCUSTOMDRAWINFO = packed record
    hdr: TNMHdr;
    dwDrawStage: DWORD;
    hdc: HDC;
    rc: TRect;
    dwItemSpec: DWORD;  // this is control specific, but it's how to specify an item.  valid only with CDDS_ITEM bit set
    uItemState: UINT;
    lItemlParam: LPARAM;
  end;
  PNMCustomDraw = ^TNMCustomDraw;
  TNMCustomDraw = tagNMCUSTOMDRAWINFO;


  tagNMTVCUSTOMDRAW = packed record
    nmcd: TNMCustomDraw;
    clrText: COLORREF;
    clrTextBk: COLORREF;
    iLevel: Integer;
  end;
  PNMTVCustomDraw = ^TNMTVCustomDraw;
  TNMTVCustomDraw = tagNMTVCUSTOMDRAW;

function TreeView_SetItemHeight(hwnd: HWND; iHeight: Integer): Integer;
begin
  Result := SendMessage(hwnd, TVM_SETITEMHEIGHT, iHeight, 0);
end;

function TreeView_GetItemHeight(hwnd: HWND): Integer;
begin
  Result := SendMessage(hwnd, TVM_GETITEMHEIGHT, 0, 0);
end;
{$ENDIF}

procedure TParamTreeview.CMDesignHitTest(var message: TCMDesignHitTest);
begin
  if htOnItem in GetHitTestInfoAt(message.XPos,message.YPos) then
    message.Result := 1
  else
    message.Result := 0;
end;

procedure TParamTreeview.CNNotify(var message: TWMNotify);
var
  TVcd:TNMTVCustomDraw;
  TVdi:TTVDISPINFO;
  Canvas: TCanvas;
  a,s,f: string;
  xsize,ysize,ml,hl: Integer;
  tn: TTreenode;
  r,hr,cr: TRect;
  urlcol: TColor;
  Selected: Boolean;
  CID,CV,CT: string;
  pt: TPoint;
  FHyperLink,fl: Integer;
  FHC, FHFC: TColor;

begin
  if message.NMHdr^.code = TVN_GETDISPINFO then
  begin
    TVDi := PTVDispInfo(pointer(message.nmhdr))^;

    if (tvdi.item.mask and TVIF_TEXT = TVIF_TEXT) then
    begin
      tn := Items.GetNode(tvdi.item.hitem);
      s := HTMLStrip(tn.text);
      strplcopy(tvdi.item.pszText,s,255);
      tvdi.item.mask := tvdi.item.mask or TVIF_DI_SETITEM;
      message.Result := 0;
      Exit;
    end;
  end;

  if message.NMHdr^.code = NM_CUSTOMDRAW then
  begin
    FIndent := TreeView_GetIndent(Handle);
    TVcd := PNMTVCustomDraw(Pointer(message.NMHdr))^;
    case TVcd.nmcd.dwDrawStage of
    CDDS_PREPAINT:
      begin
        message.Result := CDRF_NOTIFYITEMDRAW or CDRF_NOTIFYPOSTPAINT;
      end;
    CDDS_ITEMPREPAINT:
      begin
        //if TVcd.nmcd.uItemState and CDIS_FOCUS = CDIS_FOCUS then
        //  TVcd.nmcd.uItemState := TVcd.nmcd.uItemState and not CDIS_FOCUS;
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
        Canvas := TCanvas.Create;
        Canvas.Handle := TVcd.nmcd.hdc;
        Canvas.Font.Assign(Self.Font);

        tn := Items.GetNode(HTReeItem(TVcd.nmcd.dwitemSpec));

        if ShowRoot then
          TVcd.nmcd.rc.left := TVcd.nmcd.rc.left + FIndent*(tn.level + 1) - GetScrollPos(Handle,SB_HORZ)
        else
          TVcd.nmcd.rc.left := TVcd.nmcd.rc.left + FIndent*(tn.level) - GetScrollPos(Handle,SB_HORZ);

        //Canvas.TextOut(TVcd.nmcd.rc.Left,TVcd.nmcd.rc.Top,tn.Text);

        Selected := (TVcd.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED);

        HTMLDrawEx(Canvas,tn.Text,TVcd.nmcd.rc,TImageList(HTMLImages),0,0,-1,-1,2,
          False,True,False,False,Selected and FShowSelection,FHover,Wordwrap,FMouseDown,False,1.0,clGreen,
          FHoverColor,FHoverFontColor,clGray,a,s,f,xsize,ysize,hl,ml,hr,cr,CID,CV,CT,
          FImageCache,FContainer,Handle,0);

        r := TVcd.nmcd.rc;

//        if  (TVcd.nmcd.uItemState and CDIS_FOCUS = CDIS_FOCUS) then
        begin
          Canvas.Pen.Color := Color;
          Canvas.Brush.Color := Color;
          Canvas.Rectangle(r.Left ,r.Top,r.Right ,r.Bottom);
        end;

        if (YSize < r.Bottom - r.Top) then
          r.Top := r.Top + ((r.Bottom - r.Top - YSize) shr 1);

        if Selected then
        begin
          if FShowSelection then
          begin
            Canvas.Brush.Color := FSelectionColor;
            Canvas.Pen.Color := FSelectionColor;
            Canvas.Font.Color := FSelectionFontColor;
          end;

          with TVcd.nmcd.rc do
          begin
            Canvas.Rectangle(Left,Top,self.Width - 4,Bottom);
            DrawFocusRect(Canvas.Handle,Rect(Left,Top,self.Width - 4,Bottom));
          end;

          if  (TVcd.nmcd.uItemState and CDIS_FOCUS = CDIS_FOCUS) then
          begin
            Canvas.Pen.Color := clBlack;
            Canvas.Brush.color := clBlack;
            //Canvas.DrawFocusRect(TVcd.nmcd.rc);
          end;
          TVcd.nmcd.rc := r;
          TVcd.nmcd.rc.Left := TVcd.nmcd.rc.Left + xsize + 4;
        end
        else
        begin
          Canvas.Brush.Color := self.Color;
          Canvas.Pen.Color := self.Color;
          with TVcd.nmcd.rc do
            Canvas.Rectangle(Left,Top,Width,Bottom);
        end;

        TVcd.nmcd.rc := r;
        TVcd.nmcd.rc.Left := TVcd.nmcd.rc.Left + 1;

        urlcol := FParamColor;

        if  (TVcd.nmcd.uItemState and CDIS_SELECTED = CDIS_SELECTED) and FShowSelection then
        begin
          Canvas.Brush.Color := FSelectionColor;
          Canvas.Pen.Color := FSelectionColor;
          Urlcol := FSelectionFontColor;
        end;

        GetCursorPos(pt);
        pt := ScreenToClient(pt);

        if (FHoverNode <> tn) then
          FHyperlink := -1
        else
          FHyperLink := FHoverHyperLink;

        if (TVcd.nmcd.uItemState and CDIS_FOCUS = CDIS_FOCUS) and ShowFocusBorder then
        begin
          fl := FFocusLink;
        end
        else
          fl := -1;

        if not FHover then
        begin
          FHC := clNone;
          FHFC := clNone;
        end
        else
        begin
          FHC := FHoverColor;
          FHFC := FHoverFontColor;
        end;

        HTMLDrawEx(Canvas,tn.Text,TVcd.nmcd.rc,TImageList(HTMLImages),pt.X,pt.Y,fl,FHyperLink,ShadowOffset,
          False,False,False,False,Selected and FShowSelection,FHover,WordWrap,FMouseDown,False,1.0,urlCol,
          FHC,FHFC,ShadowColor,a,s,f,xsize,ysize,hl,ml,hr,cr,CID,CV,CT,
          FImageCache,FContainer,Handle,0);

        Canvas.Free;

      end;
      else
        message.Result := 0;
    end;
  end
  else
    inherited;
end;


procedure TParamTreeview.WMLButtonUp(var message: TWMLButtonDown);
var
  hr,cr: TRect;
  CID,CT,CV,s: string;
  Node: TTreeNode;

begin
  inherited;

  FMouseDown := False;

  Node := GetNodeAt(message.XPos,message.YPos);
  if Assigned(Node) then
  begin
    IsParam(message.XPos,message.YPos,False,Node,hr,cr,CID,CT,CV);

    if CID <> '' then
    begin

      if CT = 'CHECK' then
      begin
        BeginUpdate;
        s := Node.Text;

        if Uppercase(CV) = 'TRUE' then
          SetControlValue(s,CID,'FALSE')
        else
          SetControlValue(s,CID,'TRUE');

        Node.Text := s;
        EndUpdate;
      end;


      if FCurrCtrlDown.Left <> -1 then
        InvalidateRect(Handle,@FCurrCtrlDown,true);

    end;
    FCurrCtrlDown := Rect(-1,-1,-1,-1);
  end;

end;


procedure TParamTreeview.WMLButtonDown(var message: TWMLButtonDown);
var
  Node: TTreeNode;
  a: string;
  hr,cr: TRect;
  CID, CV, CT: string;

begin
  Node := GetNodeAt(message.XPos,message.YPos);

  inherited;

  if FStopMouseProcessing then
  begin
    FStopMouseProcessing := False;
    Exit;
  end;

  if csDesigning in ComponentState then
    Exit;

  FMouseDown := True;

  if Assigned(Node) then
  begin
    Selected := Node;
    a := IsParam(Message.XPos,Message.Ypos,False,Node,hr,cr,CID,CV,CT);

      if CID <> '' then
      begin
        InvalidateRect(Handle,@cr,true);
        FCurrCtrlDown := cr;
      end
      else
        FCurrCtrlDown := Rect(-1,-1,-1,-1);

    if (a <> '') then
    begin
      hr.Left := hr.Left - GetScrollPos(Handle, SB_HORZ);
      StartParamEdit(a,Node,hr);
    end;
  end;

end;

procedure TParamTreeView.StartParamEdit(param:string;Node: TTreeNode; hr: TRect);
var
  a, oldvalue,newvalue,v,c,p,h: string;
  NewValues: TStringList;
  DoPopup,DoList: Boolean;
  pt: TPoint;
  i,lh,rh: Integer;
  NewMenu: TMenuItem;
  wndstyle: DWORD;
  cw: integer;
  ptleft: TPoint;
  ptright: TPoint;
begin
  a := param;

  GetParamInfo(Node,a,v,c,p,h);

  // FFocusItem := Index;
  FFocusLink := ParamNodeIndex[Node,param];

  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('start editing link on '+inttostr(ffocuslink)));
  {$ENDIF}

//  if not ShowRoot then
//    hr.Left := hr.Left - FIndent;

  FIndent := TreeView_GetIndent(Handle);

//  hr.Right := hr.Right - FIndent * Node.Level;

  pt := ClientToScreen(Point(hr.left,hr.top));

  ptLeft := ClientToScreen(Point(0,0));

  cw := Width;

  wndStyle := GetWindowLong(self.Handle, GWL_STYLE);
  if wndStyle and WS_VSCROLL = WS_VSCROLL then
    cw := cw - GetSystemMetrics(SM_CXVSCROLL);

  ptright := ClientToScreen(Point(cw,0));

  pt.X := Max(ptLeft.X - 2,pt.X + FIndent * (Node.Level + 1));

  //lh := Max(0, hr.Left +  FIndent * (Node.Level + 1));
  rh := hr.Right + FIndent * (Node.Level + 1);
  lh := hr.Left;

  //max width allowed
  cw := ptright.X - pt.X - 4;

  if Assigned(FOnParamClick) then
  begin
    FIsEditing := True;
    PrepareParam(Param,v);
    oldvalue := v;
    FOnParamClick(Self,Node,param,v);
    if (v <> oldvalue) then
      ControlUpdate(self,Param,v);
    FIsEditing := False;
  end;

  if (c = 'TOGGLE') then
  begin
    NewValues := TStringList.Create;
    PropToList(InvHTMLPrep(p),NewValues);

    if NewValues.Count > 1 then
    begin
      if v = NewValues[0] then
        v := NewValues[1]
      else
        v := NewValues[0];
      ControlUpdate(self,Param,v);

      FCurrHoverRect := Rect(0,0,0,0);
      FHoverNode := nil;
      FHoverHyperLink := -1;
      Repaint;
    end;
    NewValues.Free;
  end;

  if (c = 'MENU') then
  begin
    FIsEditing := True;
    GetHRefValue(Node.Text,a,OldValue);
    newvalue := oldvalue;
    NewValues := TStringList.Create;
    NewValues.Sorted := FParamListSorted;
    DoPopup := True;

    PropToList(InvHTMLPrep(p),NewValues);

    if Assigned(FOnParamPopup) then
      FOnParamPopup(self,Node,a,NewValues,DoPopup);

    if DoPopup then
    begin
      while FParamPopup.Items.Count>0 do
        FParamPopup.Items[0].Free;

      FParamPopup.AutoHotkeys := maManual;

      for i := 1 to NewValues.Count do
      begin
        NewMenu := TMenuItem.Create(Self);
        NewMenu.Caption := NewValues.Strings[i - 1];
        NewMenu.OnClick := HandlePopup;
        FParamPopup.Items.Add(newmenu);
      end;

      FOldParam := a;

      PrepareParam(a,v);

      FParamPopup.Popup(pt.x,pt.y+2);
    end;
    NewValues.Free;
    FIsEditing := False;
  end;

  if c = 'DATE' then
  begin
    FIsEditing := True;

    FParamDatePicker.Top := pt.Y - 2;
    FParamDatePicker.Left := pt.X + 2;

    if Max(16,rh - lh) + lh > cw then
      FParamDatePicker.Width := Min(cw, rh - lh - 4)
    else
      FParamDatePicker.Width := Max(64,hr.Right - hr.Left);

    FParamDatePicker.Kind := dtkDate;
    FParamDatePicker.Cancelled := False;
    FParamDatePicker.Parent := Self;
    FParamDatePicker.Param := a;
    FParamDatePicker.Visible := True;
    FParamDatePicker.OnUpdate := ControlUpdate;
    FParamDatePicker.OnReturn := AdvanceEdit;

    PrepareParam(a,v);

    try
      {$IFDEF DELPHI6_LVL}
      FParamDatePicker.Date := VarToDateTime(v);
      {$ENDIF}
      {$IFNDEF DELPHI6_LVL}
      FParamDatePicker.Date := StrToDate(v);
      {$ENDIF}
    except
    end;
    FParamDatePicker.SetFocus;
  end;

  if c = 'TIME' then
  begin
    FIsEditing := True;

    FParamDatePicker.Top := pt.Y - 2;
    FParamDatePicker.Left := pt.X + 2;

    if Max(16,rh - lh) + lh > cw then
      FParamDatePicker.Width := Min(cw, rh - lh - 4)
    else
      FParamDatePicker.Width := Max(64,hr.Right - hr.Left);

    FParamDatePicker.ReInit;

    FParamDatePicker.Cancelled := False;
    FParamDatePicker.Parent := Self;
    FParamDatePicker.OnUpdate := ControlUpdate;
    FParamDatePicker.OnReturn := AdvanceEdit;
    FParamDatePicker.Kind := dtkTime;
    FParamDatePicker.Param := a;
    FParamDatePicker.Visible := True;

    PrepareParam(a,v);

    try
      FParamDatePicker.DateTime := EncodeDate(2005,1,1) + StrToTime(v);
    except
    end;
    FParamDatePicker.SetFocus;
  end;

  if c = 'SPIN' then
  begin
    FIsEditing := True;

    FParamSpinEdit.Top := pt.Y - 2;
    FParamSpinEdit.Left := pt.X + 2;

    if Max(16,rh - lh) + lh > cw then
      FParamSpinEdit.Width := Min(cw, rh - lh - 4)
    else
      FParamSpinEdit.Width := Max(16,hr.Right - hr.Left) + 16;

    FParamSpinEdit.Cancelled := False;
    FParamSpinEdit.Parent := Self;
    FParamSpinEdit.OnUpdate := ControlUpdate;
    FParamSpinEdit.OnReturn := AdvanceEdit;
    FParamSpinEdit.Param := a;
    FParamSpinEdit.Visible := True;

    PrepareParam(a,v);

    try
      FParamSpinEdit.Value := StrToInt(Trim(v));
    except
      FParamSpinEdit.Value := 0;
    end;
    FParamSpinEdit.SetFocus;
  end;

  if c = 'EDIT' then
  begin
    FIsEditing := True;

    FParamEdit.Top := pt.Y - 2;
    FParamEdit.Left := pt.X + 2;

    if Max(16,rh - lh) + lh > cw then
      FParamEdit.Width := Min(cw, rh - lh - 4)
    else
      FParamEdit.Width := Max(16,hr.Right - hr.Left);

    FParamEdit.AutoSize := EditAutoSize;
    FParamEdit.Cancelled := False;
    FParamEdit.Parent := Self;
    FParamEdit.OnUpdate := ControlUpdate;
    FParamEdit.OnReturn := AdvanceEdit;
    FParamEdit.Param := a;
    FParamEdit.Visible := True;

    v := InvHTMLPrep(v);

    PrepareParam(a,v);

    FParamEdit.Text := v;
    FParamEdit.SetFocus;
  end;

  if (c = 'MASK') then
  begin
    FIsEditing := True;

    FParamMaskEdit.Top := pt.Y - 2;
    FParamMaskEdit.Left := pt.X;

    if Max(16,rh - lh) + lh > cw then
      FParamMaskEdit.Width := Min(cw, rh - lh - 4)
    else
      FParamMaskEdit.Width := Max(16,hr.Right - hr.Left) + 16;

    FParamMaskEdit.Cancelled := False;
    FParamMaskEdit.Parent := Self;
    FParamMaskEdit.OnUpdate := ControlUpdate;
    FParamMaskEdit.OnReturn := AdvanceEdit;
    FParamMaskEdit.Param := a;
    FParamMaskEdit.Visible := True;

    PrepareParam(a,v);

    FParamMaskEdit.EditMask := InvHTMLPrep(p);

    FParamMaskEdit.Text := v;

    FParamMaskEdit.SetFocus;
  end;

  if c = 'DIR' then
  begin
    FIsEditing := True;
    hr.Left := hr.Left + FIndent * (Node.Level + 1) - GetScrollPos(Handle,SB_HORZ);
    PrepareParam(Param,v);
    StartParamDir(Node,param,v,hr);
    FIsEditing := False;
  end;

  if  (c = 'QUERY') then
  begin
    FIsEditing := True;
    PrepareParam(a,v);
    if Assigned(OnParamQuery) then
      OnParamQuery(Self,Node,a,v);
    ControlUpdate(self,a,v);
  end;

  if  (c = 'CUSTOM') then
  begin
    PrepareParam(Param,v);
    pt := ClientToScreen(Point(hr.left,hr.top));
    pt.X := pt.X + FIndent * (Node.Level + 1) - GetScrollPos(Handle,SB_HORZ);
    Dec(pt.Y, 2);
    Inc(pt.X, 2);

    FIsEditing := True;
    if Assigned(OnParamCustomEdit) then
      OnParamCustomEdit(Self,Selected,Param,v,p,Rect(pt.x,pt.Y,pt.X + hr.Right - hr.Left,pt.Y + hr.Bottom - hr.Top));
  end;

  if (c = 'LIST') then
  begin
    FIsEditing := True;
    DoList := True;
    GetHRefValue(Node.Text,a,OldValue);
    newvalue := oldvalue;
    NewValues := TStringList.Create;
    NewValues.Sorted := FParamListSorted;
    DoList := True;

    PropToList(InvHTMLPrep(p),NewValues);

    if Assigned(FOnParamList) then
      FOnParamList(self,Node,a,Newvalues,Dolist);

    if DoList then
    begin
      FIndent := TreeView_GetIndent(Handle);
      pt := Clienttoscreen(Point(hr.left,hr.bottom));
      pt.X := pt.X + FIndent * (Node.Level + 1) - GetScrollPos(Handle,SB_HORZ);

      FParamList.Top := pt.y;
      FParamList.Left := pt.x;
      FParamlist.OnUpdate := ControlUpdate;
      FParamList.OnReturn := AdvanceEdit;
      FParamlist.Param := a;
      FParamList.Parent := Self;

      SetWindowLong( FParamList.Handle, GWL_EXSTYLE,
                 GetWindowLong(FParamList.Handle, GWL_EXSTYLE) or
                 WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW);

      FParamlist.Visible := True;
      FParamList.Items.Assign(NewValues);


      PrepareParam(a,OldValue);

      FParamList.Ctl3D := False;
      FParamList.Font.Assign(self.Font);
      FParamList.SizeDropDownWidth;
      FParamList.ItemIndex := FParamList.Items.IndexOf(OldValue);
      FParamList.SetFocus;
    end;
    NewValues.Free;
    FIsEditing := False;
  end;
end;

procedure TParamTreeView.WMHScroll(var message:TMessage);
begin
  inherited;

  if FOldScrollPos <> GetScrollPos(handle,SB_HORZ) then
  begin
    Invalidate;
    FOldScrollPos := GetScrollPos(handle,SB_HORZ);
  end;

//  Invalidate;
end;

function TParamTreeview.IsParam(x, y: Integer;GetFocusRect: Boolean; var Node: TTreeNode; var HR,cr: TRect;var CID,CT,CV: string): string;
var
  r: TRect;
  s,a,fa: string;
  xsize,ysize,ml,hl,fl: Integer;
  wndStyle: Longint;

begin
  Result := '';

  if not GetFocusRect then
    Node := GetNodeAt(x,y);

  if Assigned(Node) then
  begin
    r := Node.DisplayRect(True);

    wndStyle := GetWindowLong(self.Handle, GWL_STYLE);

    if wndStyle and WS_VSCROLL = WS_VSCROLL then
    begin
      r.Right := Width - GetSystemMetrics(SM_CXVSCROLL);
    end
    else
      r.Right := Width;

//    r.Right := r.Left + Width;
    if not WordWrap then
      r.Right := 4096;

    r.Left := r.Left - 2;

    a := '';

    Canvas.Font.Assign(Font);

    HTMLDrawEx(Canvas,Node.Text,r,TImageList(HTMLImages),0,0,-1,-1,2,
      True,True,False,False,False,False,WordWrap,FMouseDown,False,1.0,clBlue,
      clNone,clNone,clGray,a,s,fa,xsize,ysize,hl,ml,hr,cr,CID,CV,CT,
      FImageCache,FContainer,Handle,0);

    a := '';

    // this is for vertically centered alignment
    if (YSize < r.Bottom - r.Top) then
      r.Top := r.Top + ((r.Bottom - r.Top - YSize) shr 1);

    if GetFocusRect then
      fl := FFocusLink + 1
    else
      fl := -1;

    if HTMLDrawEx(Canvas,Node.Text,r,TImageList(HTMLImages),x,y,fl,-1,2,
      True,False,False,False,False,False,WordWrap,FMouseDown,GetFocusRect,1.0,clBlue,
      clNone,clNone,clGray,a,s,fa,xsize,ysize,FNumHyperLinks,FHoverHyperLink,hr,cr,CID,CV,CT,
      FImageCache,FContainer,Handle,0) then
    begin
      Result := a;
    end;


  end;
end;

procedure TParamTreeview.WMMouseMove(var message: TWMMouseMove);
var
  Node : TTreeNode;
  r,cr,hr: TRect;
  s,a,f,v: string;
  xsize,ysize,hl,ml: Integer;
  CID,CV,CT: string;
  param:string;
begin
  if csDesigning in ComponentState then
  begin
    inherited;
    Exit;
  end;

  if FIsEditing then
  begin
    inherited;
    Exit;
  end;

  Node := GetNodeAt(message.XPos,message.YPos);

  if Assigned(Node) then
  begin
    param := IsParam(message.XPos,message.YPos,False,Node,hr,cr,CID,CT,CV);

    {$IFDEF TMSDEBUG}
    outputdebugstring(pchar(inttostr(FHOverIdx)+':'+inttostr(FHoverHyperLink)+':'+inttostr(fnumhyperlinks)));
    {$ENDIF}

    if param <> FOldAnchor then
      Application.CancelHint;

    if (param = '') and
       (FHoverNode <> Node) and (FHoverNode <> nil) and
        FHover then
    begin
      InvalidateRect(Handle,@FCurrHoverRect,true);
      FHoverNode := nil;
    end;

    if (CID = '') and (FCurrCtrlID <> '') then
    begin
      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar('out : '+FCurrCtrlID));
      {$ENDIF}
      InvalidateRect(Handle,@FCurrCtrlRect,True);

      FCurrCtrlID := CID;
    end;

    if (CID <> FCurrCtrlID) and (CID <> '') then
    begin
      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar('in : '+cid));
      {$ENDIF}
      InvalidateRect(Handle,@cr,True);

      FCurrCtrlID := CID;
      FCurrCtrlRect := cr;
    end;

    r := Node.DisplayRect(True);
    r.Right := r.Left + Width;
    r.Left := r.Left + 2;

    a := '';
    Canvas.Font.Assign(Font);

    HTMLDrawEx(Canvas,Node.Text,r,TImageList(HTMLImages),message.xpos,message.ypos,-1,-1,2,
      True,True,False,False,False,False,WordWrap,FMouseDown,False,1.0,clBlue,
      clNone,clNone,clGray,a,s,f,xsize,ysize,hl,ml,hr,cr,CID,CV,CT,
      FImageCache,FContainer,Handle,FLineSpacing);

    a := '';

    if (ysize < r.Bottom - r.Top) then
      r.top := r.top + ((r.bottom - r.top - ysize) shr 1);

    HTMLDrawEx(Canvas,Node.Text,r,TImageList(HTMLImages),message.xpos,message.ypos,-1,-1,2,
      True,True,False,False,False,False,WordWrap,FMouseDown,False,1.0,clBlue,
      clNone,clNone,clGray,a,s,f,xsize,ysize,hl,ml,hr,cr,CID,CV,CT,
      FImageCache,FContainer,Handle,FLineSpacing);


    if (param <> '') then
    begin
      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar('mousemove:'+param));
      {$ENDIF}
      if FHover then
      begin
        if (Node <> FHoverNode) or not EqualRect(hr,fCurrHoverRect) then
          InvalidateRect(self.handle,@fCurrHoverRect,true);
       end;

      FHoverNode := Node;

      if (Cursor <> crHandPoint) then
      begin
        FOldCursor := self.Cursor;
        Cursor := crHandPoint;
        hr.Left := hr.Left - GetScrollPos(Handle,SB_HORZ);

        if FHover then
          InvalidateRect(self.handle,@hr,true);
        FCurrHoverRect := hr;
      end;
      GetHRefValue(Node.Text,a,v);

      if (FOldAnchor <> a) and (FOldIndex <> Node.AbsoluteIndex) then
        if Assigned(FOnParamEnter) then
          FOnParamEnter(Self,Node,a,v);
        
      FOldAnchor := param;
      FOldIndex := Node.AbsoluteIndex;
    end
    else
    begin
      if (Cursor = crHandPoint) then
      begin
        Cursor := FOldCursor;
        if (FOldAnchor <> '') and (FOldIndex <> -1)  then
        begin
          if Assigned(FOnParamExit) then
            FOnParamExit(self,Items[FOldIndex],FOldAnchor, NodeParameter[Items[FOldIndex],FOldAnchor]);
          FOldAnchor := '';
          FOldIndex := -1;
          FHoverHyperlink := -1;
        end;
        if FHover then
          InvalidateRect(Handle,@FCurrHoverRect,true);
      end;
    end;
  end
  else
  begin
    if (Cursor = crHandPoint) or (FOldAnchor <> '') or (FOldIndex <> -1)  then
    begin
      if Assigned(FOnParamExit) then
        FOnParamExit(self,Items[FOldIndex],FOldAnchor, NodeParameter[Items[FOldIndex],FOldAnchor]);
      Application.CancelHint;
      Cursor := FOldCursor;
      FOldAnchor := '';
      FOldIndex := -1;
      FHoverNode := nil;
      FHoverHyperlink := -1;
      if FHover then
        InvalidateRect(Handle,@FCurrHoverRect,true);
    end;
  end;
  
  inherited;
end;

constructor TParamTreeview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVersion := '1';
  Tooltips:=False;
  FEmptyParam := '?';
  FParamColor := clGreen;
  FSelectionColor := clHighLight;
  FSelectionFontColor := clHighLightText;
  FItemHeight := 16;
  FOldScrollPos := -1;

  FParamPopup := TPopupMenu.Create(Self);
  FParamList := TPopupListBox.Create(Self);
  FParamDatePicker := TPopupDatePicker.Create(Self);
  FParamSpinEdit := TPopupSpinEdit.Create(Self);
  FParamEdit := TPopupEdit.Create(Self);
  FParamMaskEdit := TPopupMaskEdit.Create(Self);
  FImageCache := THTMLPictureCache.Create;

  ReadOnly := True;
  FShowSelection := False;
  FShadowOffset := 1;
  FShadowColor := clGray;
  FShowFocusBorder := true;
  FUpdateCount := 0;
  FWordWrap := True;
  FHover := True;
  FHoverColor := clGreen;
  FHoverFontColor := clWhite;
  FHoverHyperLink := -1;
  FNumHyperLinks := -1;
end;


destructor TParamTreeView.Destroy;
begin
  FParamPopup.Free;
  FParamList.Free;
  FParamDatePicker.Free;
  FParamSpinEdit.Free;
  FParamEdit.Free;
  FParamMaskEdit.Free;
  FImageCache.Free;
  inherited;
end;



function TParamTreeview.GetItemHeight: integer;
begin
  Result := TreeView_GetItemHeight(Handle);
end;

procedure TParamTreeview.SetItemHeight(const Value: integer);
begin
  if (Value <> FItemHeight) then
  begin
    FItemHeight := Value;
    TreeView_SetItemHeight(Handle,FItemHeight);
  end;
end;


procedure TParamTreeview.SetSelectionColor(const Value: tcolor);
begin
  FSelectionColor := Value;
  Invalidate;
end;

procedure TParamTreeview.SetSelectionFontColor(const Value: tcolor);
begin
  FSelectionFontColor := Value;
  Invalidate;
end;

procedure TParamTreeview.SetParamColor(const Value: tcolor);
begin
  FParamColor := Value;
  Invalidate;
end;

procedure TParamTreeview.Loaded;
begin
  inherited;
  FOldCursor := Cursor;
end;

procedure TParamTreeview.CreateWnd;
begin
  inherited;
  ItemHeight := FItemHeight;
end;

procedure TParamTreeview.SetImages(const Value: TImageList);
begin
  FImages := Value;
  Invalidate;
end;

procedure TParamTreeView.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;
  inherited;
end;

procedure TParamTreeview.WMSize(var message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TParamTreeview.WndProc(var Message: TMessage);
var
  sp: integer;
begin
  sp := 0;
  if HandleAllocated then
    sp := GetScrollPos(Handle,SB_HORZ);

  inherited;

  if (Message.Msg = WM_MOUSEWHEEL) and HandleAllocated then
  begin
    if (sp <> GetScrollPos(Handle,SB_HORZ)) then
      Invalidate;
  end;
end;

procedure TParamTreeview.HandlePopup(Sender: TObject);
var
  NewValue,OldValue,NodeText: string;
begin
  with (Sender as TMenuItem) do
  begin
    NewValue := Caption;
    while Pos('&',NewValue) > 0 do
      System.Delete(NewValue,Pos('&',NewValue),1);

      NodeText := Selected.Text;

      GetHRefValue(NodeText,FOldParam,OldValue);

      if Assigned(FOnParamSelect) then
        FOnParamSelect(Self,Selected,FOldParam,NewValue);

      SetHRefValue(NodeText,FOldParam,NewValue);

      Selected.Text := NodeText;

      if (OldValue <> NewValue) then
        if Assigned(FOnParamChanged) then
           FOnParamChanged(self,Selected,FOldParam,OldValue,NewValue);
  end;
end;

procedure TParamTreeview.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
  Invalidate;
end;

procedure TParamTreeview.SetShadowOffset(const Value: Integer);
begin
  FShadowOffset := Value;
  Invalidate;
end;

procedure TParamTreeview.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
  Invalidate;
end;

procedure TParamTreeview.SetShowSelection(const Value: Boolean);
begin
  FShowSelection := Value;
  Invalidate;
end;

function TParamTreeview.GetNodeParameter(Node: TTreeNode;
  HRef: String): string;
var
  Value: String;
begin
  Result := '';
  GetHRefValue(Node.Text,HRef,Value);

  Value := InvHTMLPrep(Value);
  Result := Value;

end;

procedure TParamTreeview.SetNodeParameter(Node: TTreeNode; HRef: String;
  const Value: string);
var
  NodeText,v: string;
begin
  NodeText := Node.Text;

  v := HTMLPrep(Value);

  SetHRefValue(NodeText,HRef,v);
  Node.Text := NodeText;
end;

function TParamTreeview.GetParamInfo(Node: TTreeNode; HRef: string;
  var AValue, AClass, AProp, AHint: string): Boolean;
begin
  if Assigned(Node)then
    Result := ExtractParamInfo(Node.Text,HRef,AClass,AValue,AProp,AHint)
  else
    Result := False;
end;

procedure TParamTreeview.UpdateParam(Param, Value: string);
var
  NodeText: string;
  OldValue: string;
begin
  if not Assigned(Selected) then
  begin
    Refresh;
    Exit;
  end;

  NodeText := Selected.Text;

  GetHRefValue(NodeText,Param,OldValue);

  Value := HTMLPrep(Value);

  if Assigned(FOnParamSelect) then
    FOnParamSelect(Self,Selected,Param,Value);

   SetHRefValue(NodeText,Param,Value);

   Selected.Text := NodeText;

   if OldValue <> Value then
     if Assigned(FOnParamChanged) then
       FOnParamChanged(Self,Selected,Param,OldValue,Value);
end;

procedure TParamTreeview.PrepareParam(Param: string; var Value: string);
begin
  if (Value = EmptyParam) and (EmptyParam <> '') then
    Value := '';

  if Assigned(FOnParamPrepare) then
    FOnParamPrepare(Self,Selected,Param,Value);

  if Assigned(FOnParamEditStart) then
    FOnParamEditStart(Self,Selected, Param, Value);
end;

procedure TParamTreeview.CMHintShow(var Msg: TMessage);
var
  CanShow: Boolean;
  hi: PHintInfo;
  Anchor: string;
  hr,cr: TRect;
  Node: TTreeNode;
  CID,CV,CT: string;
  v,c,p,h:string;
Begin
  CanShow := True;
  hi := PHintInfo(Msg.LParam);
  if FParamHint and not FIsEditing then
  begin
    Anchor := IsParam(hi^.cursorPos.x,hi^.cursorpos.y,False,Node,hr,cr,CID,CV,CT);

    GetParamInfo(Node,Anchor,v,c,p,h);

    if h <> '' then
      Anchor := h;

    if (Anchor <> '') then
    begin
      hi^.HintPos := clienttoscreen(hi^.CursorPos);
      hi^.hintpos.y := hi^.hintpos.y-10;
      hi^.hintpos.x := hi^.hintpos.x+10;
      if Assigned(FOnParamHint) then
        FOnParamHint(self,Node,Anchor,Anchor,CanShow);
     hi^.HintStr := anchor;
    end;
  end;
  Msg.Result := Ord(Not CanShow);
end;

procedure TParamTreeview.BeginUpdate;
begin
  inc(FUpdateCount);
  if FUpdateCount = 1 then
    SendMessage(Handle,WM_SETREDRAW,0,0);
end;

procedure TParamTreeview.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      SendMessage(Handle,WM_SETREDRAW,1,0);
      RedrawWindow(Handle,nil,0,
        RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
    end;
  end;
end;


procedure TParamTreeview.Expand(Node: TTreeNode);
begin
  inherited;
  Invalidate;
end;

function TParamTreeView.HTMLPrep(s: string): string;
begin
  s := StringReplace(s,'&','&amp;',[rfReplaceAll]);
  s := StringReplace(s,'<','&lt;',[rfReplaceAll]);
  s := StringReplace(s,'>','&gt;',[rfReplaceAll]);
  s := StringReplace(s,'"','&quot;',[rfReplaceAll]);
  Result := s;
end;

function TParamTreeView.InvHTMLPrep(s: string): string;
begin
  s := StringReplace(s,'&lt;','<',[rfReplaceAll]);
  s := StringReplace(s,'&gt;','>',[rfReplaceAll]);
  s := StringReplace(s,'&amp;','&',[rfReplaceAll]);
  s := StringReplace(s,'&quot;','"',[rfReplaceAll]);
  Result := s;
end;

procedure TParamTreeView.ControlUpdate(Sender: TObject; Param,Text:string);
var
  s: string;
begin
  s := Text;
  if (s = '') and (EmptyParam <> '') then
    s := EmptyParam;

  if Assigned(FOnParamEditDone) then
    FOnParamEditDone(Self, Selected, Param, s);

  UpdateParam(Param, s);
  FIsEditing := False;

  //SetFocus;
end;

procedure TParamTreeview.SetLineSpacing(const Value: Integer);
begin
  FLineSpacing := Value;
  Invalidate;
end;

procedure TParamTreeview.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if (Msg.CharCode in [VK_RETURN]) then
    Msg.Result := 1;
end;

procedure TParamTreeview.KeyPress(var Key: Char);
var
  r: TRect;
  s: string;
begin
  inherited;
  if (Key = #13) and Assigned(Selected) then
  begin
    if GetParamItemRefCount(Selected.AbsoluteIndex) > 0 then
    begin
      s := ParamNodeRefs[Selected.AbsoluteIndex,FFocusLink];
      r := GetParamRect(s);

      StartParamEdit(s,Selected,r);
      key := #0;
    end;
  end;
end;

function TParamTreeview.GetParamItemRefCount(Item: Integer): Integer;
var
  s: string;
begin
  Result := 0;
  s := Uppercase(Items[Item].Text);
  while (pos('HREF=',s) > 0) do
  begin
    Result := Result  + 1;
    system.Delete(s,1, pos('HREF=',s) + 5);
  end;
end;

function TParamTreeview.GetParamNodeRefCount(Node: TTreeNode): Integer;
var
  s: string;
begin
  Result := 0;
  s := Uppercase(Node.Text);
  while (pos('HREF=',s) > 0) do
  begin
    Result := Result  + 1 ;
    system.Delete(s,1, pos('HREF=',s) + 5);
  end;
end;

function TParamTreeview.GetParamItemRefs(Item, Index: Integer): string;
var
  j: Integer;
  s: string;
begin
  j := 0;
  Result := '';

  s := Uppercase(Items[Item].Text);

  while (pos('HREF="',s) > 0) do
  begin
    if (Index = j) then
    begin
      system.Delete(s,1, pos('HREF="',s) + 5);

      if pos('"',s) > 0 then
      begin
        system.Delete(s,pos('"',s), length(s));
        Result := s;
      end;
      Exit;
    end
    else
      j := j + 1;

    system.Delete(s,1, pos('HREF=',s) + 5);
  end;

  if Index > j then
  begin
    Result := GetParamItemRefs(Item,0);
    if Result <> '' then
      FFocusLink := 0;
  end;
end;

function TParamTreeview.GetParamRefCount: Integer;
var
  i: Integer;
  s: string;
begin
  Result := 0;

  for i := 1 to Items.Count do
  begin
    s := Uppercase(Items[i - 1].Text);
    while (pos('HREF=',s) > 0) do
    begin
      Result := Result  + 1 ;
      system.Delete(s,1, pos('HREF=',s) + 5);
    end;
  end;
end;

function TParamTreeview.GetParamRefs(Index: Integer): string;
var
  i,j: Integer;
  s: string;
begin
  j := 0;
  Result := '';

  for i := 1 to Items.Count do
  begin
    s := Uppercase(Items[i - 1].Text);
    while (pos('HREF="',s) > 0) do
    begin
      if (Index = j) then
      begin
        system.Delete(s,1, pos('HREF="',s) + 5);

        if pos('"',s) > 0 then
        begin
          system.Delete(s,pos('"',s), length(s));

          Result := s;
        end;
        Exit;
      end
      else
        j := j + 1;
      system.Delete(s,1, pos('HREF=',s) + 5);
    end;
  end;
end;

function TParamTreeview.GetParamRect(href: string): TRect;
var
  cr: TRect;
  CID,CV,CT: string;
  Node: TTreeNode;
begin
  if not Assigned(Selected) then
    Exit;
{$IFDEF TMSDEBUG}
  outputdebugstring('get rect');
{$ENDIF}
//  i := FFocusLink + 1;
  Node := Selected;

  IsParam(0,0,True,Node,Result,cr,CID,CV,CT);
end;

function TParamTreeview.GetParamNodeIndex(Node: TTreeNode;
  href: string): Integer;
var
  j: Integer;
  s,u: string;
begin
  j := 0;
  Result := -1;

  s := Uppercase(Node.Text);

  while (pos('HREF="',s) > 0) do
  begin
    system.Delete(s,1, pos('HREF="',s) + 5);

    if pos('"',s) > 0 then
    begin
      u := s;
      system.Delete(u,pos('"',u), length(u));

      if UpperCase(href) = u then
      begin
        Result := j;
        Exit;
      end;
    end;
    j := j + 1;
    system.Delete(s,1, pos('"',s));
  end;
end;

procedure TParamTreeview.KeyDown(var Key: Word; Shift: TShiftState);
var
 ir: TRect;
 cr,hr: TRect;
 CID,CV,CT: string;
 Node: TTreeNode;
begin
  if key in [VK_LEFT, VK_RIGHT] then
  begin
    if Assigned(Selected) then
    begin
      ir := Selected.DisplayRect(True);

      Node := Selected;
      IsParam(ir.Left + 2,ir.Top + 2,False,Node,cr,hr,CID,CV,CT);
      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar('num:'+inttostr(fnumhyperlinks)+':'+inttostr(ffocuslink)));
      {$ENDIF}

      if FNumHyperLinks > 1 then
      begin
        if key = VK_LEFT then
        begin
          if FFocusLink > 0 then
          begin
            Dec(FFocusLink);
            Key := 0;
            InvalidateRect(Handle,@ir,True);
          end;
        end;

        if key = VK_RIGHT then
        begin
          if FFocusLink < FNumHyperLinks - 1 then
          begin
            Inc(FFocusLink);
            Key := 0;
            InvalidateRect(Handle,@ir,True);
          end;
        end;
      end;
    {$IFDEF TMSDEBUG}
      outputdebugstring(pchar('key focus link to : '+inttostr(ffocuslink)));
    {$ENDIF}
    end;
  end;

  inherited;
end;

{$WARNINGS OFF}
function EditCallBack (Wnd: HWND; uMsg: UINT; WParam, lpData: LPARAM): Integer; stdcall;
var
  Temp: String;
  pt: TPoint;
  r: TRect;
begin
  if uMsg = BFFM_INITIALIZED then
  begin
    with TParamTreeView(lpData) Do
    begin
      if FEditValue = '' then
        Temp := GetCurrentDir
      else
        Temp := ExcludeTrailingBackslash (FEditValue);

      SendMessage(Wnd, BFFM_SETSELECTION, 1, LParam(Temp));

      with TParamTreeView(lpData) do
      begin
        pt := FEditPos;
        pt := ClientToScreen(pt);
        GetWindowRect(Wnd,r);

        if pt.X + (r.Right - r.Left) > Screen.DesktopWidth then
          pt.X := pt.X - (r.Right - r.Left);

        if pt.Y + (r.Bottom - r.Top) < Screen.DesktopHeight then
          SetWindowPos(wnd,HWND_NOTOPMOST,pt.X,pt.Y,0,0,SWP_NOSIZE or SWP_NOZORDER)
        else
          SetWindowPos(wnd,HWND_NOTOPMOST,pt.X,pt.Y-(r.Bottom - r.Top)-Height,0,0,SWP_NOSIZE or SWP_NOZORDER)
      end;
    end;
  end;
  Result := 0;
end;
{$WARNINGS ON}

procedure TParamTreeView.StartParamDir(Node: TTreeNode; param, curdir: string; hr: TRect);
var
  bi: TBrowseInfo;
  iIdList: PItemIDList;
  ResStr: array[0..MAX_PATH] of char;
  MAlloc: IMalloc;
  s: string;
  // BIF_NONEWFOLDERBUTTON
begin

  if curdir <> '' then
    StrPCopy(ResStr,curdir)
  else
    StrPCopy(ResStr,GetCurrentDir);

  FEditValue := resstr;
  FEditPos := Point(hr.Left,hr.Bottom);

  FillChar(bi, sizeof(bi), #0);
  bi.hwndOwner := Application.Handle;
  bi.pszDisplayName := ResStr;

  bi.lpszTitle := PChar('Select directory');
  bi.ulFlags := BIF_RETURNONLYFSDIRS;
  bi.lpfn := EditCallBack;
  bi.lParam := LParam(Self);

  iIdList := Nil;
  try
    iIdList := SHBrowseForFolder(bi);
  except
  end;

  if iIdList <> Nil then
  begin
    try
      FillChar(ResStr,sizeof(ResStr),#0);
      if SHGetPathFromIDList (iIdList, ResStr) then
      begin
        s := resstr;
        if Assigned(OnParamChanged) then
          OnParamChanged(Self, Node, param, curdir, s);

        UpdateParam(Param,ResStr);

        ControlUpdate(self,Param,ResStr);

      end;
    finally
      SHGetMalloc(MAlloc);
      Malloc.Free(iIdList);
    end;
  end;
end;

procedure TParamTreeview.EditParam(href: string);
var
  tn: TTreeNode;

begin
  tn := GetParamRefNode(href);
  if Assigned(tn) then
  begin
    Selected := tn;
    Selected.Expanded :=true;
    StartParamEdit(href, tn, GetParamRect(href));
  end;
end;

function TParamTreeview.GetParamRefNode(href: string): TTreeNode;
var
  i: Integer;
  tn: TTreeNode;
begin
  Result := nil;

  for i := 1 to Items.Count do
  begin
    tn := Items[i - 1];

    if ParamNodeIndex[tn, href] <> -1 then
    begin
      Result := tn;
      Break;
    end;
  end;
end;

function TParamTreeview.GetParameter(href: string): string;
var
  tn: TTreeNode;
begin
  Result := '';
  tn := ParamRefNode[href];

  if Assigned(tn) then
    Result := NodeParameter[tn,href];
end;

procedure TParamTreeview.SetParameter(href: string; const Value: string);
var
  tn: TTreeNode;
begin
  tn := ParamRefNode[href];

  if Assigned(tn) then
    NodeParameter[tn,href] := Value;
end;

procedure TParamTreeview.SetHover(const Value: Boolean);
begin
  FHover := Value;
  Invalidate;
end;

procedure TParamTreeView.AdvanceEdit(Sender: TObject);
var
  idx: Integer;
  s,v,c,p,h: string;
  Node: TTreeNode;
begin
  if not FAdvanceOnReturn then
    Exit;

  if FFocusLink = -1 then
    Exit;

  idx := FFocusLink;
  s  := ParamNodeRefs[Selected.AbsoluteIndex,idx];
  idx := ParamIndex[s];

  if idx < ParamRefCount - 1 then
    inc(idx)
  else
    idx := 0;

  s := ParamRefs[idx];

  if (s <> '') then
  begin
    Node := ParamRefNode[s];
    Selected := Node;
    FFocusLink := ParamNodeIndex[Node, s];
    GetParamInfo(Node, s, v, c, p, h);
    if c <> '' then
      StartParamEdit(s,Node,GetParamRect(s));
  end;
end;


function TParamTreeview.GetParamIndex(href: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 1 to ParamRefCount do
  begin
    if StrIComp(pchar(ParamRefs[i - 1]),pchar(href))=0 then
    begin
      Result := i - 1;
      Break;
    end;
  end;
end;

procedure TParamTreeview.Change(Node: TTreeNode);
begin
  inherited;
  if FFocusLink >= GetParamNodeRefCount(Node) then
    FFocusLink := 0;
end;

function TParamTreeview.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TParamTreeview.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TParamTreeview.SetVersion(const Value: string);
begin

end;

end.

