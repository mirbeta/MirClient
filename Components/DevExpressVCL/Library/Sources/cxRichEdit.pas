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

unit cxRichEdit;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Messages, ActiveX, OleDlg, OleConst, OleCtnrs, Classes, ClipBrd, ComCtrls, Controls, Dialogs,
  Forms, Graphics, Menus, RichEdit, StdCtrls, SysUtils,
  cxDataUtils, dxCore, dxCoreClasses, dxMessages, cxClasses, cxContainer, cxControls, cxEdit, cxDrawTextUtils,
  cxGraphics, cxLookAndFeels, cxMemo, cxScrollbar, cxTextEdit, dxGDIPlusApi, cxRichEditUtils, cxExtEditConsts,
  dxDrawRichTextUtils, cxGeometry;

(*$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IOleLink);'*)

type
  TcxRichEditClass = (recRichEdit10, recRichEdit20, recRichEdit41);

const
  cxMaxVersionRichEditClass = recRichEdit41;
  cxMinVersionRichEditClass = recRichEdit20;

type
  TcxRichEditStreamMode = (resmSelection, resmPlainRtf, resmRtfNoObjs, resmUnicode, resmTextIzed);
  TcxRichEditStreamModes = set of TcxRichEditStreamMode;

  TcxTextRange = record
    chrg: TCharRange;
    lpstrText: PChar;
  end;

  TReObject = packed record
    cbStruct: DWORD;        // Size of structure
    cp: Cardinal;           // Character position of object
    clsid: TCLSID;          // Class ID of object
    oleobj: IOleObject;     // OLE object interface
    stg: IStorage;          // Associated storage interface
    olesite: IOLEClientSite;// Associated client site interface
    sizel: TSize;           // Size of object (may be 0,0)
    dvaspect: DWORD;        // Display aspect to use
    dwFlags: DWORD;         // Object status flags
    dwUser: DWORD;          // Dword for user's use
  end;

  TcxCustomRichEdit = class;

  TcxRichEditURLClickEvent = procedure(Sender: TcxCustomRichEdit; const URLText: string; Button: TMouseButton) of object;
  TcxRichEditURLMoveEvent = procedure(Sender: TcxCustomRichEdit; const URLText: string) of object;
  TcxRichEditQueryInsertObjectEvent = procedure(Sender: TcxCustomRichEdit; var AAllowInsertObject: Boolean;
    const ACLSID: TCLSID) of object;

  TcxCustomRichEditViewInfo = class(TcxCustomMemoViewInfo)
  protected
    function GetBackgroundPaintingStyle: TcxEditBackgroundPaintingStyle; override;
  public
    DrawBitmap: HBITMAP;
    IsDrawBitmapDirty: Boolean;
    PrevDrawBitmapSize: TSize;
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawNativeStyleEditBackground(ACanvas: TcxCanvas;
      ADrawBackground: Boolean; ABackgroundBrush: TBrushHandle); override;
    procedure DrawText(ACanvas: TcxCanvas); override;
    function GetUpdateRegion(AViewInfo: TcxContainerViewInfo): TcxRegion; override;
    function NeedShowHint(ACanvas: TcxCanvas; const P: TPoint;
      const AVisibleBounds: TRect; out AText: TCaption;
      out AIsMultiLine: Boolean; out ATextRect: TRect; AMaxLineCount: Integer = 0): Boolean; override;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TcxCustomRichEditProperties = class;

  TcxCustomRichEditViewData = class(TcxCustomMemoViewData)
  private
    function GetProperties: TcxCustomRichEditProperties;
  protected
    function InternalGetEditContentSize(ACanvas: TcxCanvas;
      const AEditValue: TcxEditValue;
      const AEditSizeProperties: TcxEditSizeProperties): TSize; override;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
      AIsMouseEvent: Boolean); override;
    property Properties: TcxCustomRichEditProperties read GetProperties;
  end;

  {IRichEditOleCallback}

  TcxRichInnerEdit = class;

  IcxRichEditOleCallback = interface(IUnknown)
    ['{00020D00-0000-0000-C000-000000000046}']
    function GetNewStorage(out stg: IStorage): HRESULT; stdcall;
    function GetInPlaceContext(out Frame: IOleInPlaceFrame;
      out Doc: IOleInPlaceUIWindow; lpFrameInfo: POleInPlaceFrameInfo): HRESULT; stdcall;
    function ShowContainerUI(fShow: BOOL): HRESULT; stdcall;
    function QueryInsertObject(const clsid: TCLSID; stg: IStorage; cp: longint): HRESULT; stdcall;
    function DeleteObject(oleobj: IOLEObject): HRESULT; stdcall;
    function QueryAcceptData(dataobj: IDataObject; var cfFormat: TClipFormat;
      reco: DWORD; fReally: BOOL; hMetaPict: HGLOBAL): HRESULT; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HRESULT; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
      out dataobj: IDataObject): HRESULT; stdcall;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
      var dwEffect: DWORD): HRESULT; stdcall;
    function GetContextMenu(seltype: Word; oleobj: IOleObject;
      const chrg: TCharRange; var menu: HMENU): HRESULT; stdcall;
  end;

  { IRichEditOle }

  IcxRichEditOle = interface(IUnknown)
    ['{00020D00-0000-0000-C000-000000000046}']
    function GetClientSite(out clientSite: IOleClientSite): HResult; stdcall;
    function GetObjectCount: HResult; stdcall;
    function GetLinkCount: HResult; stdcall;
    function GetObject(iob: Longint; out reobject: TReObject;
      dwFlags: DWORD): HResult; stdcall;
    function InsertObject(var reobject: TReObject): HResult; stdcall;
    function ConvertObject(iob: Longint; rclsidNew: TIID;
      lpstrUserTypeNew: LPCSTR): HResult; stdcall;
    function ActivateAs(rclsid: TIID; rclsidAs: TIID): HResult; stdcall;
    function SetHostNames(lpstrContainerApp: LPCSTR;
      lpstrContainerObj: LPCSTR): HResult; stdcall;
    function SetLinkAvailable(iob: Longint; fAvailable: BOOL): HResult; stdcall;
    function SetDvaspect(iob: Longint; dvaspect: DWORD): HResult; stdcall;
    function HandsOffStorage(iob: Longint): HResult; stdcall;
    function SaveCompleted(iob: Longint; const stg: IStorage): HResult; stdcall;
    function InPlaceDeactivate: HResult; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
    function GetClipboardData(var chrg: TCharRange; reco: DWORD;
      out dataobj: IDataObject): HResult; stdcall;
    function ImportDataObject(dataobj: IDataObject; cf: TClipFormat;
      hMetaPict: HGLOBAL): HResult; stdcall;
  end;

  { TcxRichEditOleCallback }

  TcxRichEditOleCallback = class(TcxIUnknownObject, IcxRichEditOleCallback)
  private
    FEdit: TcxRichInnerEdit;
    FDocParentForm: IVCLFrameForm;
    FParentFrame: IVCLFrameForm;
    FAccelTable: HAccel;
    FAccelCount: Integer;
    procedure AssignParentFrame;
    procedure CreateAccelTable;
    procedure DestroyAccelTable;
  protected
    property ParentFrame: IVCLFrameForm read FParentFrame;
    property DocParentForm: IVCLFrameForm read FDocParentForm;
  public
    constructor Create(AOwner: TcxRichInnerEdit);

    //IRichEditOleCallback
    function ContextSensitiveHelp(fEnterMode: BOOL): HRESULT; stdcall;
    function DeleteObject(oleobj: IOLEObject): HRESULT; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
      out dataobj: IDataObject): HRESULT; stdcall;
    function GetContextMenu(seltype: Word; oleobj: IOleObject;
      const chrg: TCharRange; var menu: HMENU): HRESULT; stdcall;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
      var dwEffect: DWORD): HRESULT; stdcall;
    function GetInPlaceContext(out Frame: IOleInPlaceFrame;
      out Doc: IOleInPlaceUIWindow; lpFrameInfo: POleInPlaceFrameInfo): HRESULT; stdcall;
    function GetNewStorage(out stg: IStorage): HRESULT; stdcall;
    function QueryInsertObject(const clsid: TCLSID; stg: IStorage; cp: longint): HRESULT; stdcall;
    function QueryAcceptData(dataobj: IDataObject; var cfFormat: TClipFormat;
      reco: DWORD; fReally: BOOL; hMetaPict: HGLOBAL): HRESULT; stdcall;
    function ShowContainerUI(fShow: BOOL): HRESULT; stdcall;
  end;

  { TcxCustomRichEditProperties }

  TcxCustomRichEditProperties = class(TcxCustomMemoProperties)
  strict private
    FAdvancedTypography: Boolean;
    FAllowObjects: Boolean;
    FAutoURLDetect: Boolean;
    FHideScrollBars: Boolean;
    FMemoMode: Boolean;
    FPlainText: Boolean;
    FPlainTextChanged: Boolean;
    FRichEditClass: TcxRichEditClass;
    FSelectionBar: Boolean;
    FStreamModes: TcxRichEditStreamModes;
    FZoomFactor: Double;

    FOnQueryInsertObject: TcxRichEditQueryInsertObjectEvent;
    FOnProtectChange: TRichEditProtectChange;
    FOnResizeRequest: TRichEditResizeEvent;
    FOnSaveClipboard: TRichEditSaveClipboard;
    FOnSelectionChange: TNotifyEvent;
    FOnURLClick: TcxRichEditURLClickEvent;
    FOnURLMove: TcxRichEditURLMoveEvent;

    function GetStreamModes: TcxRichEditStreamModes;
    function IsZoomFactorStored: Boolean;
    procedure SetAdvancedTypography(const Value: Boolean);
    procedure SetAllowObjects(const Value: Boolean);
    procedure SetAutoURLDetect(const Value: Boolean);
    procedure SetHideScrollBars(Value: Boolean);
    procedure SetMemoMode(Value: Boolean);
    procedure SetPlainText(Value: Boolean);
    procedure SetRichEditClass(AValue: TcxRichEditClass);
    procedure SetSelectionBar(Value: Boolean);
    procedure SetStreamModes(const Value: TcxRichEditStreamModes);
    procedure SetOnQueryInsertObject(Value: TcxRichEditQueryInsertObjectEvent);
    procedure SetZoomFactor(AValue: Double);
  protected
    function CanValidate: Boolean; override;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function SupportsMultiThreading: Boolean; override;

    property PlainTextChanged: Boolean read FPlainTextChanged;
  public
    constructor Create(AOwner: TPersistent); override;

    class function GetContainerClass: TcxContainerClass; override;
    function GetDisplayText(const AEditValue: TcxEditValue;
      AFullText: Boolean = False; AIsInplace: Boolean = True): string; override;
    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; override;
    function GetSpecialFeatures: TcxEditSpecialFeatures; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function IsResetEditClass: Boolean; override;
    property AdvancedTypography: Boolean read FAdvancedTypography write SetAdvancedTypography;
    property AllowObjects: Boolean read FAllowObjects write SetAllowObjects default False;
    property AutoURLDetect: Boolean read FAutoURLDetect write SetAutoURLDetect default False;
    property PlainText: Boolean read FPlainText write SetPlainText default False;
    property RichEditClass: TcxRichEditClass read FRichEditClass write SetRichEditClass default cxMaxVersionRichEditClass;
    // !!!
    property HideScrollBars: Boolean read FHideScrollBars write SetHideScrollBars default True;
    property MemoMode: Boolean read FMemoMode write SetMemoMode default False;
    property SelectionBar: Boolean read FSelectionBar write SetSelectionBar default False;
    property StreamModes: TcxRichEditStreamModes read GetStreamModes write SetStreamModes default [];
    property ZoomFactor: Double read FZoomFactor write SetZoomFactor stored IsZoomFactorStored;
    property OnQueryInsertObject: TcxRichEditQueryInsertObjectEvent read FOnQueryInsertObject write SetOnQueryInsertObject;
    property OnProtectChange: TRichEditProtectChange read FOnProtectChange write FOnProtectChange;
    property OnResizeRequest: TRichEditResizeEvent read FOnResizeRequest write FOnResizeRequest;
    property OnSaveClipboard: TRichEditSaveClipboard read FOnSaveClipboard write FOnSaveClipboard;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnURLClick: TcxRichEditURLClickEvent read FOnURLClick write FOnURLClick;
    property OnURLMove: TcxRichEditURLMoveEvent read FOnURLMove write FOnURLMove;
  end;

  { TcxRichEditProperties }

  TcxRichEditProperties = class(TcxCustomRichEditProperties)
  published
    property Alignment;
    property AllowObjects;
    property AssignedValues;
    property AutoSelect;
    property AutoURLDetect;
    property ClearKey;
    property HideScrollBars;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property MemoMode;
    property OEMConvert;
    property PlainText;
    property ReadOnly;
    property RichEditClass;
    property ScrollBars;
    property SelectionBar;
    property StreamModes;
    property VisibleLineCount;
    property WantReturns;
    property WantTabs;
    property WordWrap;
    property ZoomFactor;
    property OnQueryInsertObject;
    property OnChange;
    property OnEditValueChanged;
    property OnProtectChange;
    property OnResizeRequest;
    property OnSaveClipboard;
    property OnSelectionChange;
    property OnURLClick;
    property OnURLMove;
  end;

  { TcxOleUILinkInfo }

  TcxOleUILinkInfo = class(TcxIUnknownObject, IOleUILinkInfo)
  private
    FRichEdit: TcxRichInnerEdit;
    FReObject: TReObject;
    FOleLink: IOleLink;
  public
    constructor Create(AOwner: TcxRichInnerEdit; AReObject: TReObject);
    destructor Destroy; override;

    //IOleUILinkInfo
    function GetLastUpdate(dwLink: Longint; var LastUpdate: TFileTime): HResult; stdcall;

    //IOleUILinkContainer
    function GetNextLink(dwLink: Longint): Longint; stdcall;
    function SetLinkUpdateOptions(dwLink: Longint; dwUpdateOpt: Longint): HResult; stdcall;
    function GetLinkUpdateOptions(dwLink: Longint;
      var dwUpdateOpt: Longint): HResult; stdcall;
    function SetLinkSource(dwLink: Longint; pszDisplayName: PChar;
      lenFileName: Longint; var chEaten: Longint;
      fValidateSource: BOOL): HResult; stdcall;
    function GetLinkSource(dwLink: Longint; var pszDisplayName: PChar;
      var lenFileName: Longint; var pszFullLinkType: PChar;
      var pszShortLinkType: PChar; var fSourceAvailable: BOOL;
      var fIsSelected: BOOL): HResult; stdcall;
    function OpenLinkSource(dwLink: Longint): HResult; stdcall;
    function UpdateLink(dwLink: Longint; fErrorMessage: BOOL;
      fErrorAction: BOOL): HResult; stdcall;
    function CancelLink(dwLink: Longint): HResult; stdcall;
  end;

  { TcxOleUIObjInfo }

  TcxOleUIObjInfo = class(TcxIUnknownObject, IOleUIObjInfo)
  private
    FRichEdit: TcxRichInnerEdit;
    FReObject: TReObject;

    function GetObjectDataSize: Integer;
  public
    constructor Create(AOwner: TcxRichInnerEdit; AReObject: TReObject);

    //IOleUIObjInfo
    function GetObjectInfo(dwObject: Longint;
      var dwObjSize: Longint; var lpszLabel: PChar;
      var lpszType: PChar; var lpszShortType: PChar;
      var lpszLocation: PChar): HResult; stdcall;
    function GetConvertInfo(dwObject: Longint; var ClassID: TCLSID;
      var wFormat: Word; var ConvertDefaultClassID: TCLSID;
      var lpClsidExclude: PCLSID; var cClsidExclude: Longint): HResult; stdcall;
    function ConvertObject(dwObject: Longint; const clsidNew: TCLSID): HResult; stdcall;
    function GetViewInfo(dwObject: Longint; var hMetaPict: HGlobal;
      var dvAspect: Longint; var nCurrentScale: Integer): HResult; stdcall;
    function SetViewInfo(dwObject: Longint; hMetaPict: HGlobal;
      dvAspect: Longint; nCurrentScale: Integer;
      bRelativeToOrig: BOOL): HResult; stdcall;
  end;

  { TcxCustomRichEdit }

  TcxCustomRichEdit = class(TcxCustomMemo, IcxInplaceEditIMEHelper)
  private
    FDefAttributes2: TcxTextAttributes2;
    FEditPopupMenu: TComponent;
    FIsNullEditValue: Boolean;
    FSelAttributes2: TcxTextAttributes2;
    FLastLineCount: Integer;
    FParagraph2: TcxParaAttributes2;
    FPropertiesChange: Boolean;

    procedure DoProtectChange(Sender: TObject; AStartPos, AEndPos: Integer; var AAllowChange: Boolean);
    procedure DoSaveClipboard(Sender: TObject; ANumObjects, ANumChars: Integer; var ASaveClipboard: Boolean);
    procedure EditPopupMenuClick(Sender: TObject);
    function GetLines: TStrings;
    function GetInnerRich: TcxRichInnerEdit;
    procedure SetLines(Value: TStrings);
    function GetActiveProperties: TcxCustomRichEditProperties;
    function GetProperties: TcxCustomRichEditProperties;
    function GetRichVersion: Integer;
    procedure SetProperties(Value: TcxCustomRichEditProperties);
    function GetCanUndo: Boolean;
    function GetDefAttributes: TTextAttributes;
    function GetDefaultConverter: TConversionClass;
    function GetPageRect: TRect;
    function GetParagraph: TParaAttributes;
    function GetSelAttributes: TTextAttributes;
    procedure RefreshScrollBars;
    procedure SetDefAttributes(const Value: TTextAttributes);
    procedure SetDefAttributes2(const Value: TcxTextAttributes2);
    procedure SetDefaultConverter(Value: TConversionClass);
    procedure SetPageRect(const Value: TRect);
    procedure SetSelAttributes2(Value: TcxTextAttributes2);
    procedure SetSelAttributes(const Value: TTextAttributes);

    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure EMCanPaste(var Message: TMessage); message EM_CANPASTE;
    procedure WMSetFont(var Message: TWMSetFont); message WM_SETFONT;
  protected
    procedure AdjustInnerEdit; override;
    procedure AdjustVisibleFontHeight(AVisibleFont: TFont); override;
    function CanFocusOnClick: Boolean; override;
    function CanKeyDownModifyEdit(Key: Word; Shift: TShiftState): Boolean; override;
    function DoShowPopupMenu(AMenu: TComponent; X, Y: Integer): Boolean; override;
    procedure DoSpellCheckerPostEditValue; override;
    function GetEditValue: TcxEditValue; override;
    function GetInnerEditClass: TControlClass; override;
    procedure ChangeHandler(Sender: TObject); override;
    procedure SelChange(Sender: TObject); override;
    procedure Initialize; override;
    procedure InitializeInnerEdit; override;
    procedure InternalSetEditValue(const Value: TcxEditValue; AValidateEditValue: Boolean); override;
    procedure InternalValidateDisplayValue(const ADisplayValue: TcxEditValue); override;
    function CanDeleteSelection: Boolean;
    procedure Changed(Sender: TObject);
    procedure DoOnResizeRequest(const R: TRect);
    procedure DoOnSelectionChange;
    procedure DoScrollUIModeChanged; override;
    procedure DoTextChanged; override;
    procedure PropertiesChanged(Sender: TObject); override;
    procedure ResetEditValue; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure SynchronizeDisplayValue; override;
    procedure SynchronizeEditValue; override;
    function GetEditPopupMenuInstance: TComponent; virtual;
    function IsNavigationKey(Key: Word; Shift: TShiftState): Boolean; virtual;
    function UpdateContentOnFocusChanging: Boolean; override;
    procedure UpdateEditPopupMenuItems(APopupMenu: TComponent); virtual;

    // IcxInplaceEditIMEHelper
    procedure IMEComposition(var AMessage: TMessage);
    procedure IMEStartComposition;

    property EditPopupMenu: TComponent read FEditPopupMenu write FEditPopupMenu;
    property InnerRich: TcxRichInnerEdit read GetInnerRich;
    property PropertiesChange: Boolean read FPropertiesChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ClearSelection; override;
    procedure CutToClipboard; override;
    function FindTexT(const ASearchStr: string; AStartPos, ALength: Integer; AOptions: TSearchTypes; AForward: Boolean = True): Integer;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure InsertTable(AColumnCount, ARowCount: Integer; AParams: TcxRichEditTableParams);
    procedure PasteFromClipboard; override;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue; out EditValue: TcxEditValue; AEditFocused: Boolean); override;
    procedure Print(const Caption: string); virtual;
    procedure SaveSelectionToStream(Stream: TStream); virtual;
    procedure Undo; override;
    function InsertObject: Boolean;
    function PasteSpecial: Boolean;
    function ShowObjectProperties: Boolean;
    class procedure RegisterConversionFormat(const AExtension: string; AConversionClass: TConversionClass);
    property ActiveProperties: TcxCustomRichEditProperties read GetActiveProperties;
    property CanUndo: Boolean read GetCanUndo;
    property DefAttributes: TTextAttributes read GetDefAttributes write SetDefAttributes;
    property DefAttributes2: TcxTextAttributes2 read FDefAttributes2 write SetDefAttributes2;
    property DefaultConverter: TConversionClass read GetDefaultConverter write SetDefaultConverter;
    property Lines: TStrings read GetLines write SetLines;
    property PageRect: TRect read GetPageRect write SetPageRect;
    property Paragraph: TParaAttributes read GetParagraph;
    property Paragraph2: TcxParaAttributes2 read FParagraph2;
    property Properties: TcxCustomRichEditProperties read GetProperties write SetProperties;
    property RichVersion: Integer read GetRichVersion;
    property SelAttributes: TTextAttributes read GetSelAttributes write SetSelAttributes;
    property SelAttributes2: TcxTextAttributes2 read FSelAttributes2 write SetSelAttributes2;
  end;

  { TcxRichEdit }

  TcxRichEdit = class(TcxCustomRichEdit)
  private
    function GetActiveProperties: TcxRichEditProperties;
    function GetProperties: TcxRichEditProperties;
    procedure SetProperties(Value: TcxRichEditProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxRichEditProperties read GetActiveProperties;
  published
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxRichEditProperties read GetProperties
      write SetProperties;
    property Lines; // must be after Properties because of Properties.Alignment
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TcxRichInnerEditHelper = class(TcxInterfacedPersistent,
    IcxContainerInnerControl, IcxCustomInnerEdit, IcxInnerTextEdit,
    IcxInnerMemo)
  private
    FEdit: TcxRichInnerEdit;
  protected
    property Edit: TcxRichInnerEdit read FEdit;
  public
    constructor Create(AEdit: TcxRichInnerEdit); reintroduce; virtual;

    // IcxContainerInnerControl
    function GetControlContainer: TcxContainer;
    function GetControl: TWinControl;

    // IcxCustomInnerEdit
    function CallDefWndProc(AMsg: UINT; WParam: WPARAM;
      LParam: LPARAM): LRESULT;
    function CanProcessClipboardMessages: Boolean;
    function GetEditValue: TcxEditValue;
    function GetOnChange: TNotifyEvent;
    procedure LockBounds(ALock: Boolean);
    procedure SafelySetFocus;
    procedure SetEditValue(const Value: TcxEditValue);
    procedure SetParent(Value: TWinControl);
    procedure SetOnChange(Value: TNotifyEvent);

    // IcxInnerTextEdit
    procedure ClearSelection;
    procedure CopyToClipboard;
    function GetAlignment: TAlignment;
    function GetAutoSelect: Boolean;
    function GetCharCase: TEditCharCase;
    function GetEchoMode: TcxEditEchoMode;
    function GetFirstVisibleCharIndex: Integer;
    function GetHideSelection: Boolean;
    function GetImeLastChar: Char;
    function GetImeMode: TImeMode;
    function GetImeName: TImeName;
    function GetInternalUpdating: Boolean;
    function GetMaxLength: Integer;
    function GetMultiLine: Boolean;
    function GetOEMConvert: Boolean;
    function GetOnSelChange: TNotifyEvent;
    function GetPasswordChar: TCaptionChar;
    function GetReadOnly: Boolean;
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    function GetUseLeftAlignmentOnEditing: Boolean;
    procedure SelectAll;
    procedure SetAlignment(Value: TAlignment);
    procedure SetAutoSelect(Value: Boolean);
    procedure SetCharCase(Value: TEditCharCase);
    procedure SetEchoMode(Value: TcxEditEchoMode);
    procedure SetHideSelection(Value: Boolean);
    procedure SetInternalUpdating(Value: Boolean);
    procedure SetImeMode(Value: TImeMode);
    procedure SetImeName(const Value: TImeName);
    procedure SetMaxLength(Value: Integer);
    procedure SetOEMConvert(Value: Boolean);
    procedure SetOnSelChange(Value: TNotifyEvent);
    procedure SetPasswordChar(Value: TCaptionChar);
    procedure SetReadOnly(Value: Boolean);
    procedure SetSelLength(Value: Integer);
    procedure SetSelStart(Value: Integer);
    procedure SetSelText(Value: string);
    procedure SetUseLeftAlignmentOnEditing(Value: Boolean);
    function GetTextHint: string;
    procedure SetTextHint(Value: string);

    // IcxInnerMemo
    function GetCaretPos: TPoint;
    function GetLines: TStrings;
    function GetScrollBars: TcxScrollStyle;
    function GetWantReturns: Boolean;
    function GetWantTabs: Boolean;
    function GetWordWrap: Boolean;
    procedure SetCaretPos(const Value: TPoint);
    procedure SetScrollBars(Value: TcxScrollStyle);
    procedure SetWantReturns(Value: Boolean);
    procedure SetWantTabs(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
  end;

  { TcxRichEditStrings }

  TcxRichEditStreamOperation = (esoLoadFrom, esoSaveTo);

  TcxRichEditStreamOperationInfo = record
    EditStream: TEditStream;
    StreamInfo: TRichEditStreamInfo;
    TextType: Longint;
    Encoding: TEncoding
  end;

  TcxRichEditStrings = class(TStrings)
  private
    FConverter: TConversion;
    FIsModification: Boolean;
    FRichEdit: TcxRichInnerEdit;
    FTextType: Longint;
    function CalcStreamTextType(AStreamOperation: TcxRichEditStreamOperation; ACustom: Boolean = False;
      ACustomStreamModes: TcxRichEditStreamModes = []): Longint;
    function GetAllowStreamModesByStreamOperation(AStreamOperation: TcxRichEditStreamOperation): TcxRichEditStreamModes;
    function GetStreamModes: TcxRichEditStreamModes;
  protected
    function Get(Index: Integer): string; override;
    procedure InitConverter(const AFileName: string); virtual;
    procedure InitStreamOperation(AStream: TStream;
      var AStreamOperationInfo: TcxRichEditStreamOperationInfo;
      AStreamOperation: TcxRichEditStreamOperation; ACustom: Boolean = False;
      ACustomStreamModes: TcxRichEditStreamModes = []);
    function GetCount: Integer; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure SaveSelectionToStream(Stream: TStream);
    procedure SetUpdateState(Updating: Boolean); override;
    procedure SetTextStr(const Value: string); override;
    function GetLineBreakString: string; virtual;
    property IsModification: Boolean read FIsModification write FIsModification;
    property RichEdit: TcxRichInnerEdit read FRichEdit;
  public
    constructor Create(ARichEdit: TcxRichInnerEdit); virtual;
    destructor Destroy; override;
    procedure Clear; override;
    procedure AddStrings(Strings: TStrings); override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure LoadFromFile(const FileName: string; Encoding: TEncoding); override;
    procedure LoadFromStream(Stream: TStream; Encoding: TEncoding); override;
    procedure SaveToFile(const FileName: string; Encoding: TEncoding); override;
    procedure SaveToStream(Stream: TStream; Encoding: TEncoding); override;
  end;

  { TcxRichInnerEdit }

  TcxRichInnerEdit = class(TRichEdit,
    IcxContainerInnerControl,
    IcxInnerEditHelper,
    IcxPaintControlsHelper)
  private
    FAdvancedTypography: Boolean;
    FAllowObjects: Boolean;
    FAutoSelect: Boolean;
    FAutoURLDetect: Boolean;
    FEchoMode: TcxEditEchoMode;
    FEditValueLockCount: Integer;
    FEscapePressed: Boolean;
    FHelper: TcxRichInnerEditHelper;
    FInternalUpdating: Boolean;
    FIsEraseBackgroundLocked: Boolean;
    FKeyPressProcessed: Boolean;
    FLockBoundsCount: Integer;
    FMemoMode: Boolean;
    FRichEditClass: TcxRichEditClass;
    FRichEditOle: IUnknown;
    FRichEditOleCallback: TObject;
    FRichLines: TcxRichEditStrings;
    FRichVersion: Integer;
    FSavedEditValue: Variant;
    FSavedPlainText: Boolean;
    FScaleFactor: TdxScaleFactor;
    FScrollUIActivityHelper: TdxTouchScrollUIActivityHelper;
    FSelectionBar: Boolean;
    FStreamModes: TcxRichEditStreamModes;
    FURLClickBtn: TMouseButton;
    FURLClickRange: TCharRange;
    FZoomFactor: Double;

    FOnQueryInsertObject: TcxRichEditQueryInsertObjectEvent;

    procedure ApplyZoomFactor;
    procedure CloseOleObjects;
    // IcxContainerInnerControl
    function GetControl: TWinControl;
    function GetControlContainer: TcxContainer;

    // IcxInnerEditHelper
    function GetHelper: IcxCustomInnerEdit;

    function IsEditValueLocked: Boolean;
    procedure RestoreEditValue;
    procedure StoreEditValue;

    function GetAdvancedTypography: Boolean;
    function GetAutoURLDetect: Boolean;
    function GetContainer: TcxCustomRichEdit;
    function GetLineCount: Integer;
    function GetLineIndex(AIndex: Integer): Integer;
    function GetLineLength(AIndex: Integer): Integer;
    function GetRichLines: TcxRichEditStrings;
    function GetRichEditOle: IcxRichEditOle;
    function GetRichEditOleCallBack: TcxRichEditOleCallback;
    function GetTextRange(AStartPos, AEndPos: Longint): string;
    procedure InternalSetMemoMode(AForcedReload: Boolean = False);
    procedure SetAdvancedTypography(Value: Boolean);
    procedure SetAllowObjects(Value: Boolean);
    procedure SetAutoURLDetect(Value: Boolean);
    procedure SetMemoMode(Value: Boolean);
    procedure SetRichEditClass(AValue: TcxRichEditClass);
    procedure SetRichLines(Value: TcxRichEditStrings);
    procedure SetSelectionBar(Value: Boolean);
    procedure SetOleControlActive(AActive: Boolean);
    procedure SetZoomFactor(const Value: Double);
    procedure CMDocWindowActivate(var Message: TMessage); message CM_DOCWINDOWACTIVATE;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMClear(var Message: TMessage); message WM_CLEAR;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMMButtonDown(var Message: TWMMButtonDown); message WM_MBUTTONDOWN;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSetFont(var Message: TWMSetFont); message WM_SETFONT;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure EMReplaceSel(var Message: TMessage); message EM_REPLACESEL;
    procedure EMSetCharFormat(var Message: TMessage); message EM_SETCHARFORMAT;
    procedure EMSetParaFormat(var Message: TMessage); message EM_SETPARAFORMAT;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure WMGetText(var Message: TMessage); message WM_GETTEXT;
    procedure WMGetTextLength(var Message: TWMGetTextLength); message WM_GETTEXTLENGTH;
    procedure WMSetText(var Message: TWMSetText); message WM_SETTEXT;
    procedure WMIMEComposition(var Message: TMessage); message WM_IME_COMPOSITION;
    procedure EMExLineFromChar(var Message: TMessage); message EM_EXLINEFROMCHAR;
    procedure EMLineLength(var Message: TMessage); message EM_LINELENGTH;
    procedure WMPrintClient(var Message: TMessage); message WM_PRINTCLIENT;
  protected
    procedure BeforeInsertObject(var AAllowInsertObject: Boolean; const ACLSID: TCLSID); dynamic;
    procedure Click; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DestroyWindowHandle; override;
    procedure CreateWnd; override;
    procedure DblClick; override;
    procedure DestroyWnd; override;
    procedure DoGetGestureOptions(var Gestures: TInteractiveGestures; var Options: TInteractiveGestureOptions); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    function GetSelText: string; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); dynamic;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure RequestAlign; override;
    procedure RequestSize(const Rect: TRect); override;
    procedure RichCreateParams(var Params: TCreateParams; out ARichVersion: Integer); virtual;
    procedure URLClick(const AURLText: string; AButton: TMouseButton); dynamic;
    procedure URLMove(const AURLText: string); dynamic;
    procedure WndProc(var Message: TMessage); override;
    function CanPaste: Boolean;
    function GetSelection: TCharRange; virtual;

  {$IFDEF DELPHIBERLIN}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
  {$ELSE}
    procedure ChangeScale(M, D: Integer); override;
  {$ENDIF}

    // IcxPaintControlsHelper
    function AllowDrawEdgesAndBorders: Boolean;

    property AdvancedTypography: Boolean read GetAdvancedTypography write SetAdvancedTypography default False;
    property AllowObjects: Boolean read FAllowObjects write SetAllowObjects default True;
    property AutoSelect: Boolean read FAutoSelect write FAutoSelect default False;
    property AutoURLDetect: Boolean read GetAutoURLDetect write SetAutoURLDetect default True;
    property Container: TcxCustomRichEdit read GetContainer;
    property Helper: TcxRichInnerEditHelper read FHelper;
    property MemoMode: Boolean read FMemoMode write SetMemoMode default False;
    property RichEditClass: TcxRichEditClass read FRichEditClass write SetRichEditClass;
    property RichEditOle: IcxRichEditOle read GetRichEditOle;
    property RichEditOleCallBack: TcxRichEditOleCallback read GetRichEditOleCallBack;
    property RichVersion: Integer read FRichVersion write FRichVersion;
    property ScaleFactor: TdxScaleFactor read FScaleFactor;
    property SelectionBar: Boolean read FSelectionBar write SetSelectionBar default False;
    property StreamModes: TcxRichEditStreamModes read FStreamModes write FStreamModes default [];
    property ZoomFactor: Double read FZoomFactor write SetZoomFactor;
    property OnQueryInsertObject: TcxRichEditQueryInsertObjectEvent read FOnQueryInsertObject write FOnQueryInsertObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DefaultHandler(var Message); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function FindTexT(const ASearchStr: string; AStartPos, ALength: Longint; AOptions: TSearchTypes; AForward: Boolean = True): Integer;
    function InsertObject: Boolean;
    function ShowObjectProperties: Boolean;
    function PasteSpecial: Boolean;
    procedure Print(const Caption: string); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function CanFocus: Boolean; override;
    function CanRedo: Boolean; virtual;
    procedure Redo; virtual;
    procedure Undo; virtual;
    property RichLines: TcxRichEditStrings read GetRichLines write SetRichLines;
  end;

function AdjustRichLineBreaks(ADest, ASource: PChar; AShortBreak: Boolean = False): Integer;
function AdjustRichLineBreaksA(ADest, ASource: PAnsiChar; AShortBreak: Boolean = False): Integer;
function AdjustRichLineBreaksW(ADest, ASource: PWideChar; AShortBreak: Boolean = False): Integer;
procedure SetRichEditText(ARichEdit: TRichEdit; const AEditValue: TcxEditValue);

procedure cxDrawRichEdit(ADC: HDC; const ARect: TRect; const AText: string;
  AProperties: TcxCustomRichEditProperties; AFont: TFont; AColor: TColor;
  ATextColor: TColor; ACalculateHeight: Boolean; out AHeight: Integer; AScaleFactor: TdxScaleFactor = nil); overload;
procedure cxDrawRichEdit(ADC: HDC; const ARect: TRect; ARichHandle: HWND;
  AMinCharIndex, AMaxCharIndex: Integer; ACalculateHeight: Boolean; out AHeight: Integer); overload;
procedure cxDrawRichEdit(ACanvas: TCanvas; const ARect: TRect; ARichEdit: TcxRichEdit;
  AMinCharIndex, AMaxCharIndex: Integer; ACalculateHeight: Boolean; out AHeight: Integer); overload;
function dxBitmapToRTF(ABitmap: TBitmap): AnsiString;

implementation

uses
  CommDlg, Printers, Math, Types, ComObj, CommCtrl,
{$IFDEF DELPHI18}
  AnsiStrings,
{$ENDIF}
  dxSpellCheckerCore, cxEditPaintUtils, cxEditUtils, cxVariants, cxDWMAPI, dxUxTheme, dxThemeConsts, dxThemeManager,
  dxCoreGraphics, dxForms, dxDPIAwareUtils;

type
  TStringsAccess = class(TStrings);
  PcxENLink = ^TENLink;

  PcxConversionFormat = ^TcxConversionFormat;

  TcxConversionFormat = record
    ConversionClass: TConversionClass;
    Extension: string;
    Next: PcxConversionFormat;
  end;

  { TcxRichRenderer }

  TcxRichRenderer = class(TcxRichInnerEdit)
  private
    FRequestSizeRect: TRect;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure RequestSize(const Rect: TRect); override;
    procedure CreateWnd; override;
  public
    property RequestSizeRect: TRect read FRequestSizeRect;
  end;

const
  RTFConversionFormat: TcxConversionFormat = (
    ConversionClass: TConversion;
    Extension: 'rtf';
    Next: nil
  );
  TextConversionFormat: TcxConversionFormat = (
    ConversionClass: TConversion;
    Extension: 'txt';
    Next: @RTFConversionFormat
  );
  cxRichReadError = $0001;
  cxRichWriteError = $0002;
  cxRichNoError = $0000;

  cxRichEditZoomMax = 64;
  cxRichEditZoomDenominator = cxRichEditZoomMax * 100;

const
  cxRichEditVersions: array [TcxRichEditClass] of Integer =
    (100, 200, 410);
  cxRichEditClassNames: array [TcxRichEditClass] of string =
    ('RICHEDIT', 'RICHEDIT20W', 'RICHEDIT50W');

const
  // Flags to specify which interfaces should be returned in the structure above
  REO_GETOBJ_NO_INTERFACES  = $00000000;
  REO_GETOBJ_POLEOBJ        = $00000001;
  REO_GETOBJ_PSTG           = $00000002;
  REO_GETOBJ_POLESITE       = $00000004;
  REO_GETOBJ_ALL_INTERFACES = $00000007;

  // Place object at selection
  REO_CP_SELECTION = $FFFFFFFF;

  // Use character position to specify object instead of index
  REO_IOB_SELECTION = $FFFFFFFF;
  REO_IOB_USE_CP    = $FFFFFFFE;

  // Object flags
  REO_NULL            = $00000000;	// No flags
  REO_READWRITEMASK   = $0000003F;	// Mask out RO bits
  REO_DONTNEEDPALETTE = $00000020;	// Object doesn't need palette
  REO_BLANK           = $00000010;	// Object is blank
  REO_DYNAMICSIZE     = $00000008;	// Object defines size always
  REO_INVERTEDSELECT  = $00000004;	// Object drawn all inverted if sel
  REO_BELOWBASELINE   = $00000002;	// Object sits below the baseline
  REO_RESIZABLE       = $00000001;	// Object may be resized
  REO_LINK            = $80000000;	// Object is a link (RO)
  REO_STATIC          = $40000000;	// Object is static (RO)
  REO_SELECTED        = $08000000;	// Object selected (RO)
  REO_OPEN            = $04000000;	// Object open in its server (RO)
  REO_INPLACEACTIVE   = $02000000;	// Object in place active (RO)
  REO_HILITED         = $01000000;	// Object is to be hilited (RO)
  REO_LINKAVAILABLE   = $00800000;	// Link believed available (RO)
  REO_GETMETAFILE     = $00400000;	// Object requires metafile (RO)

  RECO_PASTE          = $00000000;	// paste from clipboard
  RECO_DROP           = $00000001;	// drop
  RECO_COPY           = $00000002;	// copy to the clipboard
  RECO_CUT            = $00000003;	// cut to the clipboard
  RECO_DRAG           = $00000004;	// drag

  cxDataFormatCount = 6;
  cxPasteFormatCount = 6;

var
  FRichEditLibrary: HMODULE = 0;
  FRichRenderer, FRichConverter: TcxRichRenderer;
  FConversionFormatList: PcxConversionFormat = @TextConversionFormat;

  FRichEditDLLNames: TStringDynArray;

  CFObjectDescriptor: Integer;
  CFEmbeddedObject: Integer;
  CFLinkSource: Integer;
  CFRtf: Integer;
  CFRETextObj: Integer;

function AreFontsEqual(AFont1, AFont2: TFont): Boolean;
var
  ALogFont1, ALogFont2: TLogFont;
begin
  Result := AFont1 = AFont2;
  if Result then
    Exit;
  if (AFont1 = nil) or (AFont2 = nil) then
    Exit;
  ZeroMemory(@ALogFont1, SizeOf(TLogFont));
  ZeroMemory(@ALogFont2, SizeOf(TLogFont));
  Result := (AFont1.Color = AFont2.Color) and
    dxGetFontData(AFont1.Handle, ALogFont1) and dxGetFontData(AFont2.Handle, ALogFont2) and
    CompareMem(@ALogFont1, @ALogFont2, SizeOf(TLogFont));
end;

procedure ReleaseObject(var AObj);
begin
  if IUnknown(AObj) <> nil then
    IUnknown(AObj)._Release;
  IUnknown(AObj) := nil;
end;

function cxIsFormMDIChild(AForm: TCustomForm): Boolean;
begin
  Result := (AForm is TForm) and (TForm(AForm).FormStyle = fsMDIChild);
end;

function cxSetDrawAspect(AOleObject: IOleObject; AIconic: Boolean; AIconMetaPict: HGlobal; var ADrawAspect: Cardinal): HResult;
var
  AOleCache: IOleCache;
  AEnumStatData: IEnumStatData;
  AOldAspect: Cardinal;
  AAdviseFlags, AConnection: Longint;
  ATempMetaPict: HGlobal;
  AFormatEtc: TFormatEtc;
  AMedium: TStgMedium;
  AClassID: TCLSID;
  AStatData: TStatData;
  AViewObject: IViewObject;
begin
  AOldAspect := ADrawAspect;
  if AIconic then
  begin
    ADrawAspect := DVASPECT_ICON;
    AAdviseFlags := ADVF_NODATA;
  end
  else
  begin
    ADrawAspect := DVASPECT_CONTENT;
    AAdviseFlags := ADVF_PRIMEFIRST;
  end;

  if (ADrawAspect <> AOldAspect) or (ADrawAspect = DVASPECT_ICON) then
  begin
    AOleCache := AOleObject as IOleCache;
    if ADrawAspect <> AOldAspect then
    begin
      OleCheck(AOleCache.EnumCache(AEnumStatData));
      if AEnumStatData <> nil then
        while AEnumStatData.Next(1, AStatData, nil) = 0 do
          if AStatData.formatetc.dwAspect = Integer(AOldAspect) then
            AOleCache.Uncache(AStatData.dwConnection);
      FillChar(AFormatEtc, SizeOf(FormatEtc), 0);
      AFormatEtc.dwAspect := ADrawAspect;
      AFormatEtc.lIndex := -1;
      OleCheck(AOleCache.Cache(AFormatEtc, AAdviseFlags, AConnection));
      if AOleObject.QueryInterface(IViewObject, AViewObject) = 0 then
        AViewObject.SetAdvise(ADrawAspect, 0, nil);
    end;
    if ADrawAspect = DVASPECT_ICON then
    begin
      ATempMetaPict := 0;
      if AIconMetaPict = 0 then
      begin
        OleCheck(AOleObject.GetUserClassID(AClassID));
        ATempMetaPict := OleGetIconOfClass(AClassID, nil, True);
        AIconMetaPict := ATempMetaPict;
      end;
      try
        with AFormatEtc do
        begin
          cfFormat := CF_METAFILEPICT;
          ptd := nil;
          dwAspect := DVASPECT_ICON;
          lindex := -1;
          tymed := TYMED_MFPICT;
        end;
        with AMedium do
        begin
          tymed := TYMED_MFPICT;
          hMetaFilePict := AIconMetaPict;
          unkForRelease := nil;
        end;
        OleCheck(AOleCache.SetData(AFormatEtc, AMedium, False));
      finally
        DestroyMetaPict(ATempMetaPict);
      end;
    end;
    if ADrawAspect <> DVASPECT_ICON then
      AOleObject.Update;
  end;
  Result := S_OK;
end;

procedure cxCreateStorage(var AStorage: IStorage);
var
  ALockBytes: ILockBytes;
begin
  OleCheck(CreateILockBytesOnHGlobal(0, True, ALockBytes));
  OleCheck(StgCreateDocfileOnILockBytes(ALockBytes, STGM_READWRITE
    or STGM_SHARE_EXCLUSIVE or STGM_CREATE, 0, AStorage));
  ReleaseObject(ALockBytes);
end;

function cxWStrLen(AStr: PWideChar): Integer;
begin
  Result := 0;
  while AStr[Result] <> #0 do Inc(Result);
end;

procedure cxCenterWindow(Wnd: HWnd);
var
  ARect: TRect;
begin
  ARect := cxGetWindowRect(Wnd);
  SetWindowPos(Wnd, 0,
    (GetSystemMetrics(SM_CXSCREEN) - cxRectWidth(ARect)) div 2,
    (GetSystemMetrics(SM_CYSCREEN) - cxRectHeight(ARect)) div 3,
    0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
end;

function cxOleDialogHook(Wnd: HWnd; Msg, WParam, LParam: Longint): Longint; stdcall;
begin
  Result := 0;
  if Msg = WM_INITDIALOG then
  begin
    if IsChildClassWindow(Wnd) then
      Wnd := GetWindowLong(Wnd, GWL_HWNDPARENT);
    cxCenterWindow(Wnd);
    Result := 1;
  end;
end;

function cxOleStdGetFirstMoniker(const AMoniker: IMoniker): IMoniker;
var
  AMksys: Longint;
  AEnumMoniker: IEnumMoniker;
begin
  Result := nil;
  if AMoniker <> nil then
  begin
    if (AMoniker.IsSystemMoniker(AMksys) = 0) and
      (AMksys = MKSYS_GENERICCOMPOSITE) then
    begin
      if AMoniker.Enum(True, AEnumMoniker) <> 0 then Exit;
      AEnumMoniker.Next(1, Result, nil);
    end
    else
      Result := AMoniker;
  end;
end;

function cxOleStdGetLenFilePrefixOfMoniker(const AMoniker: IMoniker): Integer;
var
  AMkFirst: IMoniker;
  ABindCtx: IBindCtx;
  AMksys: Longint;
  P: PWideChar;
begin
  Result := 0;
  if AMoniker <> nil then
  begin
    AMkFirst := cxOleStdGetFirstMoniker(AMoniker);
    if (AMkFirst <> nil) and
      (AMkFirst.IsSystemMoniker(AMksys) = 0) and
      (AMksys = MKSYS_FILEMONIKER) and
      (CreateBindCtx(0, ABindCtx) = 0) and
      (AMkFirst.GetDisplayName(ABindCtx, nil, P) = 0) and (P <> nil) then
    begin
      Result := cxWStrLen(P);
      CoTaskMemFree(P);
    end;
  end;
end;

function cxCoAllocCStr(const S: AnsiString): PAnsiChar; overload;
begin
  Result := dxStrCopy(CoTaskMemAlloc(Length(S) + 1), PAnsiChar(S));
end;

function cxCoAllocCStr(const S: WideString): PChar; overload;
begin
  Result := StrCopy(CoTaskMemAlloc(Length(S) + 1), PChar(S));
end;

function cxGetOleLinkDisplayName(const AOleLink: IOleLink): PChar;
var
  P: PWideChar;
begin
  AOleLink.GetSourceDisplayName(P);
  Result := cxCoAllocCStr(P);
end;

function cxGetOleObjectFullName(const AOleObject: IOleObject): PChar;
var
  P: PWideChar;
begin
  AOleObject.GetUserType(USERCLASSTYPE_FULL, P);
  Result := cxCoAllocCStr(P);
  CoTaskMemFree(P);
end;

function cxGetOleObjectShortName(const AOleObject: IOleObject): PChar;
var
  P: PWideChar;
begin
  AOleObject.GetUserType(USERCLASSTYPE_SHORT, P);
  Result := cxCoAllocCStr(P);
  CoTaskMemFree(P);
end;

function cxGetIconMetaPict(AOleObject: IOleObject; ADrawAspect: Longint): HGlobal;
var
  ADataObject: IDataObject;
  AFormatEtc: TFormatEtc;
  AMedium: TStgMedium;
  AClassID: TCLSID;
begin
  Result := 0;
  if ADrawAspect = DVASPECT_ICON then
  begin
    AOleObject.QueryInterface(IDataObject, ADataObject);
    if ADataObject <> nil then
    begin
      with AFormatEtc do
      begin
        cfFormat := CF_METAFILEPICT;
        ptd := nil;
        dwAspect := DVASPECT_ICON;
        lIndex := -1;
        tymed := TYMED_MFPICT;
      end;
      if Succeeded(ADataObject.GetData(AFormatEtc, AMedium)) then
        Result := AMedium.hMetaFilePict;
      ReleaseObject(ADataObject);
    end;
  end;
  if Result = 0 then
  begin
    OleCheck(AOleObject.GetUserClassID(AClassID));
    Result := OleGetIconOfClass(AClassID, nil, True);
  end;
end;

function cxRichEditGetOleInterface(ARichEdit: TcxRichInnerEdit;
  out AOleInterface: IcxRichEditOle): Boolean;
begin
  Result := SendMessage(ARichEdit.Handle, EM_GETOLEINTERFACE, 0, LPARAM(@AOleInterface)) <> 0;
end;

function cxRichEditSetOleCallback(ARichEdit: TcxRichInnerEdit;
  AOleInterface: IcxRichEditOleCallback): Boolean;
begin
  Result := SendMessage(ARichEdit.Handle, EM_SETOLECALLBACK, 0, LPARAM(AOleInterface)) <> 0;
end;

function cxGetVCLFrameForm(AForm: TCustomForm): IVCLFrameForm;
begin
  if AForm.OleFormObject = nil then
    TOleForm.Create(AForm);
  Result := AForm.OleFormObject as IVCLFrameForm;
end;

function cxRichEditDLLNames: TStringDynArray;

  procedure InitRichEditDLLNames;
  const
    cxRichEditDLLNamesCount = 3;
  begin
    SetLength(FRichEditDLLNames, cxRichEditDLLNamesCount);
    FRichEditDLLNames[0] :=  'Riched32.dll';
    FRichEditDLLNames[1] :=  'Riched20.dll';
    FRichEditDLLNames[2] :=  'Msftedit.dll';
  end;

begin
  if Length(FRichEditDLLNames) = 0 then
    InitRichEditDLLNames;
  Result := FRichEditDLLNames;
end;

function AdjustRichLineBreaksW(ADest, ASource: PWideChar; AShortBreak: Boolean = False): Integer;
var
  APrevDest: PWideChar;
begin
  APrevDest := ADest;
  repeat
    if (ASource^ = WideChar($0D)) or (ASource^ = WideChar($0A)) then
    begin
      if AShortBreak then
        ADest^ := WideChar($0D)
      else
      begin
        ADest^ := WideChar($0D);
        Inc(ADest);
        ADest^ := WideChar($0A);
      end;
      if (ASource^ = WideChar($0D)) and ((ASource + 1)^ = WideChar($0A)) then
        Inc(ASource);
    end
    else
      ADest^ := ASource^;
    Inc(ASource);
    Inc(ADest);
  until ASource^ = WideChar($00);
  ADest^ := WideChar($00);
  Result := ADest - APrevDest;
end;

function AdjustRichLineBreaksA(ADest, ASource: PAnsiChar; AShortBreak: Boolean = False): Integer;
var
  APrevDest: PAnsiChar;
begin
  APrevDest := ADest;
  repeat
    if (ASource^ = AnsiChar($0D)) or (ASource^ = AnsiChar($0A)) then
    begin
      if AShortBreak then
        ADest^ := AnsiChar($0D)
      else
      begin
        ADest^ := AnsiChar($0D);
        Inc(ADest);
        ADest^ := AnsiChar($0A);
      end;
      if (ASource^ = AnsiChar($0D)) and ((ASource + 1)^ = AnsiChar($0A)) then
        Inc(ASource);
    end
    else
      ADest^ := ASource^;
    Inc(ASource);
    Inc(ADest);
  until ASource^ = AnsiChar($00);
  ADest^ := AnsiChar($00);
  Result := ADest - APrevDest;
end;

function AdjustRichLineBreaks(ADest, ASource: PChar; AShortBreak: Boolean = False): Integer;
begin
  Result := AdjustRichLineBreaksW(ADest, ASource, AShortBreak);
end;

function cxRichEditStreamLoad(dwCookie: DWORD_PTR; pbBuff: PByte;
  cb: Longint; var pcb: Longint): Longint; stdcall;
var
  AStreamInfo: TRichEditStreamInfo;
  P, APreamble: TBytes;
begin
  Result := cxRichNoError;
  try
    pcb := 0;
    AStreamInfo := PRichEditStreamInfo(dwCookie)^;
    if AStreamInfo.Converter <> nil then
    begin
      SetLength(P, cb + 1);
      pcb := AStreamInfo.Converter.ConvertReadStream(AStreamInfo.Stream, P, cb);
      if AStreamInfo.PlainText then
      begin
        if AStreamInfo.Encoding = nil then
        begin
          P := TEncoding.Convert(TEncoding.Default, TEncoding.Unicode, P, 0, pcb);
          pcb := Length(P);
        end
        else
        begin
          if not TEncoding.Unicode.Equals(AStreamInfo.Encoding) then
          begin
            P := TEncoding.Convert(AStreamInfo.Encoding, TEncoding.Unicode, P, 0, pcb);
            pcb := Length(P);
          end;
        end;
      end;
      APreamble := TEncoding.Unicode.GetPreamble;
      if (pcb >= 2) and (P[0] = APreamble[0]) and (P[1] = APreamble[1]) then
      begin
        Dec(pcb, 2);
        Move(P[2], pbBuff^, pcb);
      end
      else
        Move(P[0], pbBuff^, pcb);
    end;
  except
    Result := cxRichReadError;
  end;
end;

function cxRichEditStreamSave(dwCookie: DWORD_PTR; pbBuff: PByte; cb: Longint;
  var pcb: Longint): Longint; stdcall;

  function BytesOf(AValue: PAnsiChar): TBytes;
  var
    ALength: Integer;
  begin
    ALength := dxStrLen(AValue) + 1;
    SetLength(Result, ALength);
    Move(AValue^, PAnsiChar(Result)^, ALength);
  end;

var
  AStreamInfo: TRichEditStreamInfo;
  P: TBytes;
begin
  Result := cxRichNoError;
  try
    pcb := 0;
    AStreamInfo := PRichEditStreamInfo(dwCookie)^;
    if AStreamInfo.Converter <> nil then
    begin
      SetLength(P, cb);
      Move(pbBuff^, P[0], cb);
      if AStreamInfo.PlainText then
      begin
        if AStreamInfo.Encoding = nil then
          P := TEncoding.Convert(TEncoding.Unicode, TEncoding.Default, P)
        else
        begin
          if not TEncoding.Unicode.Equals(AStreamInfo.Encoding) then
            P := TEncoding.Convert(TEncoding.Unicode, AStreamInfo.Encoding, P);
        end;
      end;
      pcb := AStreamInfo.Converter.ConvertWriteStream(AStreamInfo.Stream, P, Length(P));
      if (pcb <> cb) and (pcb = Length(P)) then
        pcb := cb;
    end;
  except
    Result := cxRichWriteError;
  end;
end;

function CheckZoomFactor(const AValue: Double): Double;
begin
  Result := Min(Max(AValue, 1 / cxRichEditZoomMax), cxRichEditZoomMax);
end;

procedure ReleaseConversionFormatList;
var
  AConversionFormatList: PcxConversionFormat;
begin
  while FConversionFormatList <> @TextConversionFormat do
  begin
    AConversionFormatList := FConversionFormatList^.Next;
    Dispose(FConversionFormatList);
    FConversionFormatList := AConversionFormatList;
  end;
end;

function CreateInnerRich: TcxRichRenderer;
begin
  Result := TcxRichRenderer.Create(nil);
end;

function RichRenderer: TcxRichRenderer;
begin
  if FRichRenderer = nil then
    FRichRenderer := CreateInnerRich;
  Result := FRichRenderer;
end;

function RichConverter: TcxRichRenderer;
begin
  if FRichConverter = nil then
    FRichConverter := CreateInnerRich;
  Result := FRichConverter;
end;

procedure InternalSetRichEditText(ARichEdit: TRichEdit; const AText: string);
begin
  if not ARichEdit.PlainText then
    dxRichLoadFromString(ARichEdit.Lines, AText)
  else
    ARichEdit.Perform(WM_SETTEXT, 0, LPARAM(PChar(AText)));
end;

function ConvertRichText(const AText: string): string;
begin
  InternalSetRichEditText(RichConverter, AText);
  Result := RichConverter.Text;
end;

procedure SetRichDefAttributes(AEdit: TRichEdit; AFont: TFont; ATextColor: TColor);
begin
  if AEdit.HandleAllocated then
  begin
    AEdit.DefAttributes.Assign(AFont);
    AEdit.DefAttributes.Color := ATextColor;
  end;
end;

procedure InitRichRenderer(AProperties: TcxCustomRichEditProperties; AFont: TFont;
  AColor, ATextColor: TColor; const AText: string; AScaleFactor: TdxScaleFactor = nil);
begin
  if AScaleFactor = nil then
    AScaleFactor := AProperties.ScaleFactor;

  RichRenderer.RichEditClass := TcxCustomRichEditProperties(AProperties).RichEditClass;
  RichRenderer.MemoMode := TcxCustomRichEditProperties(AProperties).MemoMode;
  RichRenderer.PlainText := TcxCustomRichEditProperties(AProperties).PlainText;
  if RichRenderer.Alignment <> TcxCustomRichEditProperties(AProperties).Alignment then
  begin
    RichRenderer.Alignment := TcxCustomRichEditProperties(AProperties).Alignment;
    RichRenderer.Paragraph.Alignment := RichRenderer.Alignment;
  end;
  RichRenderer.AutoURLDetect := TcxCustomRichEditProperties(AProperties).AutoURLDetect;
  RichRenderer.AllowObjects := TcxCustomRichEditProperties(AProperties).AllowObjects;
  RichRenderer.AdvancedTypography := TcxCustomRichEditProperties(AProperties).AdvancedTypography;
  RichRenderer.ZoomFactor := TcxCustomRichEditProperties(AProperties).ZoomFactor;
  RichRenderer.ScaleFactor.Assign(AScaleFactor);
  RichRenderer.HandleNeeded;

  SetRichDefAttributes(RichRenderer, AFont, ATextColor);

  if RichRenderer.MemoMode then
    RichRenderer.Text := AText
  else
    dxRichLoadFromString(RichRenderer.RichLines, AText);

  SendMessage(RichRenderer.Handle, EM_SETBKGNDCOLOR, 0, ColorToRGB(AColor));
end;

procedure cxDrawRichEdit(ADC: HDC; const ARect: TRect; ARichHandle: HWND;
  AMinCharIndex, AMaxCharIndex: Integer; ACalculateHeight: Boolean; out AHeight: Integer);
begin
  dxDrawRichEdit(ADC, cxRectSetNullOrigin(ARect), ARichHandle, AMinCharIndex, AMaxCharIndex, ACalculateHeight, AHeight);
end;

function cxRichEditCalculateBestFit(ADC: HDC; const AText: string;
  AProperties: TcxCustomRichEditProperties; AFont: TFont; AScaleFactor: TdxScaleFactor): Integer;
begin
  RichRenderer.RecreateWnd;
  InitRichRenderer(AProperties, AFont, clNone, clNone, AText, AScaleFactor);
  Result := cxRectWidth(RichRenderer.RequestSizeRect);
end;

procedure cxDrawRichEdit(ADC: HDC; const ARect: TRect; const AText: string;
  AProperties: TcxCustomRichEditProperties; AFont: TFont; AColor: TColor;
  ATextColor: TColor; ACalculateHeight: Boolean; out AHeight: Integer;
  AScaleFactor: TdxScaleFactor = nil); overload;
var
  R: TRect;
  ANotScaledFont: TFont;
begin
  if AScaleFactor = nil then
    AScaleFactor := AProperties.ScaleFactor;

  ANotScaledFont := TFont.Create;
  try
    ANotScaledFont.Assign(AFont);
    ANotScaledFont.Height := dxSystemScaleFactor.Apply(ANotScaledFont.Height, AScaleFactor);

    R := ARect;
    if not AProperties.WordWrap then
      R.Right := cxMaxRectSize;
    if not ACalculateHeight then
      FillRect(ADC, cxRect(cxSize(ARect)), GetSolidBrush(AColor));
    InitRichRenderer(AProperties, ANotScaledFont, AColor, ATextColor, AText, AScaleFactor);
    dxDrawRichEdit(ADC, cxRectSetNullOrigin(R), RichRenderer.Handle, AScaleFactor, 0, -1, ACalculateHeight, AHeight);
  finally
    ANotScaledFont.Free;
  end;
end;

procedure cxDrawRichEdit(ACanvas: TCanvas; const ARect: TRect; ARichEdit: TcxRichEdit;
  AMinCharIndex, AMaxCharIndex: Integer; ACalculateHeight: Boolean; out AHeight: Integer); overload;
begin
  cxDrawRichEdit(ACanvas.Handle, ARect, ARichEdit.InnerControl.Handle, AMinCharIndex, AMaxCharIndex, ACalculateHeight, AHeight);
end;

function dxBitmapToRTF(ABitmap: TBitmap): AnsiString;
var
  ABitmapInfo, ABits: AnsiString;
  AInfoHeaderSize, AImageSize: Cardinal;
begin
  GetDIBSizes(ABitmap.Handle, AInfoHeaderSize, AImageSize);
  SetLength(ABitmapInfo, AInfoHeaderSize);
  SetLength(ABits, AImageSize);
  GetDIB(ABitmap.Handle, ABitmap.Palette, PAnsiChar(ABitmapInfo)^, PAnsiChar(ABits)^);
  Result := '{\rtf1 {\pict\dibitmap0 ' + dxBinToHex(ABitmapInfo + ABits) + ' }}';
end;

procedure SetRichEditText(ARichEdit: TRichEdit; const AEditValue: TcxEditValue);
begin
  InternalSetRichEditText(ARichEdit, VarToStr(AEditValue));
end;

{ TcxRichRenderer }

procedure TcxRichRenderer.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if (Application.MainForm <> nil) and (Application.MainForm.HandleAllocated) then
    Params.WndParent := Application.MainForm.Handle
  else
    Params.WndParent := cxMessageWindow.Handle;
end;

procedure TcxRichRenderer.CreateWnd;
begin
  inherited CreateWnd;
  if HandleAllocated then
    SendMessage(Handle, EM_SETEVENTMASK, 0, ENM_REQUESTRESIZE);
end;

procedure TcxRichRenderer.RequestSize(const Rect: TRect);
begin
  FRequestSizeRect := Rect;
  inherited RequestSize(Rect);
end;

{ TcxRichEdit }

class function TcxRichEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxRichEditProperties;
end;

function TcxRichEdit.GetActiveProperties: TcxRichEditProperties;
begin
  Result := TcxRichEditProperties(InternalGetActiveProperties);
end;

function TcxRichEdit.GetProperties: TcxRichEditProperties;
begin
  Result := TcxRichEditProperties(inherited Properties);
end;

procedure TcxRichEdit.SetProperties(Value: TcxRichEditProperties);
begin
  Properties.Assign(Value);
end;

{ TcxRichInnerEditHelper }

constructor TcxRichInnerEditHelper.Create(AEdit: TcxRichInnerEdit);
begin
  inherited Create(nil);
  FEdit := AEdit;
  FEdit.PlainText := False;
  FEdit.WordWrap := False;
end;

function TcxRichInnerEditHelper.GetControl: TWinControl;
begin
  Result := Edit;
end;

procedure TcxRichInnerEditHelper.LockBounds(ALock: Boolean);
begin
  with Edit do
    if ALock then
      Inc(FLockBoundsCount)
    else
      if FLockBoundsCount > 0 then
        Dec(FLockBoundsCount);
end;

function TcxRichInnerEditHelper.GetOnChange: TNotifyEvent;
begin
  Result := Edit.OnChange;
end;

procedure TcxRichInnerEditHelper.SafelySetFocus;
var
  APrevAutoSelect: Boolean;
begin
  with Edit do
  begin
    APrevAutoSelect := AutoSelect;
    AutoSelect := False;
    SetFocus;
    AutoSelect := APrevAutoSelect;
  end;
end;

function TcxRichInnerEditHelper.CallDefWndProc(AMsg: UINT; WParam: WPARAM;
  LParam: LPARAM): LRESULT;
begin
  Result := CallWindowProc(Edit.DefWndProc, Edit.Handle, AMsg, WParam, LParam);
end;

function TcxRichInnerEditHelper.CanProcessClipboardMessages: Boolean;
begin
  Result := True;
end;

function TcxRichInnerEditHelper.GetEditValue: TcxEditValue;
begin
  with Edit do
    Result := Text;
end;

procedure TcxRichInnerEditHelper.SetEditValue(const Value: TcxEditValue);
var
  AContainer: TcxCustomRichEdit;
begin
  AContainer := Edit.Container;

  if CanAllocateHandle(Edit) and not Edit.HandleAllocated and not Edit.Container.IsLoading then
    Edit.HandleNeeded;

  Edit.Container.LockChangeEvents(True);
  try
    if Edit.HandleAllocated then
      SendMessage(Edit.Handle, WM_SETTEXT, 0, LPARAM(PChar('')));

    if AContainer.ActiveProperties.MemoMode or not Edit.HandleAllocated then
    begin
      Edit.FSavedEditValue := Value;
      Edit.Text := dxVariantToString(Value);
    end
    else
      dxRichLoadFromString(Edit.RichLines, dxVariantToString(Value));
  finally
    Edit.Container.LockChangeEvents(False, not AContainer.PropertiesChange);
  end;
end;

procedure TcxRichInnerEditHelper.SetParent(Value: TWinControl);
begin
  Edit.Parent := Value;
end;

procedure TcxRichInnerEditHelper.SetOnChange(Value: TNotifyEvent);
begin
  Edit.OnChange := Value;
end;

// IcxInnerTextEdit
procedure TcxRichInnerEditHelper.ClearSelection;
begin
  Edit.ClearSelection;
end;

procedure TcxRichInnerEditHelper.CopyToClipboard;
begin
  Edit.CopyToClipboard;
end;

function TcxRichInnerEditHelper.GetAlignment: TAlignment;
begin
  Result := Edit.Alignment;
end;

function TcxRichInnerEditHelper.GetAutoSelect: Boolean;
begin
  Result := Edit.AutoSelect;
end;

function TcxRichInnerEditHelper.GetCharCase: TEditCharCase;
begin
  Result := Edit.CharCase;
end;

function TcxRichInnerEditHelper.GetEchoMode: TcxEditEchoMode;
begin
  Result := eemNormal;
end;

function TcxRichInnerEditHelper.GetFirstVisibleCharIndex: Integer;
var
  R: TRect;
begin
  SendMessage(Edit.Handle, EM_GETRECT, 0, LPARAM(@R));
  Result := SendMessage(Edit.Handle, EM_CHARFROMPOS, 0, LPARAM(@R.TopLeft));
end;

function TcxRichInnerEditHelper.GetHideSelection: Boolean;
begin
  Result := Edit.HideSelection;
end;

function TcxRichInnerEditHelper.GetInternalUpdating: Boolean;
begin
  Result := Edit.FInternalUpdating;
end;

function TcxRichInnerEditHelper.GetMaxLength: Integer;
begin
  Result := Edit.MaxLength;
end;

function TcxRichInnerEditHelper.GetMultiLine: Boolean;
begin
  Result := True;
end;

function TcxRichInnerEditHelper.GetOEMConvert: Boolean;
begin
  Result := Edit.OEMConvert;
end;

function TcxRichInnerEditHelper.GetOnSelChange: TNotifyEvent;
begin
  Result := Edit.OnSelectionChange;
end;

function TcxRichInnerEditHelper.GetPasswordChar: TCaptionChar;
begin
  Result := #0;
end;

function TcxRichInnerEditHelper.GetReadOnly: Boolean;
begin
  Result := Edit.ReadOnly;
end;

function TcxRichInnerEditHelper.GetSelLength: Integer;
begin
  Result := Edit.SelLength;
end;

function TcxRichInnerEditHelper.GetSelStart: Integer;
begin
  Result := Edit.SelStart;
end;

function TcxRichInnerEditHelper.GetSelText: string;
begin
  Result := Edit.SelText;
end;

function TcxRichInnerEditHelper.GetUseLeftAlignmentOnEditing: Boolean;
begin
  Result := False;
end;

procedure TcxRichInnerEditHelper.SelectAll;
begin
  with Edit do
    if HandleAllocated then
      SelectAll;
end;

procedure TcxRichInnerEditHelper.SetAlignment(Value: TAlignment);
begin
  Edit.Alignment := Value;
end;

procedure TcxRichInnerEditHelper.SetAutoSelect(Value: Boolean);
begin
  Edit.AutoSelect := Value;
end;

procedure TcxRichInnerEditHelper.SetCharCase(Value: TEditCharCase);
begin
  Edit.CharCase := Value;
end;

procedure TcxRichInnerEditHelper.SetEchoMode(Value: TcxEditEchoMode);
begin
end;

procedure TcxRichInnerEditHelper.SetHideSelection(Value: Boolean);
begin
  if not Edit.Container.IsInplace then
    Edit.HideSelection := Value;
end;

procedure TcxRichInnerEditHelper.SetInternalUpdating(Value: Boolean);
begin
  Edit.FInternalUpdating := Value;
end;

procedure TcxRichInnerEditHelper.SetImeMode(Value: TImeMode);
begin
  Edit.ImeMode := Value;
end;

procedure TcxRichInnerEditHelper.SetImeName(const Value: TImeName);
begin
  Edit.ImeName := Value;
end;

procedure TcxRichInnerEditHelper.SetMaxLength(Value: Integer);
begin
  Edit.MaxLength := Value;
end;

procedure TcxRichInnerEditHelper.SetOEMConvert(Value: Boolean);
begin
  Edit.OEMConvert := Value;
end;

procedure TcxRichInnerEditHelper.SetOnSelChange(Value: TNotifyEvent);
begin
  Edit.OnSelectionChange := Value;
end;

procedure TcxRichInnerEditHelper.SetPasswordChar(Value: TCaptionChar);
begin
end;

procedure TcxRichInnerEditHelper.SetReadOnly(Value: Boolean);
begin
  Edit.ReadOnly := Value;
end;

procedure TcxRichInnerEditHelper.SetSelLength(Value: Integer);
begin
  Edit.SelLength := Value;
end;

procedure TcxRichInnerEditHelper.SetSelStart(Value: Integer);
begin
  with Edit do
    SelStart := Value;
end;

procedure TcxRichInnerEditHelper.SetSelText(Value: string);
begin
  Edit.SelText := Value;
end;

procedure TcxRichInnerEditHelper.SetUseLeftAlignmentOnEditing(Value: Boolean);
begin
end;

function TcxRichInnerEditHelper.GetImeLastChar: Char;
begin
  Result := #0;
end;

function TcxRichInnerEditHelper.GetImeMode: TImeMode;
begin
  Result := Edit.ImeMode;
end;

function TcxRichInnerEditHelper.GetImeName: TImeName;
begin
  Result := Edit.ImeName;
end;

function TcxRichInnerEditHelper.GetControlContainer: TcxContainer;
begin
  Result := Edit.Container;
end;

// IcxInnerMemo
function TcxRichInnerEditHelper.GetCaretPos: TPoint;
begin
  Result := Edit.CaretPos;
end;

function TcxRichInnerEditHelper.GetLines: TStrings;
begin
  Result := Edit.Lines;
end;

function TcxRichInnerEditHelper.GetScrollBars: TcxScrollStyle;
begin
  Result := Edit.ScrollBars;
end;

function TcxRichInnerEditHelper.GetWantReturns: Boolean;
begin
  Result := Edit.WantReturns;
end;

function TcxRichInnerEditHelper.GetWantTabs: Boolean;
begin
  Result := Edit.WantTabs;
end;

function TcxRichInnerEditHelper.GetWordWrap: Boolean;
begin
  Result := Edit.WordWrap;
end;

function TcxRichInnerEditHelper.GetTextHint: string;
begin
  Result := Edit.TextHint;
end;

procedure TcxRichInnerEditHelper.SetTextHint(Value: string);
begin
  Edit.TextHint := Value;
end;

procedure TcxRichInnerEditHelper.SetCaretPos(const Value: TPoint);
begin
  SetMemoCaretPos(Edit, Value);
end;

procedure TcxRichInnerEditHelper.SetScrollBars(Value: TcxScrollStyle);
begin
  Edit.ScrollBars := Value;
end;

procedure TcxRichInnerEditHelper.SetWantReturns(Value: Boolean);
begin
  Edit.WantReturns := Value;
end;

procedure TcxRichInnerEditHelper.SetWantTabs(Value: Boolean);
begin
  Edit.WantTabs := Value;
end;

procedure TcxRichInnerEditHelper.SetWordWrap(Value: Boolean);
begin
  Edit.WordWrap := Value;
end;

{ TcxRichEditStrings }

constructor TcxRichEditStrings.Create(ARichEdit: TcxRichInnerEdit);
begin
  inherited Create;
  FRichEdit := ARichEdit;
  FTextType := SF_TEXT;
end;

destructor TcxRichEditStrings.Destroy;
begin
  FreeAndNil(FConverter);
  inherited Destroy;
end;

procedure TcxRichEditStrings.Clear;
begin
  if Count > 0 then
    RichEdit.Lines.Clear;
end;

function TcxRichEditStrings.CalcStreamTextType(AStreamOperation: TcxRichEditStreamOperation; ACustom: Boolean;
  ACustomStreamModes: TcxRichEditStreamModes): Longint;
var
  AStreamModes, AAllowStreamModes: TcxRichEditStreamModes;
begin
  if ACustom then
    AStreamModes := ACustomStreamModes
  else
    AStreamModes := GetStreamModes;
  AAllowStreamModes := GetAllowStreamModesByStreamOperation(AStreamOperation);
  if RichEdit.MemoMode or RichEdit.PlainText then
  begin
    Result := SF_TEXT or SF_UNICODE;
    if (resmUnicode in AStreamModes) and (resmUnicode in AAllowStreamModes) then
      Result := Result or SF_UNICODE;
    if (resmTextIzed in AStreamModes) and (resmTextIzed in AAllowStreamModes) then
      Result := SF_TEXTIZED;
  end
  else
  begin
    Result := SF_RTF;
    if (resmRtfNoObjs in AStreamModes) and (resmRtfNoObjs in AAllowStreamModes) then
      Result := SF_RTFNOOBJS;
    if (resmPlainRtf in AStreamModes) and (resmPlainRtf in AAllowStreamModes) then
      Result := Result or SFF_PLAINRTF;
  end;
  if (resmSelection in AStreamModes) and (resmSelection in AAllowStreamModes) and not RichEdit.IsEditValueLocked then
    Result := Result or SFF_SELECTION;
end;

function TcxRichEditStrings.GetAllowStreamModesByStreamOperation(AStreamOperation: TcxRichEditStreamOperation): TcxRichEditStreamModes;
const
  AResultMap: array[Boolean] of TcxRichEditStreamModes = ([resmSelection, resmPlainRtf, resmRtfNoObjs, resmUnicode, resmTextIzed],
    [resmSelection, resmPlainRtf, resmUnicode]);
begin
  Result := AResultMap[AStreamOperation = esoSaveTo];
end;

function TcxRichEditStrings.GetStreamModes: TcxRichEditStreamModes;
begin
  Result := FRichEdit.StreamModes;
end;

procedure TcxRichEditStrings.AddStrings(Strings: TStrings);
var
  APrevSelectionChange: TNotifyEvent;
begin
  APrevSelectionChange := RichEdit.OnSelectionChange;
  RichEdit.OnSelectionChange := nil;
  try
    inherited AddStrings(Strings);
  finally
    RichEdit.OnSelectionChange := APrevSelectionChange;
  end;
end;

procedure TcxRichEditStrings.Delete(Index: Integer);
begin
  FRichEdit.Lines.Delete(Index);
end;

procedure TcxRichEditStrings.Insert(Index: Integer; const S: string);
var
  AFormat: string;
  AStr: PChar;
  ASelection: TCharRange;
begin
  if (Index < 0) or (Index > Count) then
    Exit;
  IsModification := True;
  try
    ASelection.cpMin := FRichEdit.GetLineIndex(Index);
    if ASelection.cpMin < 0 then
    begin
      ASelection.cpMin := FRichEdit.GetLineIndex(Index - 1);
      if ASelection.cpMin < 0 then
        ASelection.cpMin := 0
      else
        ASelection.cpMin := ASelection.cpMin + FRichEdit.GetLineLength(Index - 1);
      AFormat := GetLineBreakString + '%s';
    end
    else
      AFormat := '%s'+ GetLineBreakString;
    ASelection.cpMax := ASelection.cpMin;
    AStr := PChar(Format(AFormat, [S]));
    cxSendStructMessage(FRichEdit.Handle, EM_EXSETSEL, 0, ASelection);
    AdjustRichLineBreaks(AStr, PChar(Format(AFormat, [S])), Length(GetLineBreakString) = 1);
    SendMessage(FRichEdit.Handle, EM_REPLACESEL, 0, LPARAM(AStr));
    if FRichEdit.SelStart <> (ASelection.cpMax + Length(AStr)) then
      raise EOutOfResources.Create(
        cxGetResourceString(@cxSEditRichEditLineInsertionError));
  finally
    IsModification := False;
  end;
end;

procedure TcxRichEditStrings.LoadFromFile(const FileName: string; Encoding: TEncoding);
begin
  InitConverter(FileName);
  inherited LoadFromFile(FileName, Encoding);
  FRichEdit.Container.EditModified := False
end;

procedure TcxRichEditStrings.LoadFromStream(Stream: TStream; Encoding: TEncoding);
var
  APos: Longint;
  AStreamOperationInfo: TcxRichEditStreamOperationInfo;
  AOldReadOnly: Boolean;
begin
  APos := Stream.Position;
  try
    AOldReadOnly := RichEdit.ReadOnly;
    if IsWin10OrLater then
      RichEdit.ReadOnly := False;
    try
      AStreamOperationInfo.StreamInfo.Encoding := Encoding;
      InitStreamOperation(Stream, AStreamOperationInfo, esoLoadFrom);
      with AStreamOperationInfo do
      begin
        cxSendStructMessage(RichEdit.Handle, EM_STREAMIN, TextType, EditStream);
        if ((TextType and SF_RTF) = SF_RTF) and (EditStream.dwError <> 0) then
        begin
          Stream.Position := APos;
          TextType := TextType and not SF_RTF;
          if StreamInfo.PlainText then
            TextType := TextType or SF_RTF
          else
            TextType := TextType or SF_TEXT or SF_UNICODE;
          StreamInfo.PlainText := not StreamInfo.PlainText;
          cxSendStructMessage(RichEdit.Handle, EM_STREAMIN, TextType, EditStream);
        end;
        if EditStream.dwError <> 0 then
          raise EOutOfResources.Create(cxGetResourceString(@cxSEditRichEditLoadFail));
        FTextType := TextType;
      end;
    finally
      if IsWin10OrLater then
        RichEdit.ReadOnly := AOldReadOnly;
    end;
  finally
    if FConverter = nil then
      FreeAndNil(AStreamOperationInfo.StreamInfo.Converter);
  end;

  with FRichEdit do
  begin
    ApplyZoomFactor;
    if Container <> nil then
      Container.EditModified := False;
  end;
end;

procedure TcxRichEditStrings.SaveToFile(const FileName: string; Encoding: TEncoding);
begin
  InitConverter(FileName);
  inherited SaveToFile(FileName, Encoding);
end;

procedure TcxRichEditStrings.SaveToStream(Stream: TStream; Encoding: TEncoding);
var
  AStreamOperationInfo: TcxRichEditStreamOperationInfo;
  APreamble: TBytes;
begin
  try
    AStreamOperationInfo.StreamInfo.Encoding := Encoding;
    InitStreamOperation(Stream, AStreamOperationInfo, esoSaveTo);
    if (Encoding <> nil) and RichEdit.PlainText then
    begin
      APreamble := Encoding.GetPreamble;
      if Length(APreamble) > 0 then
        Stream.WriteBuffer(APreamble[0], Length(APreamble));
    end;
    with AStreamOperationInfo do
    begin
      cxSendStructMessage(RichEdit.Handle, EM_STREAMOUT, TextType, EditStream);
      if EditStream.dwError <> 0 then
        raise EOutOfResources.Create(cxGetResourceString(@cxSEditRichEditSaveFail));
    end;
  finally
    if FConverter = nil then
      FreeAndNil(AStreamOperationInfo.StreamInfo.Converter);
  end;
end;

function TcxRichEditStrings.Get(Index: Integer): string;
var
  Text: array[0..4095] of Char;
begin
  cxZeroMemory(@Text, Length(Text));
  Word((@Text)^) := Length(Text);

  if SendMessage(RichEdit.Handle, EM_GETLINE, Index, LPARAM(@Text)) > 0 then
  begin
    Result := Text;
    while (Length(Result) > 0) and dxCharInSet(Result[Length(Result)], [#10, #13]) do
      System.Delete(Result, Length(Result), 1);
  end
  else
    Result := '';
end;

procedure TcxRichEditStrings.InitConverter(const AFileName: string);
var
  AExtension: string;
  AConversionFormat: PcxConversionFormat;
begin
{$IFDEF DELPHI15}
  AExtension := AnsiLowerCase(ExtractFileExt(AFilename));
{$ELSE}
  AExtension := AnsiLowerCaseFileName(ExtractFileExt(AFilename));
{$ENDIF}
  System.Delete(AExtension, 1, 1);
  AConversionFormat := FConversionFormatList;
  while AConversionFormat <> nil do
    with AConversionFormat^ do
      if Extension <> AExtension then AConversionFormat := Next
      else Break;
  if AConversionFormat = nil then
    AConversionFormat := @TextConversionFormat;
  if (FConverter = nil) or
    (FConverter.ClassType <> AConversionFormat^.ConversionClass) then
  begin
    FreeAndNil(FConverter);
    FConverter := AConversionFormat^.ConversionClass.Create;
  end;
end;

procedure TcxRichEditStrings.InitStreamOperation(AStream: TStream;
  var AStreamOperationInfo: TcxRichEditStreamOperationInfo;
  AStreamOperation: TcxRichEditStreamOperation; ACustom: Boolean;
      ACustomStreamModes: TcxRichEditStreamModes);

  function ContainsPreamble(AStream: TStream; ASignature: TBytes): Boolean;
  var
    ABuffer: TBytes;
    I, ALBufLen, ALSignatureLen, ALPosition: Integer;
  begin
    Result := True;
    ALSignatureLen := Length(ASignature);
    ALPosition := AStream.Position;
    try
      SetLength(ABuffer, ALSignatureLen);
      ALBufLen := AStream.Read(ABuffer[0], ALSignatureLen);
    finally
      AStream.Position := ALPosition;
    end;

    if ALBufLen = ALSignatureLen then
    begin
      for I := 1 to ALSignatureLen do
        if ABuffer[I - 1] <> ASignature [I - 1] then
        begin
          Result := False;
          Break;
        end;
    end
    else
      Result := False;
  end;

  function GetEncoding: TEncoding;
  begin
    if ContainsPreamble(AStream, TEncoding.Unicode.GetPreamble) then
      Result := TEncoding.Unicode
    else
      if ContainsPreamble(AStream, TEncoding.BigEndianUnicode.GetPreamble) then
        Result := TEncoding.BigEndianUnicode
      else
        if ContainsPreamble(AStream, TEncoding.UTF8.GetPreamble) then
          Result := TEncoding.UTF8
        else
          Result := TEncoding.Default;
  end;

var
  AConverter: TConversion;
begin
  if FConverter <> nil then
    AConverter := FConverter
  else
    AConverter := RichEdit.DefaultConverter.Create;
  with AStreamOperationInfo do
  begin
    if (StreamInfo.Encoding = nil) and (AStreamOperation = esoLoadFrom) then
      StreamInfo.Encoding := GetEncoding;
    StreamInfo.PlainText := RichEdit.PlainText;
    StreamInfo.Converter := AConverter;
    StreamInfo.Stream := AStream;
    EditStream.dwCookie := DWORD_PTR(@StreamInfo);
    EditStream.dwError := 0;
    if AStreamOperation = esoLoadFrom then
      EditStream.pfnCallBack := @cxRichEditStreamLoad
    else
      EditStream.pfnCallBack := @cxRichEditStreamSave;
    TextType := CalcStreamTextType(AStreamOperation, ACustom, ACustomStreamModes);
  end;
end;

function TcxRichEditStrings.GetCount: Integer;
begin
  Result := RichEdit.GetLineCount;
  if (Result > 0) and (RichEdit.GetLineLength(Result - 1) = 0) then
    Dec(Result);
end;

procedure TcxRichEditStrings.Put(Index: Integer; const S: string);
begin
  TStringsAccess(FRichEdit.Lines).Put(Index, S);
end;

procedure TcxRichEditStrings.SaveSelectionToStream(Stream: TStream);
var
  AStreamOperationInfo: TcxRichEditStreamOperationInfo;
  ACustomStreamModes: TcxRichEditStreamModes;
begin
  ACustomStreamModes := [resmSelection];
  InitStreamOperation(Stream,
    AStreamOperationInfo, esoSaveTo, True, ACustomStreamModes);
  try
    with AStreamOperationInfo do
    begin
      cxSendStructMessage(RichEdit.Handle, EM_STREAMOUT, TextType, EditStream);
      if EditStream.dwError <> 0 then
        raise EOutOfResources.Create(cxGetResourceString(@cxSEditRichEditSelectionSaveFail));
    end;
  finally
    if FConverter = nil then
      FreeAndNil(AStreamOperationInfo.StreamInfo.Converter);
  end;
end;

procedure TcxRichEditStrings.SetUpdateState(Updating: Boolean);
begin
  TStringsAccess(FRichEdit.Lines).SetUpdateState(Updating);
end;

procedure TcxRichEditStrings.SetTextStr(const Value: string);
begin
  FRichEdit.Container.Text := Value;
end;

function TcxRichEditStrings.GetLineBreakString: string;
begin
  if FRichEdit.RichVersion >= 200 then
    Result := #13
  else
    Result := #13#10
end;

{ TcxRichInnerEdit }

constructor TcxRichInnerEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSavedEditValue := Null;
  ParentColor := True;
  FAllowObjects := False;
  FAutoURLDetect := False;
  FEchoMode := eemNormal;
  FScaleFactor := TdxScaleFactor.Create;
  FHelper := TcxRichInnerEditHelper.Create(Self);
  FScrollUIActivityHelper := TdxTouchScrollUIActivityHelper.Create;
  FInternalUpdating := False;
  FRichLines := TcxRichEditStrings.Create(Self);
  FStreamModes := [];
  FRichEditOleCallback := TcxRichEditOleCallback.Create(Self);
  FRichEditClass := recRichEdit41;
  FZoomFactor := 1;
end;

destructor TcxRichInnerEdit.Destroy;
begin
  CloseOleObjects;
  FRichEditOle := nil;
  FreeAndNil(FScaleFactor);
  FreeAndNil(FRichLines);
  FreeAndNil(FScrollUIActivityHelper);
  FreeAndNil(FHelper);
  inherited Destroy;
  FreeAndNil(FRichEditOleCallBack);
end;

procedure TcxRichInnerEdit.DefaultHandler(var Message);
begin
  if (Container = nil) or not Container.InnerControlDefaultHandler(TMessage(Message)) then
    inherited DefaultHandler(Message);
end;

procedure TcxRichInnerEdit.DragDrop(Source: TObject; X, Y: Integer);
begin
  Container.DragDrop(Source, Left + X, Top + Y);
end;

function TcxRichInnerEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (Container <> nil) and Container.DataBinding.ExecuteAction(Action);
end;

function TcxRichInnerEdit.FindText(const ASearchStr: string;
  AStartPos, ALength: Longint; AOptions: TSearchTypes; AForward: Boolean = True): Integer;

  function GetFindTextFlags: Integer;
  begin
    Result := IfThen(AForward, FR_Down) or
      IfThen(stWholeWord in AOptions, FR_WHOLEWORD) or
      IfThen(stMatchCase in AOptions, FR_MATCHCASE);
  end;

  procedure PrepareCharRange(var ACharRange: TCharRange);
  begin
    ACharRange.cpMin := AStartPos;
    ACharRange.cpMax := AStartPos;
    if AForward then
      ACharRange.cpMax := ACharRange.cpMax + ALength
    else
      ACharRange.cpMax := ACharRange.cpMax - ALength;
  end;

var
  AFindText: TFindText;
begin
  PrepareCharRange(AFindText.Chrg);
  AFindText.lpstrText := PChar(ASearchStr);
  Result := cxSendStructMessage(Handle, EM_FINDTEXT, GetFindTextFlags, AFindText);
end;

function TcxRichInnerEdit.InsertObject: Boolean;
var
  AData: TOleUIInsertObjectA;
  ANameBuffer: array[0..255] of AnsiChar;
  AOleClientSite: IOleClientSite;
  AStorage: IStorage;
  AReObject: TReObject;
  AOleObject: IOleObject;
  ASelection: TCharRange;
  AIsNewObject: Boolean;
begin
  Result := False;
  if not FAllowObjects or not Assigned(FRichEditOle) then
    Exit;
  FillChar(AData, SizeOf(AData), 0);
  FillChar(ANameBuffer, SizeOf(ANameBuffer), 0);
  AStorage := nil;
  try
    cxCreateStorage(AStorage);
    RichEditOle.GetClientSite(AOleClientSite);
    with AData do
    begin
      cbStruct := SizeOf(AData);
      dwFlags := IOF_SELECTCREATENEW or IOF_VERIFYSERVERSEXIST or
        IOF_CREATENEWOBJECT or IOF_CREATEFILEOBJECT or IOF_CREATELINKOBJECT;
      hWndOwner := Handle;
      lpfnHook := cxOleDialogHook;
      lpszFile := ANameBuffer;
      cchFile := SizeOf(ANameBuffer);
      oleRender := OLERENDER_DRAW;
      iid := IOleObject;
      lpIOleClientSite := AOleClientSite;
      lpIStorage := AStorage;
      ppvObj := @AOleObject;
    end;
    if OleUIInsertObjectA(AData) = OLEUI_OK then
      try
        AIsNewObject := AData.dwFlags and IOF_SELECTCREATENEW = IOF_SELECTCREATENEW;
        FillChar(AReObject, SizeOf(AReObject), 0);
        with AReObject do
        begin
          cbStruct := SizeOf(AReObject);
          cp := REO_CP_SELECTION;
          clsid := AData.clsid;
          oleobj := AOleObject;
          stg := AStorage;
          olesite := AOleClientSite;
          dvaspect := DVASPECT_CONTENT;
          dwFlags := REO_RESIZABLE;
          if AIsNewObject then
            dwFlags := dwFlags or REO_BLANK;
          OleCheck(cxSetDrawAspect(AOleObject, AData.dwFlags and IOF_CHECKDISPLAYASICON <> 0,
            AData.hMetaPict, dvaspect));
        end;
        if HandleAllocated then
        begin
          SendMessage(Handle, EM_EXGETSEL, 0, LPARAM(@ASelection));
          ASelection.cpMax := ASelection.cpMin + 1;
        end;
        if Succeeded(RichEditOle.InsertObject(AReObject)) then
        begin
          if HandleAllocated then
          begin
            SendMessage(Handle, EM_EXSETSEL, 0, LPARAM(@ASelection));
            SendMessage(Handle, EM_SCROLLCARET, 0, 0);
          end;
          RichEditOle.SetDvaspect(Longint(REO_IOB_SELECTION), AReObject.dvaspect);
          if AIsNewObject then OleCheck(AReObject.oleobj.DoVerb(OLEIVERB_SHOW, nil,
            AOleClientSite, 0, Handle, ClientRect));
          Result := True;
        end;
      finally
        DestroyMetaPict(AData.hMetaPict);
        ReleaseObject(AOleObject);
        ZeroMemory(@AReObject,SizeOf(AReObject));
      end;
  finally
    ZeroMemory(@AData,SizeOf(AData));
  end;
end;

function TcxRichInnerEdit.ShowObjectProperties: Boolean;
var
  AObjectProps: TOleUIObjectProps;
  APropSheet: TPropSheetHeader;
  AGeneralProps: TOleUIGnrlProps;
  AViewProps: TOleUIViewProps;
  ALinkProps: TOleUILinkProps;
  ADialogCaption: string;
  AReObject: TReObject;
begin
  Result := False;
  if not Assigned(FRichEditOle) or
      (RichEditOle.GetObjectCount <= 0) then
    Exit;
  if HandleAllocated and
      not (SendMessage(Handle, EM_SELECTIONTYPE, 0, 0) in [SEL_OBJECT, SEL_MULTIOBJECT]) then
    Exit;
  FillChar(AObjectProps, SizeOf(AObjectProps), 0);
  FillChar(APropSheet, SizeOf(APropSheet), 0);
  FillChar(AGeneralProps, SizeOf(AGeneralProps), 0);
  FillChar(AViewProps, SizeOf(AViewProps), 0);
  FillChar(ALinkProps, SizeOf(ALinkProps), 0);
  AReObject.cbStruct := SizeOf(AReObject);
  OleCheck(RichEditOle.GetObject(Longint(REO_IOB_SELECTION), AReObject, REO_GETOBJ_POLEOBJ or
    REO_GETOBJ_POLESITE or REO_GETOBJ_PSTG));
  with AObjectProps do
  begin
    cbStruct := SizeOf(AObjectProps);
    dwFlags := 0;
    lpPS := @APropSheet;
    lpObjInfo := TcxOleUIObjInfo.Create(Self, AReObject);
    if (AReObject.dwFlags and REO_LINK) <> 0 then
    begin
      dwFlags := AObjectProps.dwFlags or OPF_OBJECTISLINK;
      lpLinkInfo := TcxOleUILinkInfo.Create(Self, AReObject);
    end;
    lpGP := @AGeneralProps;
    lpVP := @AViewProps;
    lpLP := @ALinkProps;
  end;
  with APropSheet do
  begin
    dwSize := SizeOf(APropSheet);
    hWndParent := Application.Handle;
    hInstance := MainInstance;
    ADialogCaption := Format(SPropDlgCaption, [cxGetOleObjectFullName(AReObject.oleobj)]);
    pszCaption := PChar(ADialogCaption);
   end;
  AGeneralProps.cbStruct := SizeOf(AGeneralProps);
  AGeneralProps.lpfnHook := cxOleDialogHook;
  with AViewProps do
  begin
    cbStruct := SizeOf(AViewProps);
    dwFlags := VPF_DISABLESCALE;
  end;
  ALinkProps.cbStruct := SizeOf(ALinkProps);
  ALinkProps.dwFlags := ELF_DISABLECANCELLINK;
  Result := Container.CanModify and Container.DoEditing and
    (OleUIObjectProperties(AObjectProps) = OLEUI_OK);
  ZeroMemory(@AObjectProps, SizeOf(AObjectProps));
  ZeroMemory(@APropSheet, SizeOf(APropSheet));
  ZeroMemory(@AGeneralProps, SizeOf(AGeneralProps));
  ZeroMemory(@AViewProps, SizeOf(AViewProps));
  ZeroMemory(@ALinkProps, SizeOf(ALinkProps));
  ZeroMemory(@AReObject, SizeOf(AReObject));
end;

function TcxRichInnerEdit.PasteSpecial: Boolean;

  procedure SetPasteFormats(var APasteFormat: TOleUIPasteEntry; AFormat: TClipFormat;
    Atymed: DWORD; const AFormatName, AResultText: string; AFlags: DWORD);
  begin
    with APasteFormat do begin
      fmtetc.cfFormat := AFormat;
      fmtetc.dwAspect := DVASPECT_CONTENT;
      fmtetc.lIndex := -1;
      fmtetc.tymed := Atymed;
      if AFormatName <> '' then
        lpstrFormatName := PChar(AFormatName)
      else
        lpstrFormatName := '%s';
      if AResultText <> '' then
        lpstrResultText := PChar(AResultText)
      else
        lpstrResultText := '%s';
      dwFlags := AFlags;
    end;
  end;

var
  AData: TOleUIPasteSpecial;
  APasteFormats: array[0..cxPasteFormatCount - 1] of TOleUIPasteEntry;
  AFormat: Integer;
  AReObject: TReObject;
  AClientSite: IOleClientSite;
  AStorage: IStorage;
  AOleObject: IOleObject;
  ASelection: TCharRange;
begin
  Result := False;
  if not CanPaste then Exit;
  if not Assigned(FRichEditOle) then Exit;
  FillChar(AData, SizeOf(AData), 0);
  FillChar(APasteFormats, SizeOf(APasteFormats), 0);
  with AData do
  begin
    cbStruct := SizeOf(AData);
    dwFlags := PSF_SELECTPASTE;
    hWndOwner := Application.Handle;
    lpfnHook := cxOleDialogHook;
    arrPasteEntries := @APasteFormats;
    cPasteEntries := cxPasteFormatCount;
    arrLinkTypes := @CFLinkSource;
    cLinkTypes := 1;
  end;
  SetPasteFormats(APasteFormats[0], CFEmbeddedObject, TYMED_ISTORAGE,
    '%s', '%s', OLEUIPASTE_PASTE or OLEUIPASTE_ENABLEICON);
  SetPasteFormats(APasteFormats[1], CFLinkSource, TYMED_ISTREAM,
    '%s', '%s', OLEUIPASTE_LINKTYPE1 or OLEUIPASTE_ENABLEICON);
  SetPasteFormats(APasteFormats[2], CF_BITMAP, TYMED_GDI,
    'Windows bitmap', 'bitmap image', OLEUIPASTE_PASTE);
  SetPasteFormats(APasteFormats[3], CFRtf, TYMED_ISTORAGE,
    CF_RTF, CF_RTF, OLEUIPASTE_PASTE);
  SetPasteFormats(APasteFormats[4], CF_TEXT, TYMED_HGLOBAL,
    'Unformatted text', 'text without any formatting', OLEUIPASTE_PASTE);
  SetPasteFormats(APasteFormats[5], CFRETextObj, TYMED_ISTORAGE,
    CF_RETEXTOBJ, CF_RETEXTOBJ, OLEUIPASTE_PASTE);
  try
    if OleUIPasteSpecial(AData) = OLEUI_OK then
    begin
      if AData.nSelectedIndex in [0, 1] then // CFEmbeddedObject, CFLinkSource
      begin
        FillChar(AReObject, SizeOf(AReObject), 0);
        RichEditOle.GetClientSite(AClientSite);
        cxCreateStorage(AStorage);
        try
          case AData.nSelectedIndex of
            0: {CFEmbeddedObject}
              OleCheck(OleCreateFromData(AData.lpSrcDataObj, IOleObject,
               OLERENDER_DRAW, nil, AClientSite, AStorage, AOleObject));
            1: {CFLinkSource}
              OleCheck(OleCreateLinkFromData(AData.lpSrcDataObj, IOleObject,
               OLERENDER_DRAW, nil, AClientSite, AStorage, AOleObject));
          end;
          try
            with AReObject do
            begin
              cbStruct := SizeOf(AReObject);
              cp := REO_CP_SELECTION;
              oleobj := AOleObject;
              AOleObject.GetUserClassID(clsid);
              stg := AStorage;
              olesite := AClientSite;
              dvaspect := DVASPECT_CONTENT;
              dwFlags := REO_RESIZABLE;
              OleCheck(cxSetDrawAspect(oleobj,
                AData.dwFlags and PSF_CHECKDISPLAYASICON <> 0,
                AData.hMetaPict, dvaspect));
            end;
            SendMessage(Handle, EM_EXGETSEL, 0, LPARAM(@ASelection));
            ASelection.cpMax := ASelection.cpMin + 1;
            if Succeeded(RichEditOle.InsertObject(AReObject)) then
            begin
              SendMessage(Handle, EM_EXSETSEL, 0, LPARAM(@ASelection));
              OleCheck(RichEditOle.SetDvaspect(Longint(REO_IOB_SELECTION), AReObject.dvaspect));
            end;
          finally
            ZeroMemory(@AReObject, SizeOf(AReObject));
          end;
        finally
          ReleaseObject(AClientSite);
          ReleaseObject(AStorage);
        end;
      end
      else
      begin
        AFormat := APasteFormats[AData.nSelectedIndex].fmtetc.cfFormat;
        if not Succeeded(RichEditOle.ImportDataObject(AData.lpSrcDataObj,
            AFormat, AData.hMetaPict)) then
          Exit;
      end;
      Result := True;
    end;
  finally
    DestroyMetaPict(AData.hMetaPict);
    ReleaseObject(AData.lpSrcDataObj);
    ZeroMemory(@AData, SizeOf(AData));
  end;
end;

procedure TcxRichInnerEdit.Print(const Caption: string);
begin
  inherited;
end;

procedure TcxRichInnerEdit.BeforeInsertObject(var AAllowInsertObject: Boolean;
  const ACLSID: TCLSID);
begin
  if Assigned(OnQueryInsertObject) then
    OnQueryInsertObject(Container, AAllowInsertObject, ACLSID);
end;

procedure TcxRichInnerEdit.Click;
begin
  inherited Click;
  _TcxContainerAccess.Click(Container);
end;

procedure TcxRichInnerEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  RichCreateParams(Params, FRichVersion);
  with Params.WindowClass do
    style := style or CS_VREDRAW or CS_HREDRAW;
  if SelectionBar then
    Params.Style := Params.Style or ES_SELECTIONBAR;
  Params.Style := Params.Style or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
end;

procedure TcxRichInnerEdit.CreateWindowHandle(const Params: TCreateParams);
const
  AdvancedTypographyMap: array[Boolean] of DWORD = (0, TO_ADVANCEDTYPOGRAPHY);
begin
  inherited CreateWindowHandle(Params);
  if HandleAllocated then
  begin
    if FAllowObjects then
    begin
      if not cxRichEditGetOleInterface(Self, IcxRichEditOle(FRichEditOle)) then
        raise EOutOfResources.Create(cxGetResourceString(@cxSEditRichEditOleInterfaceFail));
    end;
    if not cxRichEditSetOleCallback(Self, RichEditOlecallback) then
      raise EOutOfResources.Create(cxGetResourceString(@cxSEditRichEditCallBackFail));
    SendMessage(Handle, EM_AUTOURLDETECT, WPARAM(FAutoURLDetect and not MemoMode and ((Container = nil) or not Container.IsDesigning)), 0);
    SendMessage(Handle, EM_SETTYPOGRAPHYOPTIONS, WPARAM(AdvancedTypographyMap[FAdvancedTypography]), TO_ADVANCEDTYPOGRAPHY);
  end;
end;

procedure TcxRichInnerEdit.DestroyWindowHandle;
begin
  SetOleControlActive(False);
  CloseOleObjects;
  FRichEditOle := nil;
  inherited DestroyWindowHandle;
end;

procedure TcxRichInnerEdit.CreateWnd;
begin
  if Container <> nil then
  begin
    Alignment := Container.ActiveProperties.Alignment;
    Container.ClearSavedChildControlRegions;
    PlainText := FSavedPlainText;
  end;
  inherited CreateWnd;
  if HandleAllocated then
  begin
    if Container <> nil then
    begin
      PlainText := Container.ActiveProperties.PlainText or Container.ActiveProperties.MemoMode;
      Container.AdjustInnerEdit;
      RestoreEditValue;
    end;
    SendMessage(Handle, EM_SETEVENTMASK, 0, ENM_CHANGE or ENM_SELCHANGE or ENM_IMECHANGE or
      ENM_REQUESTRESIZE or ENM_PROTECTED or ENM_KEYEVENTS or ENM_LINK or ENM_LANGCHANGE or
      ENM_OBJECTPOSITIONS or ENM_SCROLL);
    if MaxLength = 0 then
      DoSetMaxLength(MaxLongint)
    else
      DoSetMaxLength(MaxLength);
    InternalSetMemoMode;
    ApplyZoomFactor;
  end;
end;

procedure TcxRichInnerEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if FLockBoundsCount = 0 then
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

function TcxRichInnerEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (Container <> nil) and Container.DataBinding.UpdateAction(Action);
end;

function TcxRichInnerEdit.CanFocus: Boolean;
begin
  if Container = nil then
    Result := inherited CanFocus
  else
    Result := Container.CanFocus;
end;

function TcxRichInnerEdit.CanRedo: Boolean;
begin
  Result := False;
  if HandleAllocated then
    Result := SendMessage(Handle, EM_CANREDO, 0, 0) <> 0;
end;

procedure TcxRichInnerEdit.Redo;
begin
  if HandleAllocated then
    SendMessage(Handle, EM_REDO, 0, 0);
end;

procedure TcxRichInnerEdit.Undo;
begin
  if HandleAllocated then
    SendMessage(Handle, EM_UNDO, 0, 0);
end;

procedure TcxRichInnerEdit.DblClick;
begin
  inherited DblClick;
  _TcxContainerAccess.DblClick(Container);
end;

procedure TcxRichInnerEdit.DestroyWnd;
begin
  FSavedPlainText := PlainText;
  StoreEditValue;
  CloseOleObjects;
  FRichEditOle := nil;
  inherited DestroyWnd;
end;

procedure TcxRichInnerEdit.DoGetGestureOptions(var Gestures: TInteractiveGestures; var Options: TInteractiveGestureOptions);
begin
  if Container <> nil then
  begin
    Gestures := Container.Touch.InteractiveGestures;
    Options := Container.Touch.InteractiveGestureOptions;
    if (igPan in Gestures) and ((ScrollBars = ssNone) or (ScrollBars = ssHorizontal)) then
      Gestures := Gestures - [igPan];
  end
  else
    inherited DoGetGestureOptions(Gestures, Options);
end;

procedure TcxRichInnerEdit.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  _TcxContainerAccess.DragOver(Container, Source, Left + X, Top + Y, State, Accept);
end;

function TcxRichInnerEdit.GetSelText: string;
var
  ALen: Integer;
begin
  SetLength(Result, GetSelLength + 1);
  ALen := SendMessage(Handle, EM_GETSELTEXT, 0, LPARAM(PChar(Result)));
  SetLength(Result, ALen);
end;

procedure TcxRichInnerEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FInternalUpdating := False;
  _TcxContainerAccess.KeyDown(Container, Key, Shift);
  if Key = 0 then
    FInternalUpdating := True
  else
    inherited KeyDown(Key, Shift);
  if (Key = VK_RETURN) and Assigned(TdxSpellCheckerInstance.ISpellChecker) then
    Invalidate;
  if (RichVersion >= 200) and (Key = VK_RETURN) and not WantReturns and
    not(ssCtrl in KeyboardStateToShiftState) then
  begin
    Key := 0;
    Exit;
  end;
end;

procedure TcxRichInnerEdit.KeyPress(var Key: Char);
begin
  FInternalUpdating := False;
// Ctrl+I calls KeyPress with Key = Char(VK_TAB). A tab must be inserted even when WantTabs = False
//  if not WantTabs and (Key = Char(VK_TAB)) then
//    Key := #0;
  _TcxContainerAccess.KeyPress(Container, Key);
  if Key = #0 then
    FInternalUpdating := True
  else
    inherited KeyPress(Key);
end;

procedure TcxRichInnerEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  FInternalUpdating := False;
  if not WantTabs and ((Key = VK_TAB)) then
    Key := 0;
  _TcxContainerAccess.KeyUp(Container, Key, Shift);
  if Key = 0 then
    FInternalUpdating := True
  else
    inherited KeyUp(Key, Shift);
end;

procedure TcxRichInnerEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  _TcxContainerAccess.MouseDown(Container, Button, Shift, X + Left, Y + Top);
end;

procedure TcxRichInnerEdit.MouseLeave(AControl: TControl);
begin
  Container.ShortRefreshContainer(True);
end;

procedure TcxRichInnerEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  _TcxContainerAccess.MouseMove(Container, Shift, X + Left, Y + Top);
end;

procedure TcxRichInnerEdit.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  _TcxContainerAccess.MouseUp(Container, Button, Shift, X + Left, Y + Top);
end;

procedure TcxRichInnerEdit.RequestAlign;
begin
end;

procedure TcxRichInnerEdit.RequestSize(const Rect: TRect);
var
  R: TRect;
begin
  if Container <> nil then
  begin
    R := Rect;
    Dec(R.Left, Left);
    Dec(R.Top, Top);
    Inc(R.Right, Container.Width - Width - Left);
    Inc(R.Bottom, Container.Height - Height - Top);
    OffsetRect(R, Container.Left, Container.Top);
    Container.DoOnResizeRequest(R);
  end;
end;

procedure TcxRichInnerEdit.RichCreateParams(var Params: TCreateParams;
  out ARichVersion: Integer);
var
  ARichClassName: string;
  AWndClass: TWndClass;
  I: Integer;
  ARichClass: TcxRichEditClass;
begin
  if FRichEditLibrary = 0 then
    for I := High(cxRichEditDLLNames) downto Low(cxRichEditDLLNames) do
    begin
      FRichEditLibrary := LoadLibrary(PChar(cxRichEditDLLNames[I]));
      if FRichEditLibrary <> 0 then
        Break;
    end;
  if FRichEditLibrary = 0 then
    raise EcxEditError.Create(cxGetResourceString(@cxSEditRichEditLibraryError));

  for ARichClass := RichEditClass downto cxMinVersionRichEditClass do
  begin
    ARichClassName := cxRichEditClassNames[ARichClass];
    if GetClassInfo(HInstance, PChar(ARichClassName), AWndClass) then
      Break;
  end;

  if GetClassInfo(HInstance, PChar(ARichClassName), AWndClass) then
    ARichVersion := cxRichEditVersions[ARichClass]
  else
    raise EcxEditError.Create(cxGetResourceString(@cxSEditRichEditLibraryError));
  CreateSubClass(Params, PChar(ARichClassName));
end;

procedure TcxRichInnerEdit.URLClick(const AURLText: string; AButton: TMouseButton);
begin
  if Assigned(Container.ActiveProperties.OnURLClick) then
    Container.ActiveProperties.OnURLClick(Container, AURLText, AButton);
end;

procedure TcxRichInnerEdit.URLMove(const AURLText: string);
begin
  if Assigned(Container.ActiveProperties.OnURLMove) then
    Container.ActiveProperties.OnURLMove(Container, AURLText);
end;

procedure TcxRichInnerEdit.WndProc(var Message: TMessage);
begin
  if (Container <> nil) and Container.IsPopupScrollBars and FScrollUIActivityHelper.CheckScrollActivity(Self, Message) then
    Container.ShowTouchScrollUI(Container, True);
  if (Container <> nil) and Container.InnerControlMenuHandler(Message) then
    Exit;
  if ((Message.Msg = WM_LBUTTONDOWN) or (Message.Msg = WM_LBUTTONDBLCLK)) and
    (Container.DragMode = dmAutomatic) and not Container.IsDesigning then
  begin
    _TcxContainerAccess.BeginAutoDrag(Container);
    Exit;
  end;
  inherited WndProc(Message);
end;

function TcxRichInnerEdit.CanPaste: Boolean;
begin
  Result := HandleAllocated and
    (SendMessage(Handle, EM_CANPASTE, 0, 0) <> 0);
end;

function TcxRichInnerEdit.GetSelection: TCharRange;
begin
  cxSendStructMessage(Handle, EM_EXGETSEL, 0, Result);
end;

{$IFDEF DELPHIBERLIN}
procedure TcxRichInnerEdit.ChangeScale(M, D: Integer; isDpiChange: Boolean);
{$ELSE}
procedure TcxRichInnerEdit.ChangeScale(M, D: Integer);
{$ENDIF}
begin
  inherited;
  ScaleFactor.Change(M, D);
  ApplyZoomFactor;
end;

function TcxRichInnerEdit.AllowDrawEdgesAndBorders: Boolean;
begin
  Result := False;
end;

function TcxRichInnerEdit.GetAdvancedTypography: Boolean;
begin
  Result := FAdvancedTypography;
  if HandleAllocated and not (csDesigning in ComponentState) then
    Result := Boolean(SendMessage(Handle, EM_GETTYPOGRAPHYOPTIONS, 0, 0));
end;

function TcxRichInnerEdit.GetAutoURLDetect: Boolean;
begin
  Result := FAutoURLDetect;
  if HandleAllocated and not (csDesigning in ComponentState) then
    Result := Boolean(SendMessage(Handle, EM_GETAUTOURLDETECT, 0, 0));
end;

procedure TcxRichInnerEdit.ApplyZoomFactor;
var
  AZoomFactor: Double;
begin
  if not HandleAllocated then
    Exit;
  AZoomFactor := FZoomFactor;
  if not dxIsProcessPerMonitorV2Aware or (RichEditClass <> recRichEdit41) then
  begin
    AZoomFactor := AZoomFactor * ScaleFactor.TargetDPI / dxSystemScaleFactor.TargetDPI;
    AZoomFactor := cxRichEdit.CheckZoomFactor(AZoomFactor);
  end;
  SendMessage(Handle, EM_SETZOOM, Trunc(AZoomFactor * cxRichEditZoomDenominator), cxRichEditZoomDenominator);
end;

procedure TcxRichInnerEdit.CloseOleObjects;
var
  I: Integer;
  AReObject: TReObject;
begin
  if Assigned(FRichEditOle) then
  begin
    FillChar(AReObject, SizeOf(AReObject), 0);
    AReObject.cbStruct := SizeOf(AReObject);
    with IcxRichEditOle(FRichEditOle) do
    begin
      for I := GetObjectCount - 1 downto 0 do
        if Succeeded(GetObject(I, AReObject, REO_GETOBJ_POLEOBJ)) then
        begin
          if AReObject.dwFlags and REO_INPLACEACTIVE <> 0 then
            InPlaceDeactivate;
          AReObject.oleobj.Close(OLECLOSE_NOSAVE);
        end;
    end;
  end;
end;

//IcxContainerInnerControl
function TcxRichInnerEdit.GetControl: TWinControl;
begin
  Result := Self;
end;

function TcxRichInnerEdit.GetControlContainer: TcxContainer;
begin
  Result := Container;
end;

// IcxInnerEditHelper
function TcxRichInnerEdit.GetHelper: IcxCustomInnerEdit;
begin
  Result := Helper;
end;

function TcxRichInnerEdit.IsEditValueLocked: Boolean;
begin
  Result := FEditValueLockCount > 0;
end;

procedure TcxRichInnerEdit.RestoreEditValue;
begin
  Inc(FEditValueLockCount);
  try
    if not VarIsNull(FSavedEditValue) and (Container <> nil) and HandleAllocated then
    begin
      Container.EditValue := FSavedEditValue;
      FSavedEditValue := Null;
    end;
  finally
    Dec(FEditValueLockCount);
  end;
end;

procedure TcxRichInnerEdit.StoreEditValue;
begin
  Inc(FEditValueLockCount);
  try
    if VarIsNull(FSavedEditValue) and (Container <> nil) and
        not Container.IsDestroying and not Container.IsDesigning then
      FSavedEditValue := Container.EditValue;
  finally
    Dec(FEditValueLockCount);
  end;
end;

function TcxRichInnerEdit.GetContainer: TcxCustomRichEdit;
begin
  if Parent is TcxCustomRichEdit then
    Result := TcxCustomRichEdit(Parent)
  else
    Result := nil;
end;

function TcxRichInnerEdit.GetLineCount: Integer;
begin
  Result := SendMessage(Handle, EM_GETLINECOUNT, 0, 0);
end;

function TcxRichInnerEdit.GetLineIndex(AIndex: Integer): Integer;
begin
  Result := SendMessage(Handle, EM_LINEINDEX , AIndex, 0);
end;

function TcxRichInnerEdit.GetLineLength(AIndex: Integer): Integer;
begin
  if GetLineIndex(AIndex) <> -1 then
    Result := SendMessage(Handle, EM_LINELENGTH, GetLineIndex(AIndex), 0)
  else
    Result := 0;
end;

function TcxRichInnerEdit.GetRichLines: TcxRichEditStrings;
begin
  Result := FRichLines;
end;

function TcxRichInnerEdit.GetRichEditOle: IcxRichEditOle;
begin
  if FRichEditOle <> nil then
    Result := FRichEditOle as IcxRichEditOle
  else
    Result := nil;
end;

function TcxRichInnerEdit.GetRichEditOleCallBack: TcxRichEditOleCallback;
begin
  if Assigned(FRichEditOleCallback) then
    Result := FRichEditOleCallback as TcxRichEditOleCallback
  else
    Result := nil;
end;

function TcxRichInnerEdit.GetTextRange(AStartPos, AEndPos: Longint): string;
var
  ATextRange: TcxTextRange;
begin
  SetLength(Result, AEndPos - AStartPos + 1);
  ATextRange.chrg.cpMin := AStartPos;
  ATextRange.chrg.cpMax := AEndPos;
  ATextRange.lpstrText := PChar(Result);
  SetLength(Result, SendMessage(Handle, EM_GETTEXTRANGE, 0, LPARAM(@ATextRange)));
end;

procedure TcxRichInnerEdit.InternalSetMemoMode(AForcedReload: Boolean);
var
  AText: string;
  ATextMode: LRESULT;
begin
  if not HandleAllocated then
    Exit;
  ATextMode := SendMessage(Handle, EM_GETTEXTMODE, 0, 0);
  if MemoMode and (ATextMode and TM_PLAINTEXT <> 0) or not MemoMode and (ATextMode and TM_RICHTEXT <> 0) and not AForcedReload then
    Exit;
  AText := Text;
  SendMessage(Handle, WM_SETTEXT, 0, 0);
  if MemoMode then
    ATextMode := ATextMode and not TM_RICHTEXT or TM_PLAINTEXT
  else
    ATextMode := ATextMode and not TM_PLAINTEXT or TM_RICHTEXT;
  SendMessage(Handle, EM_SETTEXTMODE, ATextMode, 0);
  Text := AText;
end;

procedure TcxRichInnerEdit.SetAdvancedTypography(Value: Boolean);
begin
  if FAdvancedTypography <> Value then
  begin
    FAdvancedTypography := Value;
    RecreateWnd;
  end;
end;

procedure TcxRichInnerEdit.SetAllowObjects(Value: Boolean);
begin
  if FAllowObjects <> Value then
  begin
    FAllowObjects := Value;
    if not FAllowObjects then
    begin
      CloseOleObjects;
      FRichEditOle := nil;
    end;
    RecreateWnd;
  end;
end;

procedure TcxRichInnerEdit.SetAutoURLDetect(Value: Boolean);
begin
  if Value <> FAutoURLDetect then
  begin
    FAutoURLDetect := Value;
    RecreateWnd;
  end;
end;

procedure TcxRichInnerEdit.SetMemoMode(Value: Boolean);
begin
  if Value <> FMemoMode then
  begin
    FMemoMode := Value;
    RecreateWnd;
  end;
end;

procedure TcxRichInnerEdit.SetRichEditClass(AValue: TcxRichEditClass);
begin
  if AValue <> FRichEditClass then
  begin
    FRichEditClass := AValue;
    RecreateWnd;
  end;
end;

procedure TcxRichInnerEdit.SetRichLines(Value: TcxRichEditStrings);
begin
  FRichLines.Assign(Value);
end;

procedure TcxRichInnerEdit.SetSelectionBar(Value: Boolean);
begin
  if Value <> FSelectionBar then
  begin
    FSelectionBar := Value;
    RecreateWnd;
  end;
end;

procedure TcxRichInnerEdit.SetOleControlActive(AActive: Boolean);
var
  AForm: TCustomForm;
begin
  try
    AForm := GetParentForm(Self);
    if AForm <> nil then
      if AActive and Container.CanModify and Container.DoEditing then
      begin
        if (AForm.ActiveOleControl <> nil) and (AForm.ActiveOleControl <> Self) then
          AForm.ActiveOleControl.Perform(CM_UIDEACTIVATE, 0, 0);
        AForm.ActiveOleControl := Self;
        if AllowObjects and CanFocus then SetFocus;
      end
      else
      begin
        if AForm.ActiveOleControl = Self then
          AForm.ActiveOleControl := nil;
        if (AForm.ActiveControl = Self) and AllowObjects then
        begin
          Windows.SetFocus(Handle);
          SelectionChange;
        end;
      end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TcxRichInnerEdit.SetZoomFactor(const Value: Double);
begin
  if ZoomFactor <> Value then
  begin
    FZoomFactor := Value;
    ApplyZoomFactor;
  end;
end;

procedure TcxRichInnerEdit.WMClear(var Message: TMessage);
begin
  if (Self.SelLength > 0) and Container.DoEditing then
    inherited;
end;

procedure TcxRichInnerEdit.WMCut(var Message: TMessage);
begin
  if SelLength > 0 then
    if Container.DoEditing then
      inherited
    else
      Container.CopyToClipboard;
end;

procedure TcxRichInnerEdit.WMEraseBkgnd(var Message: TMessage);
begin
  if FIsEraseBackgroundLocked or (Container <> nil) and Container.IsInplace then
    Message.Result := 1
  else
    inherited;
//    CallWindowProc(DefWndProc, Handle, Message.Msg, Message.WParam, Message.LParam);
end;

procedure TcxRichInnerEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if Container.TabsNeeded and (GetKeyState(VK_CONTROL) >= 0) then
    Message.Result := Message.Result or DLGC_WANTTAB;
  if FEscapePressed then
    Message.Result := Message.Result and not DLGC_WANTALLKEYS;
end;

procedure TcxRichInnerEdit.WMKeyDown(var Message: TWMKeyDown);
var
  AKey: Word;
  APrevState: TcxCustomInnerTextEditPrevState;
  AShiftState: TShiftState;
begin
  if Message.CharCode <> VK_ESCAPE then
    FKeyPressProcessed := True;
  try
    SaveTextEditState(Helper, False, APrevState);
    FInternalUpdating := False;
    inherited;
    Container.SetScrollBarsParameters;
    if FInternalUpdating then
      Exit;
  finally
    FKeyPressProcessed := False;
  end;
  AShiftState := KeyDataToShiftState(Message.KeyData);
  AKey := Message.CharCode;
  if (AKey <> 0) and not Container.CanKeyDownModifyEdit(AKey, AShiftState) and
    not CheckTextEditState(Helper, APrevState) and
    not Container.IsNavigationKey(AKey, AShiftState) then
      Container.DoAfterKeyDown(AKey, AShiftState);
  Message.CharCode := AKey;
end;

procedure TcxRichInnerEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if not(csDestroying in ComponentState) then
    Container.FocusChanged;
end;

procedure TcxRichInnerEdit.WMMButtonDown(var Message: TWMMButtonDown);
begin
  Message.Result := 1;
  SendMessage(Container.Handle, WM_MBUTTONDOWN, 0,
    MakeLParam(Message.XPos + Left, Message.YPos + Top));
end;

procedure TcxRichInnerEdit.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  if (Container <> nil) and not Container.ScrollBarsCalculating then
    PostMessage(Container.Handle, DXM_UPDATESCROLLBARS, 0, 0);
end;

procedure TcxRichInnerEdit.WMNCPaint(var Message: TWMNCPaint);
begin
  inherited;
  if (Container <> nil) and UsecxScrollBars and Container.NeedsScrollBars and
    Container.HScrollBar.Visible and Container.VScrollBar.Visible then
      cxFillSizeGrip(Container);
end;

procedure TcxRichInnerEdit.WMPaint(var Message: TWMPaint);
begin
  if RichVersion >= 200 then
    FIsEraseBackgroundLocked := True;
  try
    inherited;
  finally
    FIsEraseBackgroundLocked := False;
  end;
end;

procedure TcxRichInnerEdit.WMPaste(var Message: TMessage);
begin
  if (Clipboard.FormatCount > 0) and Container.DoEditing then
    inherited;
end;

procedure TcxRichInnerEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if not(csDestroying in ComponentState) and (Message.FocusedWnd <> Container.Handle) then
    Container.FocusChanged;
  if AutoSelect and HandleAllocated then
    PostMessage(Handle, EM_SETSEL, 0, -1);
end;

procedure TcxRichInnerEdit.WMSetFont(var Message: TWMSetFont);
begin
  if HandleAllocated and MemoMode then
  begin
    with TMessage(Message) do
      Result := CallWindowProc(DefWndProc, Handle, Msg, WParam, LParam);
    DefAttributes.Color := Font.Color;
  end
  else
    inherited;
end;

procedure TcxRichInnerEdit.WMTimer(var Message: TWMTimer);
begin
  inherited;
  if Container <> nil then
    Container.RefreshScrollBars;
end;

procedure TcxRichInnerEdit.WMHScroll(var Message: TWMHScroll);
begin
  inherited;
  Container.SetScrollBarsParameters;
end;

procedure TcxRichInnerEdit.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
  Container.SetScrollBarsParameters;
end;

procedure TcxRichInnerEdit.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  if Container <> nil then
    Container.SetScrollBarsParameters;
  inherited;
end;

procedure TcxRichInnerEdit.WMWindowPosChanging(var Message: TWMWindowPosChanging);
var
  ARgn: HRGN;
begin
  inherited;
  if (Container <> nil) and not(csDestroying in ComponentState) and
    Container.NeedsScrollBars and Container.HScrollBar.Visible and Container.VScrollBar.Visible then
  begin
    ARgn := CreateRectRgnIndirect(GetSizeGripRect(Self));
    SendMessage(Handle, WM_NCPAINT, ARgn, 0);
    Windows.DeleteObject(ARgn);
  end;
end;

procedure TcxRichInnerEdit.EMReplaceSel(var Message: TMessage);
begin
  if RichLines.IsModification or (Container = nil) or Container.DoEditing then
    inherited;
end;

procedure TcxRichInnerEdit.EMSetCharFormat(var Message: TMessage);
begin
  if (Message.WParam = SCF_SELECTION) and (SelLength > 0) then
    Container.DoEditing;
  inherited;
end;

procedure TcxRichInnerEdit.EMSetParaFormat(var Message: TMessage);
begin
  if (Container <> nil) and not Container.IsDestroying and (Container.ComponentState * [csLoading, csReading] = []) then
    Container.DoEditing;
  inherited;
end;

procedure TcxRichInnerEdit.CMColorChanged(var Message: TMessage);
begin
  if (Container <> nil) and not Container.IsInplace then
    inherited;
end;

procedure TcxRichInnerEdit.CMFontChanged(var Message: TMessage);
begin
  if (Container = nil) or not Container.IsScaleChanging then
    inherited;
end;

procedure TcxRichInnerEdit.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseLeave(Self)
  else
    MouseLeave(TControl(Message.lParam));
end;

procedure TcxRichInnerEdit.CNNotify(var Message: TWMNotify);

  procedure SetOutRange(var ARange: TCharRange);
  begin
    ARange.cpMin := -1;
    ARange.cpMax := -1;
  end;

begin
  if not (csDesigning in ComponentState) then
    with Message do
      case NMHdr^.code of
        EN_REQUESTRESIZE:
          begin
            if NMHdr^.idFrom = 0 then
              Exit;
          end;
        EN_LINK:
          with PcxENLink(NMHdr)^ do
          begin
            case Msg of
              WM_RBUTTONDOWN:
                begin
                  FURLClickRange := chrg;
                  FURLClickBtn := mbRight;
                end;
              WM_RBUTTONUP:
                begin
                  if (FURLClickBtn = mbRight) and (FURLClickRange.cpMin = chrg.cpMin) and
                      (FURLClickRange.cpMax = chrg.cpMax) then
                    URLClick(GetTextRange(chrg.cpMin, chrg.cpMax), mbRight);
                  SetOutRange(FURLClickRange);
                end;
              WM_LBUTTONDOWN:
                begin
                  FURLClickRange := chrg;
                  FURLClickBtn := mbLeft;
                end;
              WM_LBUTTONUP:
                begin
                  if (FURLClickBtn = mbLeft) and (FURLClickRange.cpMin = chrg.cpMin) and
                      (FURLClickRange.cpMax = chrg.cpMax) then
                    URLClick(GetTextRange(chrg.cpMin, chrg.cpMax), mbLeft);
                  SetOutRange(FURLClickRange);
                end;
              WM_MOUSEMOVE:
                URLMove(GetTextRange(chrg.cpMin, chrg.cpMax));
            end;
          end;
      end;
  inherited;
end;

procedure TcxRichInnerEdit.CMDocWindowActivate(var Message: TMessage);
begin
  if Assigned(FRichEditOleCallback) then
    with TcxRichEditOleCallback(FRichEditOleCallback) do
      if Assigned(DocParentForm) and
        cxIsFormMDIChild(DocParentForm.Form) then
      begin
        if Message.WParam = 0 then
        begin
          ParentFrame.SetMenu(0, 0, 0);
          ParentFrame.ClearBorderSpace;
        end;
      end;
end;

procedure TcxRichInnerEdit.WMChar(var Message: TWMChar);
begin
  if Message.CharCode <> VK_ESCAPE then
    FKeyPressProcessed := True;
  try
    inherited;
  finally
    FKeyPressProcessed := False;
  end;
end;

procedure TcxRichInnerEdit.CNCommand(var Message: TWMCommand);
begin
  if Message.NotifyCode = EN_CHANGE then
    if (Container <> nil) and not Container.IsDestroying and
      (Container.ComponentState * [csLoading, csReading] = []) and
      Focused and not Container.ChangeEventsCatcher.IsLocked then
        Container.DoEditing;

  inherited;
end;

procedure TcxRichInnerEdit.CNKeyDown(var Message: TWMKeyDown);
begin
  if Message.CharCode = VK_ESCAPE then
    FEscapePressed := True;
  try
    inherited;
  finally
    FEscapePressed := False;
  end;
end;

procedure TcxRichInnerEdit.WMGetText(var Message: TMessage);
begin
  inherited;
end;

procedure TcxRichInnerEdit.WMGetTextLength(var Message: TWMGetTextLength);
begin
  inherited;
end;

procedure TcxRichInnerEdit.WMSetText(var Message: TWMSetText);
begin
  if MemoMode and dxIsRichText(Message.Text) then
    Message.Text := PChar(ConvertRichText(Message.Text));
  inherited;
end;

procedure TcxRichInnerEdit.WMIMEComposition(var Message: TMessage);
begin
  if Container.DoEditing then
    inherited;
end;

procedure TcxRichInnerEdit.EMExLineFromChar(var Message: TMessage);
begin
  inherited;
  if MemoMode then
  begin
    if GetLineIndex(Message.Result + 1) = Message.LParam then
      Message.Result := Message.Result + 1;
  end;
end;

procedure TcxRichInnerEdit.EMLineLength(var Message: TMessage);
var
  ALineIndex: Integer;
begin
  inherited;
  if MemoMode then
  begin
    ALineIndex := SendMessage(Handle, EM_EXLINEFROMCHAR, 0, Message.WParam);
    if (ALineIndex = GetLineCount - 1) and (Lines[ALineIndex] = '') then
      Message.Result := 0;
  end;
end;

procedure TcxRichInnerEdit.WMPrintClient(var Message: TMessage);
var
  AFirstLineIndex, ACharIndex, AHeight: Integer;
begin
//  inherited;
  AFirstLineIndex := SendMessage(Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
  ACharIndex := SendMessage(Handle, EM_LINEINDEX, AFirstLineIndex, 0);
  dxDrawRichEdit(Message.WParam, cxRectSetNullOrigin(ClientRect), Handle, ScaleFactor, ACharIndex, -1, False, AHeight);
end;

{ TcxOleUILinkInfo }

constructor TcxOleUILinkInfo.Create(AOwner: TcxRichInnerEdit; AReObject: TReObject);
begin
  inherited Create;
  FRichEdit := AOwner;
  FReObject := AReObject;
  FReObject.oleobj.QueryInterface(IOleLink, FOleLink);
end;

destructor TcxOleUILinkInfo.Destroy;
begin
  ReleaseObject(FOleLink);
  inherited Destroy;
end;

//IOleUILinkInfo
function TcxOleUILinkInfo.GetLastUpdate(dwLink: Longint;
  var LastUpdate: TFileTime): HResult;
begin
  Result := S_OK;
end;

//IOleUILinkContainer
function TcxOleUILinkInfo.GetNextLink(dwLink: Longint): Longint;
begin
  Result := 0;
end;

function TcxOleUILinkInfo.SetLinkUpdateOptions(dwLink: Longint;
  dwUpdateOpt: Longint): HResult;
begin
  Result := FOleLink.SetUpdateOptions(dwUpdateOpt);
  if Succeeded(Result) then
    FRichEdit.Modified := True;
end;

function TcxOleUILinkInfo.GetLinkUpdateOptions(dwLink: Longint;
  var dwUpdateOpt: Longint): HResult;
begin
  Result := FOleLink.GetUpdateOptions(dwUpdateOpt);
end;

function TcxOleUILinkInfo.SetLinkSource(dwLink: Longint; pszDisplayName: PChar;
  lenFileName: Longint; var chEaten: Longint;
  fValidateSource: BOOL): HResult;
var
  ADisplayName: string;
  ABuffer: array[0..255] of WideChar;
begin
  Result := E_FAIL;
  if fValidateSource then
  begin
    ADisplayName := pszDisplayName;
    if Succeeded(FOleLink.SetSourceDisplayName(StringToWideChar(ADisplayName,
      ABuffer, SizeOf(ABuffer) div 2))) then
    begin
      chEaten := Length(ADisplayName);
      OleCheck(FReObject.oleobj.Update);
      Result := S_OK;
    end;
  end
  else
    raise EOutOfResources.Create(cxGetResourceString(@cxSEditRichEditLinkFail));
end;

function TcxOleUILinkInfo.GetLinkSource(dwLink: Longint; var pszDisplayName: PChar;
  var lenFileName: Longint; var pszFullLinkType: PChar;
  var pszShortLinkType: PChar; var fSourceAvailable: BOOL;
  var fIsSelected: BOOL): HResult;
var
  AMoniker: IMoniker;
begin
  if @pszDisplayName <> nil then
    pszDisplayName := cxGetOleLinkDisplayName(FOleLink);
  if @lenFileName <> nil then
  begin
    lenFileName := 0;
    FOleLink.GetSourceMoniker(AMoniker);
    if AMoniker <> nil then
    begin
      lenFileName := cxOleStdGetLenFilePrefixOfMoniker(AMoniker);
      if Assigned(AMoniker) then
        AMoniker._Release;
    end;
  end;
  if @pszFullLinkType <> nil then
    pszFullLinkType := cxGetOleObjectFullName(FReObject.oleobj);
  if @pszShortLinkType <> nil then
    pszShortLinkType := cxGetOleObjectShortName(FReObject.oleobj);
  Result := S_OK;
end;

function TcxOleUILinkInfo.OpenLinkSource(dwLink: Longint): HResult;
begin
  OleCheck(FReObject.oleobj.DoVerb(OLEIVERB_SHOW, nil, FReObject.olesite,
    0, FRichEdit.Handle, FRichEdit.ClientRect));
  Result := S_OK;
end;

function TcxOleUILinkInfo.UpdateLink(dwLink: Longint; fErrorMessage: BOOL;
  fErrorAction: BOOL): HResult;
begin
  OleCheck(FReObject.oleobj.Update);
  Result := S_OK;
end;

function TcxOleUILinkInfo.CancelLink(dwLink: Longint): HResult;
begin
  Result := E_NOTIMPL;
end;

{ TcxOleUIObjInfo }

constructor TcxOleUIObjInfo.Create(AOwner: TcxRichInnerEdit; AReObject: TReObject);
begin
  inherited Create;
  FRichEdit := AOwner;
  FReObject := AReObject;
end;

function TcxOleUIObjInfo.GetObjectDataSize: Integer;
begin
  Result := -1;
end;

//IOleUIObjInfo
function TcxOleUIObjInfo.GetObjectInfo(dwObject: Longint;
  var dwObjSize: Longint; var lpszLabel: PChar;
  var lpszType: PChar; var lpszShortType: PChar;
  var lpszLocation: PChar): HResult;
begin
  if @dwObjSize <> nil then
    dwObjSize := GetObjectDataSize;
  if @lpszLabel <> nil then
    lpszLabel := cxGetOleObjectFullName(FReObject.oleobj);
  if @lpszType <> nil then
    lpszType := cxGetOleObjectFullName(FReObject.oleobj);
  if @lpszShortType <> nil then
    lpszShortType := cxGetOleObjectShortName(FReObject.oleobj);
  if @lpszLocation <> nil then
    lpszLocation := PChar(Application.Title);
  Result := S_OK;
end;

function TcxOleUIObjInfo.GetConvertInfo(dwObject: Longint; var ClassID: TCLSID;
  var wFormat: Word; var ConvertDefaultClassID: TCLSID;
  var lpClsidExclude: PCLSID; var cClsidExclude: Longint): HResult;
begin
  FReObject.oleobj.GetUserClassID(ClassID);
  Result := S_OK;
end;

function TcxOleUIObjInfo.ConvertObject(dwObject: Longint; const clsidNew: TCLSID): HResult;
begin
  Result := E_NOTIMPL;
end;

function TcxOleUIObjInfo.GetViewInfo(dwObject: Longint; var hMetaPict: HGlobal;
  var dvAspect: Longint; var nCurrentScale: Integer): HResult;
begin
  if @hMetaPict <> nil then
    hMetaPict := cxGetIconMetaPict(FReObject.oleobj, FReObject.dvaspect);
  if @dvAspect <> nil then
    dvAspect := FReObject.dvaspect;
  if @nCurrentScale <> nil then
    nCurrentScale := 100;
  Result := S_OK;
end;

function TcxOleUIObjInfo.SetViewInfo(dwObject: Longint; hMetaPict: HGlobal;
  dvAspect: Longint; nCurrentScale: Integer;
  bRelativeToOrig: BOOL): HResult;
var
  AShowAsIcon: Boolean;
begin
  if not Assigned(FRichEdit.FRichEditOle) then
  begin
    Result := E_NOTIMPL;
    Exit;
  end;
  case dvAspect of
    DVASPECT_CONTENT: AShowAsIcon := False;
    DVASPECT_ICON: AShowAsIcon := True;
  else
    AShowAsIcon := FReObject.dvaspect = DVASPECT_ICON;
  end;
  FRichEdit.RichEditOle.InPlaceDeactivate;
  Result := cxSetDrawAspect(FReObject.oleobj, AShowAsIcon, hMetaPict,
    FReObject.dvaspect);
  if Succeeded(Result) then
    FRichEdit.RichEditOle.SetDvaspect(Longint(REO_IOB_SELECTION),
      FReObject.dvaspect);
end;

{ TcxRichEdit }

constructor TcxCustomRichEdit.Create(AOwner: TComponent);
begin
  inherited;
  FParagraph2 := TcxParaAttributes2.Create(Self);
  FDefAttributes2 := TcxTextAttributes2.Create(Self, atDefaultText);
  FSelAttributes2 := TcxTextAttributes2.Create(Self, atSelected);
end;

destructor TcxCustomRichEdit.Destroy;
begin
  FreeAndNil(FSelAttributes2);
  FreeAndNil(FDefAttributes2);
  FreeAndNil(FParagraph2);
  FreeAndNil(FEditPopupMenu);
  inherited Destroy;
end;

function TcxCustomRichEdit.GetInnerEditClass: TControlClass;
begin
  Result := TcxRichInnerEdit;
end;

procedure TcxCustomRichEdit.DoProtectChange(Sender: TObject;
  AStartPos, AEndPos: Integer; var AAllowChange: Boolean);
begin
  with Properties do
    if Assigned(OnProtectChange) then
      OnProtectChange(Self, AStartPos, AEndPos, AAllowChange);
  if RepositoryItem <> nil then
    with ActiveProperties do
      if Assigned(OnProtectChange) then
        OnProtectChange(Self, AStartPos, AEndPos, AAllowChange);
end;

procedure TcxCustomRichEdit.DoSaveClipboard(Sender: TObject;
  ANumObjects, ANumChars: Integer; var ASaveClipboard: Boolean);
begin
  if IsDestroying then
    Exit;
  with Properties do
    if Assigned(OnSaveClipboard) then
      OnSaveClipboard(Self, ANumObjects, ANumChars, ASaveClipboard);
  if RepositoryItem <> nil then
    with ActiveProperties do
      if Assigned(OnSaveClipboard) then
        OnSaveClipboard(Self, ANumObjects, ANumChars, ASaveClipboard);
end;

procedure TcxCustomRichEdit.EditPopupMenuClick(Sender: TObject);
begin
  case TdxNativeInt(TMenuItem(Sender).Tag) of
    -1: Undo;
    -2: InnerRich.Redo;
    -3: CutToClipboard;
    -4: CopyToClipboard;
    -5: PasteFromClipboard;
    -6: ClearSelection;
    -7: InnerRich.SelectAll;
  end;
end;

function TcxCustomRichEdit.GetLines: TStrings;
begin
  Result := InnerRich.RichLines;
end;

function TcxCustomRichEdit.GetInnerRich: TcxRichInnerEdit;
begin
  Result := TcxRichInnerEdit(InnerControl);
end;

procedure TcxCustomRichEdit.SetLines(Value: TStrings);
begin
  InnerRich.RichLines.Assign(Value);
end;

procedure TcxCustomRichEdit.ChangeHandler(Sender: TObject);
begin
  FIsNullEditValue := False;
  inherited ChangeHandler(Sender);
  DoEditValueChanged;
end;

procedure TcxCustomRichEdit.SelChange(Sender: TObject);
begin
  if [csLoading, csReading] * ComponentState = [] then
    DoOnSelectionChange;
  InternalCheckSelection;
end;

procedure TcxCustomRichEdit.AdjustInnerEdit;
begin
  if IsDesigning or ActiveProperties.MemoMode or ActiveProperties.PlainText then
    inherited AdjustInnerEdit
  else
  begin
    if not IsInplace and (Style = ActiveStyle) and
        not AreFontsEqual(InnerRich.Font, VisibleFont) then
      InnerRich.Font := VisibleFont;
    if InnerRich.HandleAllocated then
      SendMessage(InnerRich.Handle, EM_SETBKGNDCOLOR, 0, ColorToRGB(ViewInfo.BackgroundColor))
    else
      InnerRich.Color := ViewInfo.BackgroundColor;
  end;
end;

function TcxCustomRichEdit.CanFocusOnClick: Boolean;
begin
  Result := inherited CanFocusOnClick and not (csLButtonDown in InnerRich.ControlState);
end;

function TcxCustomRichEdit.CanKeyDownModifyEdit(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := inherited CanKeyDownModifyEdit(Key, Shift) or
   (((Key = VK_DELETE) or (Key = VK_INSERT)) and (ssShift in Shift)) or
   (((Key = Ord('V')) or (Key = Ord('X')) and (ssCtrl in Shift))) and
   (Clipboard.FormatCount > 0);
  Result := Result or (Key = VK_BACK); // !!!
end;

function TcxCustomRichEdit.DoShowPopupMenu(AMenu: TComponent; X, Y: Integer): Boolean;
begin
  if Assigned(AMenu) then
    Result := inherited DoShowPopupMenu(AMenu, X, Y)
  else
  begin
    UpdateEditPopupMenuItems(GetEditPopupMenuInstance);
    Result := inherited DoShowPopupMenu(GetEditPopupMenuInstance, X, Y);
    EditingChanged;
  end;
end;

procedure TcxCustomRichEdit.DoSpellCheckerPostEditValue;
begin
  SpellCheckerSetValue(EditingValue);
end;

function TcxCustomRichEdit.GetEditValue: TcxEditValue;
begin
  if FIsNullEditValue then
    Result := Null
  else
    PrepareEditValue('', Result, False);
end;

{ TcxCustomRichEditViewData }

procedure TcxCustomRichEditViewData.Calculate(ACanvas: TcxCanvas;
  const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
  Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
  AIsMouseEvent: Boolean);
begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo, AIsMouseEvent);
  TcxCustomRichEditViewInfo(AViewInfo).IsDrawBitmapDirty := True;
end;

function TcxCustomRichEditViewData.InternalGetEditContentSize(
  ACanvas: TcxCanvas; const AEditValue: TcxEditValue;
  const AEditSizeProperties: TcxEditSizeProperties): TSize;
var
  ADC: HDC;
  AWidth, AHeight: Integer;
  S: string;
begin
  if VarIsSoftNull(AEditValue) then
    Result := inherited InternalGetEditContentSize(ACanvas, AEditValue, AEditSizeProperties)
  else
  begin
    ADC := CreateCompatibleDC(ACanvas.Handle);
    try
      S := dxVariantToString(AEditValue);
      AWidth := AEditSizeProperties.Width;
      if AWidth = -1 then
        AWidth := cxRichEditCalculateBestFit(ADC, S, Properties, Style.Font, ScaleFactor);
      Result.cx := AWidth;
      if IsInplace and (AEditSizeProperties.Width <> -1) then
        Dec(AWidth, 2);
      cxDrawRichEdit(ADC, Rect(0, 0, AWidth, 0), S, Properties, Style.Font, clWhite, clBlack, True, AHeight, ScaleFactor);
      if AHeight > 0 then
        Inc(AHeight, GetEditContentSizeCorrection.cy);
       Result.cy := AHeight;
    finally
      DeleteDC(ADC);
    end;
  end;
end;

function TcxCustomRichEditViewData.GetProperties: TcxCustomRichEditProperties;
begin
  Result := TcxCustomRichEditProperties(FProperties);
end;

{ TcxRichEditOleCallback }

constructor TcxRichEditOleCallback.Create(AOwner: TcxRichInnerEdit);
begin
  inherited Create;
  FEdit := AOwner;
  FAccelCount := 0;
end;

function TcxRichEditOleCallback.ContextSensitiveHelp(fEnterMode: BOOL): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TcxRichEditOleCallback.DeleteObject(oleobj: IOLEObject): HRESULT;
begin
  if not FEdit.AllowObjects then
  begin
    Result := E_NOTIMPL;
    Exit;
  end;
  if Assigned(oleobj) then
    oleobj.Close(OLECLOSE_NOSAVE);
  Result := S_OK;
end;

function TcxRichEditOleCallback.GetClipboardData(const chrg: TCharRange; reco: DWORD;
  out dataobj: IDataObject): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TcxRichEditOleCallback.GetContextMenu(seltype: Word; oleobj: IOleObject;
  const chrg: TCharRange; var menu: HMENU): HRESULT;
var
  P: TPoint;
begin
  P := GetMouseCursorPos;
  SendMessage(FEdit.Handle, WM_CONTEXTMENU, FEdit.Handle, dxPointToLParam(P));
  Result := S_OK;
end;

function TcxRichEditOleCallback.GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
  var dwEffect: DWORD): HRESULT;
  var Effect: DWORD;
begin
  if not FEdit.AllowObjects then
  begin
    Result := E_NOTIMPL;
    Exit;
  end;
  Result:= S_OK;
  if not fDrag then
  begin
    if ((grfKeyState and (MK_CONTROL or MK_SHIFT)) = (MK_CONTROL or MK_SHIFT)) then
      Effect := DROPEFFECT_LINK
    else if ((grfKeyState and MK_CONTROL) = MK_CONTROL) then
      Effect := DROPEFFECT_COPY
    else
      Effect := DROPEFFECT_MOVE;
    if (Effect and dwEffect <> 0) then
      dwEffect := Effect;
  end;
end;

function TcxRichEditOleCallback.GetInPlaceContext(out Frame: IOleInPlaceFrame;
  out Doc: IOleInPlaceUIWindow;
  lpFrameInfo: POleInPlaceFrameInfo): HRESULT;
begin
  AssignParentFrame;
  if Assigned(FParentFrame) and FEdit.AllowObjects then
  begin
    Frame := FParentFrame;
    Doc := FDocParentForm;
    CreateAccelTable;
    with lpFrameInfo^ do
    begin
      fMDIApp := False;
      FParentFrame.GetWindow(hWndFrame);
      hAccel := FAccelTable;
      cAccelEntries := FAccelCount;
    end;
    Result := S_OK;
  end
  else
    Result := E_NOTIMPL;
end;

function TcxRichEditOleCallback.GetNewStorage(out stg: IStorage): HRESULT;
var
  LockBytes: ILockBytes;
begin
  if not FEdit.AllowObjects then
  begin
    Result := E_NOTIMPL;
    Exit;
  end;
  Result:= S_OK;
  try
    OleCheck(CreateILockBytesOnHGlobal(0, True, LockBytes));
    OleCheck(StgCreateDocfileOnILockBytes(LockBytes, STGM_READWRITE
      or STGM_SHARE_EXCLUSIVE or STGM_CREATE, 0, stg));
  except
    Result:= E_OUTOFMEMORY;
  end;
end;

function TcxRichEditOleCallback.QueryAcceptData(dataobj: IDataObject; var cfFormat: TClipFormat;
  reco: DWORD; fReally: BOOL; hMetaPict: HGLOBAL): HRESULT;
begin
  if not FEdit.AllowObjects then
    Result := E_NOTIMPL
  else
    Result := S_OK;
end;

function TcxRichEditOleCallback.QueryInsertObject(const clsid: TCLSID;
  stg: IStorage; cp: longint): HRESULT;
var
  AAllowInsertObject: Boolean;
begin
  if not FEdit.AllowObjects then
  begin
    Result := E_NOTIMPL;
    Exit;
  end;
  Result := S_OK;
  if cp <> -1 then
    Exit;
  AAllowInsertObject := True;
  FEdit.BeforeInsertObject(AAllowInsertObject, clsid);
  if not AAllowInsertObject then
    Result := E_NOTIMPL;
end;

function TcxRichEditOleCallback.ShowContainerUI(fShow: BOOL): HRESULT;
begin
  if not FEdit.AllowObjects then
  begin
    Result := E_NOTIMPL;
    Exit;
  end;
  if not fShow then AssignParentFrame;
  if Assigned(FEdit) then
  begin
    if fShow then
    begin
      FParentFrame.ClearBorderSpace;
      DestroyAccelTable;
      FParentFrame := nil;
      FDocParentForm := nil;
      FEdit.SetOleControlActive(False);
    end
    else
      FEdit.SetOleControlActive(True);
    Result := S_OK;
  end
  else
    Result := E_NOTIMPL;
end;

procedure TcxRichEditOleCallback.AssignParentFrame;
begin
  if (GetParentForm(FEdit) <> nil) and not Assigned(FParentFrame) and
    FEdit.AllowObjects then
  begin
    FDocParentForm := cxGetVCLFrameForm(ValidParentForm(FEdit));
    FParentFrame := FDocParentForm;
    if cxIsFormMDIChild(FDocParentForm.Form) then
      FParentFrame := cxGetVCLFrameForm(Application.MainForm);
  end;
end;

procedure TcxRichEditOleCallback.CreateAccelTable;
var
  AMenu: TMainMenu;
begin
  if (FAccelTable = 0) and Assigned(FParentFrame) then
  begin
    AMenu := FParentFrame.Form.Menu;
    if AMenu <> nil then
      AMenu.GetOle2AcceleratorTable(FAccelTable, FAccelCount, [0, 2, 4]);
  end;
end;

procedure TcxRichEditOleCallback.DestroyAccelTable;
begin
  if FAccelTable <> 0 then
  begin
    DestroyAcceleratorTable(FAccelTable);
    FAccelTable := 0;
    FAccelCount := 0;
  end;
end;

{ TcxCustomRichEditProperties }

constructor TcxCustomRichEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAdvancedTypography := True;
  FHideScrollBars := True;
  FRichEditClass := cxMaxVersionRichEditClass;
  FZoomFactor := 1;
end;

class function TcxCustomRichEditProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxRichEdit;
end;

function TcxCustomRichEditProperties.GetDisplayText(
  const AEditValue: TcxEditValue; AFullText: Boolean = False;
  AIsInplace: Boolean = True): string;
var
  AValue: string;
  ABytes: TBytes;
begin
  if VarIsArray(AEditValue) and VarIsType(AEditValue[0], varByte) then
  begin
    ABytes := AEditValue;
    AValue := TEncoding.ASCII.GetString(ABytes);
  end
  else
    AValue := VarToStr(AEditValue);
  if (MemoMode or not PlainText) and dxIsRichText(AValue) then
    Result := inherited GetDisplayText(ConvertRichText(AValue), AFullText)
  else
    Result := inherited GetDisplayText(AEditValue, AFullText);
end;

function TcxCustomRichEditProperties.GetEditValueSource(
  AEditFocused: Boolean): TcxDataEditValueSource;
begin
  Result := evsValue;
end;

function TcxCustomRichEditProperties.GetSpecialFeatures: TcxEditSpecialFeatures;
begin
  Result := inherited GetSpecialFeatures + [esfMultiRow];
end;

function TcxCustomRichEditProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := [esoAutoHeight, esoEditing, esoHorzAlignment, esoNeedHandle];
end;

function TcxCustomRichEditProperties.CanValidate: Boolean;
begin
  Result := False;
end;

procedure TcxCustomRichEditProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomRichEditProperties then
    with TcxCustomRichEditProperties(AProperties) do
    begin
      Self.RichEditClass := RichEditClass;
      Self.AllowObjects := AllowObjects;
      Self.AutoURLDetect := AutoURLDetect;
      Self.HideScrollBars := HideScrollBars;
      Self.MemoMode := MemoMode;
      Self.PlainText := PlainText;
      Self.SelectionBar := SelectionBar;
      Self.StreamModes := StreamModes;
      Self.ZoomFactor := ZoomFactor;
      Self.OnQueryInsertObject := OnQueryInsertObject;
      Self.OnProtectChange := OnProtectChange;
      Self.OnResizeRequest := OnResizeRequest;
      Self.OnSaveClipboard := OnSaveClipboard;
      Self.OnSelectionChange := OnSelectionChange;
      Self.OnURLClick := OnURLClick;
      Self.OnURLMove := OnURLMove;
    end;
end;

class function TcxCustomRichEditProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxCustomRichEditViewData;
end;

function TcxCustomRichEditProperties.SupportsMultiThreading: Boolean;
begin
  Result := False;
end;

class function TcxCustomRichEditProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxCustomRichEditViewInfo;
end;

function TcxCustomRichEditProperties.IsResetEditClass: Boolean;
begin
  Result := False;
end;

function TcxCustomRichEditProperties.GetStreamModes: TcxRichEditStreamModes;
begin
  Result := FStreamModes;
end;

function TcxCustomRichEditProperties.IsZoomFactorStored: Boolean;
begin
  Result := ZoomFactor <> 1.0;
end;

procedure TcxCustomRichEditProperties.SetAdvancedTypography(const Value: Boolean);
begin
  if FAdvancedTypography <> Value then
  begin
    FAdvancedTypography := Value;
    Changed;
  end;
end;

procedure TcxCustomRichEditProperties.SetAllowObjects(
  const Value: Boolean);
begin
  if FAllowObjects <> Value then
  begin
    FAllowObjects := Value;
    Changed;
  end;
end;

procedure TcxCustomRichEditProperties.SetAutoURLDetect(
  const Value: Boolean);
begin
  if Value <> FAutoURLDetect then
  begin
    FAutoURLDetect := Value;
    Changed;
  end;
end;

procedure TcxCustomRichEditProperties.SetHideScrollBars(Value: Boolean);
begin
  if Value <> FHideScrollBars then
  begin
    FHideScrollBars := Value;
    Changed;
  end;
end;

procedure TcxCustomRichEditProperties.SetMemoMode(Value: Boolean);
begin
  if Value <> FMemoMode then
  begin
    FMemoMode := Value;
    Changed;
  end;
end;

procedure TcxCustomRichEditProperties.SetPlainText(Value: Boolean);
begin
  if FPlainText <> Value then
  begin
    FPlainText := Value;
    FPlainTextChanged := True;
    try
      Changed;
    finally
      FPlainTextChanged := False;
    end;
  end;
end;

procedure TcxCustomRichEditProperties.SetRichEditClass(AValue: TcxRichEditClass);
begin
  if (AValue <> FRichEditClass) and (AValue >= cxMinVersionRichEditClass) and (AValue <= cxMaxVersionRichEditClass) then
  begin
    FRichEditClass := AValue;
    Changed;
  end;
end;

procedure TcxCustomRichEditProperties.SetSelectionBar(Value: Boolean);
begin
  if Value <> FSelectionBar then
  begin
    FSelectionBar := Value;
    Changed;
  end;
end;

procedure TcxCustomRichEditProperties.SetStreamModes(const Value: TcxRichEditStreamModes);
begin
  if Value <> FStreamModes then
  begin
    FStreamModes := Value;
    Changed;
  end;
end;

procedure TcxCustomRichEditProperties.SetZoomFactor(AValue: Double);
begin
  AValue := CheckZoomFactor(AValue);
  if ZoomFactor <> AValue then
  begin
    FZoomFactor := AValue;
    Changed;
  end;
end;

procedure TcxCustomRichEditProperties.SetOnQueryInsertObject(Value: TcxRichEditQueryInsertObjectEvent);
begin
  FOnQueryInsertObject := Value;
  Changed;
end;

{ TcxCustomRichEditViewInfo }

constructor TcxCustomRichEditViewInfo.Create;
begin
  inherited Create;
  PrevDrawBitmapSize.cx := -1;
  PrevDrawBitmapSize.cy := -1;
end;

destructor TcxCustomRichEditViewInfo.Destroy;
begin
  if DrawBitmap <> 0 then
    DeleteObject(DrawBitmap);
  inherited Destroy;
end;

procedure TcxCustomRichEditViewInfo.DrawNativeStyleEditBackground(
  ACanvas: TcxCanvas; ADrawBackground: Boolean; ABackgroundBrush: TBrushHandle);
begin
  if IsInplace or (BorderStyle = ebsNone) or not IsWinVistaOrLater then
    inherited DrawNativeStyleEditBackground(ACanvas, ADrawBackground, ABackgroundBrush)
  else
    DrawThemeBackground(OpenTheme(totEdit), ACanvas.Handle, EP_EDITTEXT, ETS_NORMAL, Bounds);
end;

procedure TcxCustomRichEditViewInfo.DrawText(ACanvas: TcxCanvas);

  procedure PrepareDrawBitmap;
  var
    ADC: HDC;
    APrevBitmap: HBITMAP;
    ATempVar: Integer;
  begin
    if IsDrawBitmapDirty then
    begin
      if (DrawBitmap = 0) or (PrevDrawBitmapSize.cx <> cxRectWidth(TextRect)) or (PrevDrawBitmapSize.cy <> cxRectHeight(TextRect)) then
      begin
        if DrawBitmap <> 0 then
          DeleteObject(DrawBitmap);
        DrawBitmap := CreateCompatibleBitmap(ACanvas.Handle, cxRectWidth(TextRect), cxRectHeight(TextRect));
      end;
      ADC := CreateCompatibleDC(ACanvas.Handle);
      APrevBitmap := 0;
      try
        APrevBitmap := SelectObject(ADC, DrawBitmap);
        cxDrawRichEdit(ADC, TextRect, Text, TcxCustomRichEditProperties(EditProperties),
          Font, BackgroundColor, TextColor, False, ATempVar, ScaleFactor);
      finally
        if APrevBitmap <> 0 then
          SelectObject(ADC, APrevBitmap);
        DeleteDC(ADC);
      end;
      IsDrawBitmapDirty := False;
    end;
  end;

var
  ADC: HDC;
  APrevBitmap: HBITMAP;
begin
  PrepareDrawBitmap;
  ADC := CreateCompatibleDC(ACanvas.Handle);
  APrevBitmap := 0;
  try
    APrevBitmap := SelectObject(ADC, DrawBitmap);
    with TextRect do
      BitBlt(ACanvas.Handle, Left, Top, Right - Left, Bottom - Top, ADC, 0, 0, SRCCOPY);
  finally
    if APrevBitmap <> 0 then
      SelectObject(ADC, APrevBitmap);
    DeleteDC(ADC);
  end;
end;

function TcxCustomRichEditViewInfo.GetBackgroundPaintingStyle: TcxEditBackgroundPaintingStyle;
begin
  Result := bpsComboListEdit;
end;

function TcxCustomRichEditViewInfo.GetUpdateRegion(AViewInfo: TcxContainerViewInfo): TcxRegion;
begin
  Result := TcxRegion.Create(Self.Bounds);
end;

function TcxCustomRichEditViewInfo.NeedShowHint(ACanvas: TcxCanvas;
  const P: TPoint; const AVisibleBounds: TRect; out AText: TCaption;
  out AIsMultiLine: Boolean; out ATextRect: TRect; AMaxLineCount: Integer = 0): Boolean;
begin
  Result := False;
end;

procedure TcxCustomRichEditViewInfo.Paint(ACanvas: TcxCanvas);
begin
  ACanvas.Lock;
  try
    if IsInplace and not Focused or IsDBEditPaintCopyDrawing then
    begin
      DrawText(ACanvas);
      ACanvas.ExcludeClipRect(TextRect);
    end;
    DrawCustomEdit(ACanvas, True, True);
  finally
    ACanvas.Unlock;
  end;
end;

function TcxCustomRichEdit.GetActiveProperties: TcxCustomRichEditProperties;
begin
  Result := TcxCustomRichEditProperties(InternalGetActiveProperties);
end;

function TcxCustomRichEdit.GetProperties: TcxCustomRichEditProperties;
begin
  Result := TcxCustomRichEditProperties(inherited Properties);
end;

function TcxCustomRichEdit.GetRichVersion: Integer;
begin
  Result := InnerRich.RichVersion;
end;

procedure TcxCustomRichEdit.SetProperties(Value: TcxCustomRichEditProperties);
begin
  Properties.Assign(Value);
end;

function TcxCustomRichEdit.GetCanUndo: Boolean;
begin
  Result := InnerRich.CanUndo;
end;

procedure TcxCustomRichEdit.Initialize;
begin
  inherited Initialize;
  InnerRich.OnProtectChange := DoProtectChange;
  InnerRich.OnSaveClipboard := DoSaveClipboard;
  Width := 185;
  Height := 89;
  FIsNullEditValue := True;
end;

procedure TcxCustomRichEdit.InitializeInnerEdit;
begin
  inherited InitializeInnerEdit;
  if not IsInplace then
    InnerRich.ParentFont := False;
end;

procedure TcxCustomRichEdit.InternalSetEditValue(const Value: TcxEditValue;
  AValidateEditValue: Boolean);
begin
  LockChangeEvents(True);
  try
    if HandleAllocated then
      SendMessage(InnerRich.Handle, WM_SETREDRAW, 0, 0);
    try
      InnerEdit.EditValue := Value;

    if IsUserAction then
      ModifiedAfterEnter := True
    else
      EditModified := False;

      FIsNullEditValue := VarIsNull(Value);
    finally
      if Parent <> nil then
      begin
        if HandleAllocated then
          SendMessage(InnerRich.Handle, WM_SETREDRAW, 1, 0);
        InnerRich.Invalidate;
      end;
    end;
  finally
    LockChangeEvents(False);
  end;
end;

procedure TcxCustomRichEdit.InternalValidateDisplayValue(const ADisplayValue: TcxEditValue);
begin
end;

procedure TcxCustomRichEdit.DoTextChanged;
var
  ALineCount: Integer;
begin
  inherited DoTextChanged;
  if InnerControl.HandleAllocated and Assigned(TdxSpellCheckerInstance.ISpellChecker) then
  begin
    ALineCount := InnerRich.Lines.Count;
    if FLastLineCount <> ALineCount then
    begin
      FLastLineCount := ALineCount;
      Invalidate;
    end;
  end;
end;

procedure TcxCustomRichEdit.PropertiesChanged(Sender: TObject);
begin
  InnerRich.HideScrollBars := ActiveProperties.HideScrollBars and not IsPopupScrollBars;
  InnerRich.MemoMode := ActiveProperties.MemoMode;
  InnerRich.PlainText := ActiveProperties.PlainText or InnerRich.MemoMode;
  InnerRich.SelectionBar := ActiveProperties.SelectionBar;
  InnerRich.AdvancedTypography := ActiveProperties.AdvancedTypography;
  InnerRich.AutoURLDetect := ActiveProperties.AutoURLDetect;
  InnerRich.AllowObjects := ActiveProperties.AllowObjects;
  InnerRich.RichEditClass := ActiveProperties.RichEditClass;
  InnerRich.StreamModes := ActiveProperties.StreamModes;
  InnerRich.ZoomFactor := ActiveProperties.ZoomFactor;
  InnerRich.OnQueryInsertObject := ActiveProperties.OnQueryInsertObject;
  if not(IsInplace or IsDBEdit) then
    FPropertiesChange := True;
  try
    inherited PropertiesChanged(Sender);
  finally
    FPropertiesChange := False;
  end;
end;

procedure TcxCustomRichEdit.ResetEditValue;
begin
  if not IsInplace and IsDBEdit then
    Reset;
end;

procedure TcxCustomRichEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  RefreshScrollBars;
end;

procedure TcxCustomRichEdit.SynchronizeDisplayValue;
begin
end;

procedure TcxCustomRichEdit.SynchronizeEditValue;
begin
end;

function TcxCustomRichEdit.GetDefAttributes: TTextAttributes;
begin
  Result := InnerRich.DefAttributes;
end;

function TcxCustomRichEdit.GetDefaultConverter: TConversionClass;
begin
  Result := InnerRich.DefaultConverter;
end;

function TcxCustomRichEdit.GetPageRect: TRect;
begin
  Result := InnerRich.PageRect;
end;

function TcxCustomRichEdit.GetParagraph: TParaAttributes;
begin
  Result := InnerRich.Paragraph;
end;

function TcxCustomRichEdit.GetSelAttributes: TTextAttributes;
begin
  if ActiveProperties.MemoMode then
    Result := InnerRich.DefAttributes
  else
    Result := InnerRich.SelAttributes;
end;

procedure TcxCustomRichEdit.RefreshScrollBars;
begin
  if HandleAllocated then
  begin
    if not NeedsScrollBars then
      DoLayoutChanged
    else
    begin
      cxRedrawNCRect(InnerRich.Handle, GetControlRect(InnerRich));
      SetScrollBarsParameters;
      MainScrollBars.Invalidate;
    end;
  end;
end;

procedure TcxCustomRichEdit.SetDefAttributes(const Value: TTextAttributes);
begin
  InnerRich.DefAttributes := Value;
end;

procedure TcxCustomRichEdit.SetDefAttributes2(const Value: TcxTextAttributes2);
begin
  FDefAttributes2.Assign(Value);
end;

procedure TcxCustomRichEdit.SetDefaultConverter(Value: TConversionClass);
begin
  InnerRich.DefaultConverter := Value;
end;

procedure TcxCustomRichEdit.SetPageRect(const Value: TRect);
begin
  InnerRich.PageRect := Value;
end;

procedure TcxCustomRichEdit.SetSelAttributes2(Value: TcxTextAttributes2);
begin
  FSelAttributes2.Assign(Value);
end;

procedure TcxCustomRichEdit.SetSelAttributes(const Value: TTextAttributes);
begin
  InnerRich.SelAttributes := Value;
end;

procedure TcxCustomRichEdit.EMCanPaste(var Message: TMessage);
begin
  InnerRich.Dispatch(Message);
end;

procedure TcxCustomRichEdit.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  RefreshScrollBars;
end;

procedure TcxCustomRichEdit.WMSetFont(var Message: TWMSetFont);
begin
  inherited;
  if not IsScaleChanging then
    DefAttributes.Assign(VisibleFont);
end;

procedure TcxCustomRichEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if not IsScaleChanging then
    DefAttributes.Assign(VisibleFont);
end;

function TcxCustomRichEdit.UpdateContentOnFocusChanging: Boolean;
begin
  Result := False;
end;

procedure TcxCustomRichEdit.AdjustVisibleFontHeight(AVisibleFont: TFont);
begin
  inherited;
  AVisibleFont.Height := dxSystemScaleFactor.Apply(AVisibleFont.Height, ScaleFactor);
end;

function TcxCustomRichEdit.CanDeleteSelection: Boolean;
begin
  Result := (SelLength > 0) and CanModify;
end;

procedure TcxCustomRichEdit.Changed(Sender: TObject);
begin
  DoEditing;
end;

procedure TcxCustomRichEdit.DoOnResizeRequest(const R: TRect);
begin
  with Properties do
    if Assigned(OnResizeRequest) then
      OnResizeRequest(Self, R);
  if RepositoryItem <> nil then
    with ActiveProperties do
      if Assigned(OnResizeRequest) then
        OnResizeRequest(Self, R);
end;

procedure TcxCustomRichEdit.DoOnSelectionChange;
begin
  with Properties do
    if Assigned(OnSelectionChange) then
      OnSelectionChange(Self);
  if RepositoryItem <> nil then
    with ActiveProperties do
      if Assigned(OnSelectionChange) then
        OnSelectionChange(Self);
end;

function TcxCustomRichEdit.GetEditPopupMenuInstance: TComponent;

  function NewItem(const ACaption: string; ATag: Integer): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    with Result do
    begin
      Caption := ACaption;
      Tag := ATag;
      OnClick := EditPopupMenuClick;
    end;
  end;

var
  APopupMenu: TPopupMenu;
begin
  if Assigned(FEditPopupMenu) then
  begin
    Result := FEditPopupMenu;
    Exit;
  end;
  APopupMenu := TPopupMenu.Create(Self);
  FEditPopupMenu := APopupMenu;
  APopupMenu.Items.Add(
    NewItem(cxGetResourceString(@cxSEditRichEditUndoCaption), -1));
  APopupMenu.Items.Add(
    NewItem(cxGetResourceString(@cxSEditRichEditRedoCaption), -2));
  APopupMenu.Items.Add(NewItem('-', MaxInt));
  APopupMenu.Items.Add(
    NewItem(cxGetResourceString(@cxSEditRichEditCutCaption), -3));
  APopupMenu.Items.Add(
    NewItem(cxGetResourceString(@cxSEditRichEditCopyCaption), -4));
  APopupMenu.Items.Add(
    NewItem(cxGetResourceString(@cxSEditRichEditPasteCaption), -5));
  APopupMenu.Items.Add(
    NewItem(cxGetResourceString(@cxSEditRichEditDeleteCaption), -6));
  APopupMenu.Items.Add(NewItem('-', MaxInt));
  APopupMenu.Items.Add(
    NewItem(cxGetResourceString(@cxSEditRichEditSelectAllCaption), -7));
  Result := APopupMenu;
end;

function TcxCustomRichEdit.IsNavigationKey(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := (((Key = VK_UP) or (Key = VK_DOWN) or
    (Key = VK_LEFT) or (Key = VK_RIGHT)) and (Shift = [])) or
    (Key = VK_NEXT) or (Key = VK_PRIOR) or (Key = VK_HOME) or (Key = VK_END);
end;

procedure TcxCustomRichEdit.UpdateEditPopupMenuItems(APopupMenu: TComponent);

  procedure UpdateItems(APopupMenu: TPopupMenu);
  begin
    APopupMenu.Items[0].Enabled := InnerRich.CanUndo and
      ((ActiveProperties.RichEditClass >= recRichEdit20) or not InnerRich.CanRedo);
    APopupMenu.Items[1].Enabled := InnerRich.CanRedo;
    APopupMenu.Items[3].Enabled := CanDeleteSelection;
    APopupMenu.Items[4].Enabled := InnerRich.SelLength > 0;
    APopupMenu.Items[5].Enabled := InnerRich.CanPaste;
    APopupMenu.Items[6].Enabled := CanDeleteSelection;
    APopupMenu.Items[8].Enabled := True;
  end;

begin
  if not (APopupMenu is TPopupMenu) then
    Exit;
  InnerRich.ReadOnly := not DataBinding.CanModify or ActiveProperties.ReadOnly;
  UpdateItems(TPopupMenu(APopupMenu));
  InnerRich.ReadOnly := RealReadOnly; // !!! ReadOnly must be True in DBRichEdit while DataSet is not in EditMode (for AddictSpellChecker)
end;

procedure TcxCustomRichEdit.IMEComposition(var AMessage: TMessage);
begin
  AMessage.Result := SendMessage(InnerRich.Handle, AMessage.Msg, AMessage.WParam, AMessage.LParam);
end;

procedure TcxCustomRichEdit.IMEStartComposition;
begin
  SendMessage(InnerRich.Handle, WM_IME_STARTCOMPOSITION, 0, 0);
end;

procedure TcxCustomRichEdit.ClearSelection;
begin
  InnerRich.ClearSelection;
end;

procedure TcxCustomRichEdit.CutToClipboard;
begin
  InnerRich.CutToClipboard;
end;

procedure TcxCustomRichEdit.DoScrollUIModeChanged;
begin
  if not FIsCreating then
    ActiveProperties.Changed;
end;

function TcxCustomRichEdit.FindText(const ASearchStr: string;
  AStartPos, ALength: Integer; AOptions: TSearchTypes; AForward: Boolean = True): Integer;
begin
  Result := InnerRich.FindText(ASearchStr, AStartPos, ALength, AOptions, AForward);
end;

class function TcxCustomRichEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomRichEditProperties;
end;

procedure TcxCustomRichEdit.InsertTable(AColumnCount, ARowCount: Integer; AParams: TcxRichEditTableParams);
begin
  AParams.InsertTable(Self, AColumnCount, ARowCount);
end;

procedure TcxCustomRichEdit.PasteFromClipboard;
begin
  InnerRich.PasteFromClipboard;
end;

procedure TcxCustomRichEdit.PrepareEditValue(const ADisplayValue: TcxEditValue;
  out EditValue: TcxEditValue; AEditFocused: Boolean);
var
  AStream: TStringStream;
begin
  if ActiveProperties.MemoMode or ActiveProperties.PlainText then
    EditValue := InnerRich.Text
  else
    if (Parent = nil) or not Parent.HandleAllocated then
      InnerRich.RestoreEditValue
    else
    begin
      AStream := TStringStream.Create('');
      try
        Lines.SaveToStream(AStream);
        EditValue := AStream.DataString;
      finally
        AStream.Free;
      end;
    end;
end;

procedure TcxCustomRichEdit.Print(const Caption: string);
begin
  InnerRich.Print(Caption);
end;

procedure TcxCustomRichEdit.SaveSelectionToStream(Stream: TStream);
begin
  TcxRichEditStrings(Lines).SaveSelectionToStream(Stream);
end;

procedure TcxCustomRichEdit.Undo;
begin
  InnerRich.Undo;
end;

function TcxCustomRichEdit.InsertObject: Boolean;
begin
  Result := InnerRich.InsertObject;
end;

function TcxCustomRichEdit.PasteSpecial: Boolean;
begin
  Result := InnerRich.PasteSpecial;
end;

function TcxCustomRichEdit.ShowObjectProperties: Boolean;
begin
  Result := InnerRich.ShowObjectProperties;
end;

class procedure TcxCustomRichEdit.RegisterConversionFormat(
  const AExtension: string; AConversionClass: TConversionClass);
var
  AConversionFormat: PcxConversionFormat;
begin
  New(AConversionFormat);
  with AConversionFormat^ do
  begin
  {$IFDEF DELPHI15}
    Extension := AnsiLowerCase(AExtension);
  {$ELSE}
    Extension := AnsiLowerCaseFileName(AExtension);
  {$ENDIF}
    ConversionClass := AConversionClass;
    Next := FConversionFormatList;
  end;
  FConversionFormatList := AConversionFormat;
  TCustomRichEdit.RegisterConversionFormat(AExtension, AConversionClass);
end;

procedure Initialize;
begin
  GetRegisteredEditProperties.Register(TcxRichEditProperties,
    cxGetResourceString(@scxSEditRepositoryRichEditItem));
  CFObjectDescriptor := RegisterClipboardFormat('Object Descriptor');
  CFEmbeddedObject := RegisterClipboardFormat('Embedded Object');
  CFLinkSource := RegisterClipboardFormat('Link Source');
  CFRtf := RegisterClipboardFormat(CF_RTF);
  CFRETextObj := RegisterClipboardFormat(CF_RETEXTOBJ);
end;

procedure Finalize;
begin
  FreeAndNil(FRichRenderer);
  FreeAndNil(FRichConverter);
  GetRegisteredEditProperties.Unregister(TcxRichEditProperties);
  if FRichEditLibrary <> 0 then
    FreeLibrary(FRichEditLibrary);
  ReleaseConversionFormatList;
end;

initialization
   dxUnitsLoader.AddUnit(@Initialize, @Finalize);

finalization
  dxUnitsLoader.RemoveUnit(@Finalize);

end.
