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
unit cxNavigator;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Messages, Windows, Classes, Forms, Controls, Graphics, ImgList,
  StdCtrls, SysUtils,
  dxCoreClasses, cxClasses, cxContainer, cxControls, cxFilter, cxGraphics, cxGeometry,
  cxLookAndFeels, cxLookAndFeelPainters, dxThemeManager, dxCustomHint, dxFading;

const
  NavigatorButtonCount = 16;

  NBDI_FIRST        = 0;
  NBDI_PRIORPAGE    = 1;
  NBDI_PRIOR        = 2;
  NBDI_NEXT         = 3;
  NBDI_NEXTPAGE     = 4;
  NBDI_LAST         = 5;
  NBDI_INSERT       = 6;
  NBDI_APPEND       = 7;
  NBDI_DELETE       = 8;
  NBDI_EDIT         = 9;
  NBDI_POST         = 10;
  NBDI_CANCEL       = 11;
  NBDI_REFRESH      = 12;
  NBDI_SAVEBOOKMARK = 13;
  NBDI_GOTOBOOKMARK = 14;
  NBDI_FILTER       = 15;

type
  TcxCustomNavigator = class;
  TcxNavigatorViewInfo = class;
  TcxCustomNavigatorButtons = class;
  TcxCustomNavigatorInfoPanel = class;

  TcxNavigatorChangeType = (nctProperties, nctSize, nctLayout);

  IcxNavigatorOwner = interface
  ['{504B7F43-8847-46C5-B84A-C24F8E5E61A6}']
    procedure NavigatorChanged(AChangeType: TcxNavigatorChangeType);
    function GetNavigatorBounds: TRect;
    function GetNavigatorButtons: TcxCustomNavigatorButtons;
    function GetNavigatorCanvas: TCanvas;
    function GetNavigatorControl: TWinControl;
    function GetNavigatorFocused: Boolean;
    function GetNavigatorLookAndFeel: TcxLookAndFeel;
    function GetNavigatorOwner: TComponent;
    function GetNavigatorShowHint: Boolean;
    function GetNavigatorTabStop: Boolean;
    procedure NavigatorStateChanged;
    procedure RefreshNavigator;
  end;

  IcxNavigatorOwner2 = interface(IcxNavigatorOwner)
  ['{5AC3BE65-B332-40D4-9635-869F52634B17}']
    function GetNavigatorInfoPanel: TcxCustomNavigatorInfoPanel;
  end;

  TcxNavigatorControlNotifier = class
  private
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddNavigator(ANavigator: IcxNavigatorOwner);
    procedure RemoveNavigator(ANavigator: IcxNavigatorOwner);
    procedure RefreshNavigatorButtons;
  end;

  IcxNavigator = interface
  ['{A15F80CA-DE56-47CB-B0EB-035D0BF90E9D}']
    function CanAppend: Boolean;
    function CanDelete: Boolean;
    function CanEdit: Boolean;
    function CanInsert: Boolean;
    function IsActive: Boolean;
    function IsBof: Boolean;
    function IsBookmarkAvailable: Boolean;
    function IsEditing: Boolean;
    function IsEof: Boolean;
    procedure ClearBookmark;
    procedure DoAction(AButtonIndex: Integer);
    function GetNotifier: TcxNavigatorControlNotifier;
    function IsActionSupported(AButtonIndex: Integer): Boolean;
  end;

  IcxNavigatorRecordPosition = interface
  ['{715C9E38-5BA0-4ED8-B35C-BB40EA739362}']
    function GetRecordCount: Integer;
    function GetRecordIndex: Integer;
  end;

  TcxNavigatorButton = class(TPersistent)
  private
    FButtons: TcxCustomNavigatorButtons;
    FDefaultIndex: Integer;
    FDefaultVisible: Boolean;
    FEnabled: Boolean;
    FHint: string;
    FImageIndex: TcxImageIndex;
    FIsVisibleAssigned: Boolean;
    FVisible: Boolean;
    FOnClick: TNotifyEvent;
    function GetInternalImageIndex: Integer;
    function GetInternalImages: TCustomImageList;
    procedure InternalSetVisible(Value: Boolean; AIsInternalSetting: Boolean = True);
    function IsVisibleStored: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetHint(const Value: string);
    procedure SetImageIndex(Value: TcxImageIndex);
    procedure SetOnClick(const Value: TNotifyEvent);
    procedure SetVisible(const Value: Boolean);
  protected
    function GetAdornerTargetElementName: string; virtual;
    function GetOwner: TPersistent; override;
    function GetInternalEnabled: Boolean;
    function GetInternalHint: string;
    function HasImage: Boolean;
    function IsCustomButton: Boolean;
    function IsUserImageListUsed: Boolean;
    procedure DoClick; dynamic;
    procedure RestoreDefaultVisible(ACanBeVisible: Boolean);

    function GetNavigator: IcxNavigatorOwner;
    function GetScaleFactor: TdxScaleFactor;

    property DefaultIndex: Integer read FDefaultIndex write FDefaultIndex;
    property InternalImageIndex: Integer read GetInternalImageIndex;
    property InternalImages: TCustomImageList read GetInternalImages;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(AButtons: TcxCustomNavigatorButtons;
      ADefaultVisible: Boolean);

    procedure Assign(Source: TPersistent); override;
    procedure Click;
    function GetImageSize: TSize;

    property Buttons: TcxCustomNavigatorButtons read FButtons;
    property Navigator: IcxNavigatorOwner read GetNavigator;
    property OnClick: TNotifyEvent read FOnClick write SetOnClick;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Hint: string read FHint write SetHint;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex default -1;
    property Visible: Boolean read FVisible write SetVisible stored IsVisibleStored;
  end;

  { TcxNavigatorChildComponent }

  TcxNavigatorChildComponent = class(TPersistent)
  strict private
    FScaleFactor: TdxScaleFactor;
    FNavigator: IcxNavigatorOwner;
    procedure ScaleFactorChangeHandler(Sender: TObject; M, D: Integer; IsLoading: Boolean);
  protected
    procedure Changed(AChangeType: TcxNavigatorChangeType; ANeedRefresh: Boolean = False);
    function  GetOwner: TPersistent; override;
    function IsNavigatorEnabled: Boolean;
    procedure ScaleFactorChanged(M, D: Integer); virtual;
  public
    constructor Create(ANavigator: IcxNavigatorOwner); virtual;
    destructor Destroy; override;

    property Navigator: IcxNavigatorOwner read FNavigator;
  end;

  TcxNavigatorCustomButton = class(TCollectionItem)
  private
    FButton: TcxNavigatorButton;
    function GetEnabled: Boolean;
    function GetHint: string;
    function GetImageIndex: TcxImageIndex;
    function GetVisible: Boolean;
    procedure SetButton(const Value: TcxNavigatorButton);
    procedure SetEnabled(const Value: Boolean);
    procedure SetHint(const Value: string);
    procedure SetImageIndex(const Value: TcxImageIndex);
    procedure SetVisible(const Value: Boolean);
  protected
    property Button: TcxNavigatorButton read FButton write SetButton;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property Hint: string read GetHint write SetHint;
    property ImageIndex: TcxImageIndex read GetImageIndex write SetImageIndex default -1;
    property Visible: Boolean read GetVisible write SetVisible default True;
  end;

  TcxNavigatorCustomButtons = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TcxNavigatorCustomButton;
    procedure SetItem(Index: Integer; Value: TcxNavigatorCustomButton);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TcxNavigatorCustomButton;

    property Items[Index: Integer]: TcxNavigatorCustomButton read GetItem write SetItem; default;
  end;

  { TcxCustomNavigatorButtons }

  TcxNavigatorButtonClickEvent = procedure(Sender: TObject; AButtonIndex: Integer;
    var ADone: Boolean) of object;

  TcxCustomNavigatorButtons = class(TcxNavigatorChildComponent)
  private
    FButtons: TList;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FConfirmDelete: Boolean;
    FCustomButtons: TcxNavigatorCustomButtons;
    FOnButtonClick: TcxNavigatorButtonClickEvent;

    function GetButton(Index: Integer): TcxNavigatorButton;
    function GetButtonCount: Integer;
    function GetDefaultImages: TCustomImageList;
    procedure SetButton(Index: Integer; const Value: TcxNavigatorButton);
    procedure SetConfirmDelete(const Value: Boolean);
    procedure SetCustomButtons(const Value: TcxNavigatorCustomButtons);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetOnButtonClick(const Value: TcxNavigatorButtonClickEvent);
    procedure ImageListChange(Sender: TObject);
  protected
    procedure CreateButtons; virtual;
    procedure DestroyButtons; virtual;

    procedure CustomButtonsChanged;
    procedure DoButtonClick(ADefaultIndex: Integer); virtual;
    function GetAdornerTargetElementName(ADefaultIndex: Integer): string; virtual;
    function GetButtonEnabled(ADefaultIndex: Integer): Boolean; virtual;
    function GetButtonHint(ADefaultIndex: Integer): string; virtual;
    function GetButtonImageOffset: Integer; virtual;
    function IsButtonVisibleByDefault(AIndex: Integer): Boolean; virtual;

    property ConfirmDelete: Boolean read FConfirmDelete write SetConfirmDelete default True;

    property First: TcxNavigatorButton index NBDI_FIRST read GetButton write SetButton;
    property PriorPage: TcxNavigatorButton index NBDI_PRIORPAGE read GetButton write SetButton;
    property Prior: TcxNavigatorButton index NBDI_PRIOR read GetButton write SetButton;
    property Next: TcxNavigatorButton index NBDI_NEXT read GetButton write SetButton;
    property NextPage: TcxNavigatorButton index NBDI_NEXTPAGE read GetButton write SetButton;
    property Last: TcxNavigatorButton index NBDI_LAST read GetButton write SetButton;
    property Insert: TcxNavigatorButton index NBDI_INSERT read GetButton write SetButton;
    property Append: TcxNavigatorButton index NBDI_APPEND read GetButton write SetButton;
    property Delete: TcxNavigatorButton index NBDI_DELETE read GetButton write SetButton;
    property Edit: TcxNavigatorButton index NBDI_EDIT read GetButton write SetButton;
    property Post: TcxNavigatorButton index NBDI_POST read GetButton write SetButton;
    property Cancel: TcxNavigatorButton index NBDI_CANCEL read GetButton write SetButton;
    property CustomButtons: TcxNavigatorCustomButtons read FCustomButtons write SetCustomButtons;
    property Refresh: TcxNavigatorButton index NBDI_REFRESH read GetButton write SetButton;
    property SaveBookmark: TcxNavigatorButton index NBDI_SAVEBOOKMARK read GetButton write SetButton;
    property GotoBookmark: TcxNavigatorButton index NBDI_GOTOBOOKMARK read GetButton write SetButton;
    property Filter: TcxNavigatorButton index NBDI_FILTER read GetButton write SetButton;
  public
    constructor Create(ANavigator: IcxNavigatorOwner); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ClickButton(Index: Integer);
    procedure ReleaseBookmark; virtual;

    property ButtonCount: Integer read GetButtonCount;
    property Buttons[Index: Integer]: TcxNavigatorButton read GetButton; default;
    property DefaultImages: TCustomImageList read GetDefaultImages;
    property Images: TCustomImageList read FImages write SetImages;
  published
    property OnButtonClick: TcxNavigatorButtonClickEvent read FOnButtonClick
      write SetOnButtonClick;
  end;

  { TcxCustomNavigatorInfoPanel }

  TcxInfoPanelGetIRecordPosition = function: IcxNavigatorRecordPosition of object;
  TcxNavigatorInfoPanelClickEvent = procedure (Sender: TcxCustomNavigatorInfoPanel;
    Button: TMouseButton) of object;

  TcxCustomNavigatorInfoPanel = class(TcxNavigatorChildComponent)
  private
    FDisplayMask: string;
    FFormatString: string;
    FIsNeedRecordCount: Boolean;
    FIsNeedRecordIndex: Boolean;
    FVisible: Boolean;
    FOnClick: TcxNavigatorInfoPanelClickEvent;
    FOnGetIRecordPosition: TcxInfoPanelGetIRecordPosition;
    FWidth: Integer;
    function GetIRecordPosition: IcxNavigatorRecordPosition;
    function IsDisplayMaskStored: Boolean;
    procedure SetDisplayMask(const Value: string);
    procedure SetVisible(Value: Boolean);
    procedure SetWidth(const Value: Integer);
  protected
    procedure BuildFormatString;
    procedure DoClick(Button: TMouseButton); virtual;
    function GetAdornerTargetElementVisible: Boolean; virtual;
    function GetViewParams: TcxViewParams; virtual; abstract;
    procedure ScaleFactorChanged(M, D: Integer); override;

    property DisplayMask: string read FDisplayMask write SetDisplayMask stored IsDisplayMaskStored;
    property IsNeedRecordIndex: Boolean read FIsNeedRecordIndex;
    property IsNeedRecordCount: Boolean read FIsNeedRecordCount;
    property IRecordPosition: IcxNavigatorRecordPosition read GetIRecordPosition;
    property Visible: Boolean read FVisible write SetVisible default False;
    property Width: Integer read FWidth write SetWidth default 0;
  public
    constructor Create(ANavigator: IcxNavigatorOwner); override;
    procedure Assign(Source: TPersistent); override;
    function GetFormattedText: string; virtual;

    property OnGetIRecordPosition: TcxInfoPanelGetIRecordPosition
      read FOnGetIRecordPosition write FOnGetIRecordPosition;
  published
    property OnClick: TcxNavigatorInfoPanelClickEvent read FOnClick write FOnClick;
  end;

  { TcxNavigatorButtonViewInfo }

  TcxNavigatorButtonViewInfo = class(TcxIUnknownObject,
    IcxHintableObject,
    IdxFadingObject,
    IdxAdornerTargetElement)
  private
    FOwner: TcxNavigatorViewInfo;
    function GetNeedDrawFocusRect: Boolean;
    function GetPainter: TcxCustomLookAndFeelPainter;
  protected
    FSize: TSize;
    FState: TcxButtonState;
    function CalculateState: TcxButtonState; virtual;
    procedure CorrectButtonBounds(var R: TRect); virtual;
    procedure DrawContent(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); virtual;
    procedure DrawGlyph(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); virtual;
    // IdxFadingObject
    function CanFade: Boolean;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);
    procedure IdxFadingObject.DrawFadeImage = Invalidate;
    // IcxHintableObject
    function HasHintPoint(const P: TPoint): Boolean;
    function IsHintAtMousePos: Boolean;
    function UseHintHidePause: Boolean;
    // IdxAdornerTargetElement
    function IdxAdornerTargetElement.GetControl = GetAdornerTargetElementControl;
    function GetAdornerTargetElementControl: TWinControl; virtual;
    function IdxAdornerTargetElement.GetBounds = GetAdornerTargetElementBounds;
    function GetAdornerTargetElementBounds: TRect; virtual;
    function GetAdornerTargetElementName: string; virtual;
    function IdxAdornerTargetElement.GetVisible = GetAdornerTargetElementVisible;
    function GetAdornerTargetElementVisible: Boolean; virtual;
    //
    property NeedDrawFocusRect: Boolean read GetNeedDrawFocusRect;
  public
    Button: TcxNavigatorButton;
    Bounds: TRect;
    Enabled: Boolean;
    Hint: string;
    constructor Create(AOwner: TcxNavigatorViewInfo); virtual;
    destructor Destroy; override;
    procedure Calculate; virtual;
    procedure Draw(ACanvas: TcxCanvas); virtual;
    procedure Invalidate;
    procedure UpdateState;
    //
    property Owner: TcxNavigatorViewInfo read FOwner;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property Size: TSize read FSize;
    property State: TcxButtonState read FState;
  end;

  { TcxNavigatorButtonsViewInfo }

  TcxNavigatorButtonsViewInfo = class(TdxFastObjectList)
  strict private
    FSize: TSize;

    function GetItem(Index: Integer): TcxNavigatorButtonViewInfo;
  public
    procedure AddAdornerTargetElements(AList: TStrings); virtual;
    procedure CalculateSize;

    property Items[Index: Integer]: TcxNavigatorButtonViewInfo read GetItem; default;
    property Size: TSize read FSize;
  end;

  { TcxCustomNavigatorInfoPanelViewInfo }

  TcxCustomNavigatorInfoPanelViewInfo = class(TcxIUnknownObject,
    IdxAdornerTargetElement)
  private
    FText: string;
    FBounds: TRect;
    FNavigatorViewInfo: TcxNavigatorViewInfo;
    function GetIsPaintOnGlass: Boolean;
    function GetNavigatorOwner: IcxNavigatorOwner2;
  protected
    // IdxAdornerTargetElement
    function IdxAdornerTargetElement.GetControl = GetAdornerTargetElementControl;
    function GetAdornerTargetElementControl: TWinControl; virtual;
    function IdxAdornerTargetElement.GetBounds = GetAdornerTargetElementBounds;
    function GetAdornerTargetElementBounds: TRect; virtual;
    function IdxAdornerTargetElement.GetVisible = GetAdornerTargetElementVisible;
    function GetAdornerTargetElementVisible: Boolean; virtual;

    procedure DoRightToLeftConversion(const AClientBounds: TRect);

    property NavigatorOwner: IcxNavigatorOwner2 read GetNavigatorOwner;
    property NavigatorViewInfo: TcxNavigatorViewInfo read FNavigatorViewInfo;
  public
    constructor Create(ANavigatorViewInfo: TcxNavigatorViewInfo);
    procedure Calculate(const ABounds: TRect; const AText: string);
    procedure Paint;

    property Bounds: TRect read FBounds;
    property IsPaintOnGlass: Boolean read GetIsPaintOnGlass;
    property Text: string read FText;
  end;

  { TcxNavigatorHintHelper }

  TcxNavigatorHintHelper = class(TcxControlHintHelper)
  private
    FNavigatorViewInfo: TcxNavigatorViewInfo;
  protected
    procedure CorrectHintWindowRect(var ARect: TRect); override;
    function GetOwnerControl: TcxControl; override;
  public
    constructor Create(ANavigatorViewInfo: TcxNavigatorViewInfo);
    procedure MouseDown; override;
  end;

  { TcxNavigatorViewInfo }

  TcxNavigatorViewInfo = class
  private
    FButtonIndent: Integer;
    FButtonPressTimer: TcxTimer;
    FButtons: TcxNavigatorButtonsViewInfo;
    FCanvas: TcxCanvas;
    FFocusedButton: TcxNavigatorButton;
    FHintHelper: TcxNavigatorHintHelper;
    FHotTrackButtonViewInfo: TcxNavigatorButtonViewInfo;
    FInfoPanelPressed: Boolean;
    FInfoPanelViewInfo: TcxCustomNavigatorInfoPanelViewInfo;
    FIsDirty: Boolean;
    FIsInplace: Boolean;
    FIsSelected: Boolean;
    FIsRightToLeftConverted: Boolean;
    FNavigatorOwner: IcxNavigatorOwner;
    FNavigatorOwner2: IcxNavigatorOwner2;
    FPressedButtonViewInfo: TcxNavigatorButtonViewInfo;

    function GetButtonCount: Integer;
    function GetCanvas: TcxCanvas;
    function GetFocusedButton: TcxNavigatorButtonViewInfo;
    function GetInfoPanel: TcxCustomNavigatorInfoPanel;
    procedure SetFocusedButton(Value: TcxNavigatorButtonViewInfo);
    procedure DoButtonPressTimer(Sender: TObject);
    procedure UpdateSelected;
  protected
    //hint
    procedure CancelHint;
    procedure CheckHint;

    procedure AddAdornerTargetElements(AList: TStrings); virtual;
    procedure CalculateBounds(const AInfoPanelText: string; AInfoPanelWidth, AHeight, AButtonIndent: Integer);
    procedure CalculateButtons;
    function CalculateMinSize: TSize;
    procedure CalculateSize(var AWidth, AHeight: Integer;
      const AInfoPanelText: string; out AButtonIndent, AInfoPanelWidth: Integer);
    function CanButtonOverlapBorder: Boolean; virtual;
    procedure CheckCalculate;
    procedure ClearButtons;
    procedure DoRightToLeftConversion; virtual;
    function FindInfoPanelPositionIndex(const AInfoPanelText: string): Integer; virtual;
    function GetInfoPanelFont: TFont;
    function GetInfoPanelText: string;
    function GetInfoPanelSize(AText: string): TSize;
    function GetMinButtonSize(AButtonIndex: Integer): TSize; virtual;
    function GetScaleFactor: TdxScaleFactor; virtual;
    function HasInfoPanel: Boolean;
    procedure InvalidateButton(AButton: TcxNavigatorButtonViewInfo);
    procedure PaintButton(AButtonIndex: Integer);
    procedure PaintButtons;
    procedure RightToLeftConversion;
    procedure ValidateButtonsViewInfos;

    property Canvas: TcxCanvas read GetCanvas;
    property FocusedButton: TcxNavigatorButtonViewInfo read GetFocusedButton write SetFocusedButton;
    property HotTrackButtonViewInfo: TcxNavigatorButtonViewInfo read FHotTrackButtonViewInfo write FHotTrackButtonViewInfo;
    property InfoPanel: TcxCustomNavigatorInfoPanel read GetInfoPanel;
    property InfoPanelViewInfo: TcxCustomNavigatorInfoPanelViewInfo read FInfoPanelViewInfo;
    property IsInplace: Boolean read FIsInplace;
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted;
    property NavigatorOwner: IcxNavigatorOwner read FNavigatorOwner;
    property NavigatorOwner2: IcxNavigatorOwner2 read FNavigatorOwner2;
    property PressedButtonViewInfo: TcxNavigatorButtonViewInfo read FPressedButtonViewInfo write FPressedButtonViewInfo;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(ANavigator: IcxNavigatorOwner; AIsInplace: Boolean = True); virtual;
    destructor Destroy; override;
    procedure Calculate; virtual;
    procedure CheckSize(var AWidth, AHeight: Integer);
    procedure Clear;
    procedure DoEnter;
    procedure DoExit;
    function GetButtonAt(const pt: TPoint): TcxNavigatorButton;
    function GetButtonViewInfoAt(const pt: TPoint): TcxNavigatorButtonViewInfo;
    function GetButtonViewInfoByButton(AButton: TcxNavigatorButton): TcxNavigatorButtonViewInfo;
    procedure MakeDirty;
    procedure MouseDown(X, Y: Integer);
    procedure MouseLeave;
    procedure MouseMove(X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; X, Y: Integer);
    procedure Paint;
    procedure PaintBorder(const ABounds: TRect);
    procedure PressArrowKey(ALeftKey: Boolean);
    procedure Update;
    procedure UpdateButtonsEnabled;
    procedure UpdateInfoPanel;

    property ButtonCount: Integer read GetButtonCount;
    property ButtonIndent: Integer read FButtonIndent;
    property Buttons: TcxNavigatorButtonsViewInfo read FButtons;
  end;

  TcxNavigatorViewInfoClass = class of TcxNavigatorViewInfo;

  TcxNavigatorBorderStyle = (nbsDefault, nbsNone, nbsAlways);

  TcxNavigatorControlViewInfo = class(TcxNavigatorViewInfo)
  protected
    function CanButtonOverlapBorder: Boolean; override;
  end;

  TcxControlAccess = class(TcxControl);

  TcxInplaceNavigatorViewInfo = class(TcxNavigatorViewInfo)
  private
    FNavigatorSize: TSize;
  protected
    function GetControl: TcxControl; virtual;
    function GetHeight: Integer;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; virtual;
    function GetMinButtonSize(AButtonIndex: Integer): TSize; override;
    function GetNavigatorOffset: Integer; virtual;
    function GetNavigatorSiteBounds: TRect;
    function GetScaleFactor: TdxScaleFactor; override;
    function GetWidth: Integer;
    function IsNavigatorSizeChanged: Boolean;
    procedure UpdateNavigatorSiteBounds(const ABounds: TRect); virtual;
  public
    procedure Calculate; override;
    function GetNavigatorBounds: TRect; virtual;

    property Control: TcxControl read GetControl;
  end;

  TcxCustomNavigator = class(TcxControl,
    IcxNavigatorOwner,
    IcxNavigatorOwner2,
    IcxMouseTrackingCaller,
    IdxSkinSupport)
  private
    FBorderStyle: TcxNavigatorBorderStyle;
    FButtons: TcxCustomNavigatorButtons;
    FInfoPanel: TcxCustomNavigatorInfoPanel;
    FSubClassEvents: TNotifyEvent;
    FViewInfo: TcxNavigatorViewInfo;

    // Scaling
    FSavedButtonIndent: Integer;

    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure SetBorderStyle(const Value: TcxNavigatorBorderStyle);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    //IcxMouseTrackingCaller
    procedure IcxMouseTrackingCaller.MouseLeave = NavigatorMouseLeave;
    procedure NavigatorMouseLeave;
    //IcxNavigatorOwner
    procedure NavigatorChanged(AChangeType: TcxNavigatorChangeType);
    function GetNavigatorBounds: TRect;
    function GetNavigatorButtons: TcxCustomNavigatorButtons;
    function GetNavigatorCanvas: TCanvas;
    function GetNavigatorControl: TWinControl;
    function GetNavigatorFocused: Boolean;
    function GetNavigatorLookAndFeel: TcxLookAndFeel;
    function GetNavigatorOwner: TComponent;
    function GetNavigatorShowHint: Boolean;
    function GetNavigatorTabStop: Boolean;
    //IcxNavigatorOwner2
    function GetNavigatorInfoPanel: TcxCustomNavigatorInfoPanel;
    // IdxAdornerTargetElementCollection
    procedure GetAdornerTargetElements(AList: TStrings); override;

    function CanFocusOnClick: Boolean; override;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    function CreateButtons: TcxCustomNavigatorButtons; virtual;
    function CreateInfoPanel: TcxCustomNavigatorInfoPanel; virtual; abstract;
    procedure CreateWnd; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure EnabledChanged; override;
    procedure FontChanged; override;
    function GetBorderSize: Integer; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WndProc(var Message: TMessage); override;

    procedure ScaleFactorChanging; override;
    procedure ScaleFactorChanged; override;

    procedure CheckSize(var AWidth, AHeight: Integer);
    function GetPaintBlackOpaqueOnGlass: Boolean; override;
    function GetViewInfoClass: TcxNavigatorViewInfoClass; virtual;
    procedure DoPaint; override;
    procedure InitButtons; virtual;
    procedure NavigatorStateChanged;
    procedure RefreshNavigator;
    procedure RestoreLayoutParams;
    procedure StoreLayoutParams;

    property CustomButtons: TcxCustomNavigatorButtons read FButtons;
    property CustomInfoPanel: TcxCustomNavigatorInfoPanel read FInfoPanel;
    property ViewInfo: TcxNavigatorViewInfo read FViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClickButton(Index: Integer);
    procedure RestoreButtons;

    property BorderStyle: TcxNavigatorBorderStyle read FBorderStyle write SetBorderStyle default nbsDefault;
    property LookAndFeel;
  published
    property TabStop default False;
    property ButtonsEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;
    property InfoPanelEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;
  end;

  TcxNavigatorControlButtonsGetControl = function: IcxNavigator of object;

  TcxNavigatorControlButtons = class(TcxCustomNavigatorButtons)
  private
    FOnGetControl: TcxNavigatorControlButtonsGetControl;

    function GetControl: IcxNavigator;
  protected
    procedure DoButtonClick(ADefaultIndex: Integer); override;
    function GetButtonEnabled(ADefaultIndex: Integer): Boolean; override;

    property Control: IcxNavigator read GetControl;
  public
    procedure ReleaseBookmark; override;
    property OnGetControl: TcxNavigatorControlButtonsGetControl read FOnGetControl write FOnGetControl;
  published
    property ConfirmDelete;
    property CustomButtons;
    property Images;

    property First;
    property PriorPage;
    property Prior;
    property Next;
    property NextPage;
    property Last;
    property Insert;
    property Append;
    property Delete;
    property Edit;
    property Post;
    property Cancel;
    property Refresh;
    property SaveBookmark;
    property GotoBookmark;
    property Filter;
  end;

  TcxNavigatorControlButtonsClass = class of TcxNavigatorControlButtons;

  { TcxNavigatorControlInfoPanel }

  TcxNavigatorControlInfoPanel = class(TcxCustomNavigatorInfoPanel)
  private
    procedure SetFont(Value: TFont);
    function GetControl: TcxCustomNavigator;
    function GetFont: TFont;
    function GetParentFont: Boolean;
    function IsFontStored: Boolean;
    procedure SetParentFont(const Value: Boolean);
  protected
    function GetViewParams: TcxViewParams; override;
    procedure ResetFont; virtual;

    property Control: TcxCustomNavigator read GetControl;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property DisplayMask;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
    property ParentFont: Boolean read GetParentFont write SetParentFont default True;
    property Visible;
    property Width;
  end;

  TcxNavigatorControlInfoPanelClass = class of TcxNavigatorControlInfoPanel;

  { TcxCustomNavigatorControl }

  TcxCustomNavigatorControl = class(TcxCustomNavigator)
  private
    FControl: TComponent;
    function GetButtons: TcxNavigatorControlButtons;
    function GetIcxNavigator: IcxNavigator;
    function GetIRecordPosition: IcxNavigatorRecordPosition;
    function GetInfoPanel: TcxNavigatorControlInfoPanel;
    procedure SetButtons(Value: TcxNavigatorControlButtons);
    procedure SetControl(Value: TComponent);
    procedure SetInfoPanel(Value: TcxNavigatorControlInfoPanel);
  protected
    function CreateButtons: TcxCustomNavigatorButtons; override;
    function CreateInfoPanel: TcxCustomNavigatorInfoPanel; override;
    function GetButtonsClass: TcxNavigatorControlButtonsClass; virtual;
    function GetInfoPanelClass: TcxNavigatorControlInfoPanelClass; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure InitButtons; override;

    property Buttons: TcxNavigatorControlButtons read GetButtons write SetButtons;
    property Control: TComponent read FControl write SetControl;
    property InfoPanel: TcxNavigatorControlInfoPanel read GetInfoPanel write SetInfoPanel;
  public
    destructor Destroy; override;
  end;

  TcxNavigator = class(TcxCustomNavigatorControl)
  published
    property BorderStyle;
    property Control;
    property Buttons;
    property InfoPanel;
    property LookAndFeel;

    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Ctl3D;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

function NavigatorImages: TcxImageList;

function cxGetNavigatorFormatString(const ADisplayMask: string;
  out AIsNeedRecordIndex, AIsNeedRecordCount: Boolean): string;

implementation

uses
  Dialogs, Math, RTLConsts,
  dxCore, dxThemeConsts, dxUxTheme, cxLibraryConsts,
  cxEditPaintUtils, cxEditUtils, cxEditConsts, dxDPIAwareUtils, dxTypeHelpers;

type
  TCustomFormAccess = class(TCustomForm);

var
  FNavigatorImages: TcxImageList = nil;

function NavigatorImages: TcxImageList;
begin
  if FNavigatorImages = nil then
  begin
    FNavigatorImages := TcxImageList.Create(nil);
    FNavigatorImages.Height := 11;
    FNavigatorImages.Width := 11;
    FNavigatorImages.LoadImage(HInstance, 'CXNAVIGATORBUTTONS', $009999, 11);
  end;
  Result := FNavigatorImages;
end;

function cxGetNavigatorFormatString(const ADisplayMask: string;
  out AIsNeedRecordIndex, AIsNeedRecordCount: Boolean): string;
begin
  Result := StringReplace(ADisplayMask, '%', '%%', [rfReplaceAll]);
  AIsNeedRecordIndex := AnsiPos('[RecordIndex]', ADisplayMask) > 0;
  if AIsNeedRecordIndex then
    Result := StringReplace(Result, '[RecordIndex]', '%0:s', [rfReplaceAll, rfIgnoreCase]);
  AIsNeedRecordCount := AnsiPos('[RecordCount]', ADisplayMask) > 0;
  if AIsNeedRecordCount then
    Result := StringReplace(Result, '[RecordCount]', '%1:s', [rfReplaceAll, rfIgnoreCase]);
end;

{ TcxNavigatorButton }

constructor TcxNavigatorButton.Create(AButtons: TcxCustomNavigatorButtons;
  ADefaultVisible: Boolean);
begin
  inherited Create;
  FButtons := AButtons;
  FDefaultIndex := -1;
  FDefaultVisible := ADefaultVisible;
  FEnabled := True;
  FImageIndex := -1;
  FVisible := ADefaultVisible;
end;

function TcxNavigatorButton.GetAdornerTargetElementName: string;
begin
  Result := Buttons.GetAdornerTargetElementName(DefaultIndex);
end;

function  TcxNavigatorButton.GetOwner: TPersistent;
begin
  Result := FButtons;
end;

procedure TcxNavigatorButton.Assign(Source: TPersistent);
begin
  if Source is TcxNavigatorButton then
    with Source as TcxNavigatorButton do
    begin
      Self.Enabled := Enabled;
      Self.Hint := Hint;
      Self.ImageIndex  := ImageIndex;
      Self.Visible := Visible;
      Self.FIsVisibleAssigned := FIsVisibleAssigned;
      Self.OnClick := OnClick;
    end
  else
    inherited Assign(Source);
end;

procedure TcxNavigatorButton.Click;
begin
  if GetInternalEnabled then
    DoClick;
end;

function TcxNavigatorButton.GetInternalEnabled: Boolean;
begin
  Result := Enabled;
  if Result then
    Result := Buttons.GetButtonEnabled(DefaultIndex);
end;

function TcxNavigatorButton.GetInternalHint: string;
begin
  Result := Hint;
  if Hint = '' then
    Result := Buttons.GetButtonHint(DefaultIndex);
end;

function TcxNavigatorButton.HasImage: Boolean;
begin
  Result := not IsCustomButton or (Buttons.Images <> nil) and (ImageIndex > -1);
end;

function TcxNavigatorButton.IsCustomButton: Boolean;
begin
  Result := DefaultIndex >= NavigatorButtonCount;
end;

function TcxNavigatorButton.IsUserImageListUsed: Boolean;
begin
  Result := (Buttons.Images <> nil) and (ImageIndex > -1);
end;

procedure TcxNavigatorButton.DoClick;
var
  ADone: Boolean;
begin
  if Assigned(OnClick) then
    OnClick(Self)
  else
  begin
    ADone := False;
    if Assigned(Buttons.FOnButtonClick) then
      Buttons.FOnButtonClick(Buttons, DefaultIndex, ADone);
    if not ADone then
      Buttons.DoButtonClick(DefaultIndex);
  end;
end;

procedure TcxNavigatorButton.RestoreDefaultVisible(ACanBeVisible: Boolean);
begin
  if not FIsVisibleAssigned then
    InternalSetVisible(FDefaultVisible and ACanBeVisible);
end;

function TcxNavigatorButton.GetImageSize: TSize;
var
  APainter: TcxCustomLookAndFeelPainter;
begin
  APainter := GetNavigator.GetNavigatorLookAndFeel.SkinPainter;
  if (APainter = nil) or IsUserImageListUsed then
    Result := dxGetImageSize(InternalImages, ScaleFactor)
  else
    Result := APainter.NavigatorScaledButtonGlyphSize(ScaleFactor);
end;

function TcxNavigatorButton.GetNavigator: IcxNavigatorOwner;
begin
  Result := Buttons.Navigator;
end;

function TcxNavigatorButton.GetScaleFactor: TdxScaleFactor;
begin
  Result := dxGetScaleFactorForInterface(Navigator);
end;

procedure TcxNavigatorButton.SetEnabled(const Value: Boolean);
begin
  if Enabled <> Value then
  begin
    FEnabled := Value;
    Buttons.Changed(nctLayout, Visible);
  end;
end;

procedure TcxNavigatorButton.SetHint(const Value: string);
begin
  if AnsiCompareStr(FHint, Value) <> 0 then
  begin
    FHint := Value;
    Buttons.Changed(nctProperties, Visible);
  end;
end;

function TcxNavigatorButton.GetInternalImageIndex: Integer;
begin
  if IsUserImageListUsed or IsCustomButton then
    Result := ImageIndex
  else
    Result := DefaultIndex;
end;

function TcxNavigatorButton.GetInternalImages: TCustomImageList;
begin
  if IsUserImageListUsed then
    Result := Buttons.Images
  else
    Result := Buttons.DefaultImages;
end;

procedure TcxNavigatorButton.InternalSetVisible(Value: Boolean;
  AIsInternalSetting: Boolean = True);
begin
  if not AIsInternalSetting then
    FIsVisibleAssigned := True;
  if FVisible <> Value then
  begin
    FVisible := Value;
    Buttons.Changed(nctSize, True);
  end;
end;

function TcxNavigatorButton.IsVisibleStored: Boolean;
begin
  Result := FIsVisibleAssigned;
end;

procedure TcxNavigatorButton.SetImageIndex(Value: TcxImageIndex);
begin
  if ImageIndex <> Value then
  begin
    FImageIndex := Value;
    Buttons.Changed(nctLayout, Visible);
  end;
end;

procedure TcxNavigatorButton.SetOnClick(const Value: TNotifyEvent);
begin
  FOnClick := Value;
  Buttons.Changed(nctProperties);
end;

procedure TcxNavigatorButton.SetVisible(const Value: Boolean);
begin
  InternalSetVisible(Value, False);
end;

{ TcxNavigatorChildComponent }

constructor TcxNavigatorChildComponent.Create(ANavigator: IcxNavigatorOwner);
begin
  inherited Create;
  FNavigator := ANavigator;
  FScaleFactor := dxGetScaleFactorForInterface(ANavigator);
  FScaleFactor.ListenerAdd(ScaleFactorChangeHandler);
end;

destructor TcxNavigatorChildComponent.Destroy;
begin
  FScaleFactor.ListenerRemove(ScaleFactorChangeHandler);
  inherited Destroy;
end;

procedure TcxNavigatorChildComponent.Changed(AChangeType: TcxNavigatorChangeType;
  ANeedRefresh: Boolean = False);
begin
  if ANeedRefresh then
    Navigator.RefreshNavigator;
  Navigator.NavigatorChanged(AChangeType);
end;

function TcxNavigatorChildComponent.GetOwner: TPersistent;
begin
  Result := FNavigator.GetNavigatorOwner;
end;

function TcxNavigatorChildComponent.IsNavigatorEnabled: Boolean;
begin
  Result := Navigator.GetNavigatorControl.Enabled;
end;

procedure TcxNavigatorChildComponent.ScaleFactorChanged(M, D: Integer);
begin
//do nothing
end;

procedure TcxNavigatorChildComponent.ScaleFactorChangeHandler(Sender: TObject; M, D: Integer; IsLoading: Boolean);
begin
  if not IsLoading then
    ScaleFactorChanged(M, D);
end;

{ TcxNavigatorCustomButton }

constructor TcxNavigatorCustomButton.Create(Collection: TCollection);
begin
  inherited;
  FButton := TcxNavigatorButton.Create(Collection.Owner as TcxCustomNavigatorButtons, True);
end;

destructor TcxNavigatorCustomButton.Destroy;
begin
  FreeAndNil(FButton);
  inherited;
end;

procedure TcxNavigatorCustomButton.Assign(Source: TPersistent);
var
  ASource: TcxNavigatorCustomButton;
begin
  if Source is TcxNavigatorCustomButton then
  begin
    ASource := TcxNavigatorCustomButton(Source);
    Button := ASource.Button;
  end
  else
    inherited Assign(Source);
end;

function TcxNavigatorCustomButton.GetEnabled: Boolean;
begin
  Result := FButton.Enabled;
end;

function TcxNavigatorCustomButton.GetHint: string;
begin
  Result := FButton.Hint;
end;

function TcxNavigatorCustomButton.GetImageIndex: TcxImageIndex;
begin
  Result := FButton.ImageIndex;
end;

function TcxNavigatorCustomButton.GetVisible: Boolean;
begin
  Result := FButton.Visible;
end;

procedure TcxNavigatorCustomButton.SetButton(const Value: TcxNavigatorButton);
begin
  FButton.Assign(Value);
end;

procedure TcxNavigatorCustomButton.SetEnabled(const Value: Boolean);
begin
  FButton.Enabled := Value;
end;

procedure TcxNavigatorCustomButton.SetHint(const Value: string);
begin
  FButton.Hint := Value;
end;

procedure TcxNavigatorCustomButton.SetImageIndex(const Value: TcxImageIndex);
begin
  FButton.ImageIndex := Value;
end;

procedure TcxNavigatorCustomButton.SetVisible(const Value: Boolean);
begin
  FButton.Visible := Value;
end;

{ TcxNavigatorCustomButtons }

function TcxNavigatorCustomButtons.Add: TcxNavigatorCustomButton;
begin
  BeginUpdate;
  try
    Result := TcxNavigatorCustomButton(inherited Add);
  finally
    EndUpdate;
  end;
end;

procedure TcxNavigatorCustomButtons.Update(Item: TCollectionItem);
begin
  (Owner as TcxCustomNavigatorButtons).CustomButtonsChanged;
end;

function TcxNavigatorCustomButtons.GetItem(Index: Integer): TcxNavigatorCustomButton;
begin
  Result := TcxNavigatorCustomButton(inherited GetItem(Index));
end;

procedure TcxNavigatorCustomButtons.SetItem(Index: Integer;
  Value: TcxNavigatorCustomButton);
begin
  inherited SetItem(Index, Value);
end;

{ TcxCustomNavigatorButtons }

constructor TcxCustomNavigatorButtons.Create(ANavigator: IcxNavigatorOwner);
begin
  inherited Create(ANavigator);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FButtons := TList.Create;
  CreateButtons;
  FCustomButtons := TcxNavigatorCustomButtons.Create(Self, TcxNavigatorCustomButton);
  FConfirmDelete := True;
end;

destructor TcxCustomNavigatorButtons.Destroy;
begin
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FCustomButtons);
  DestroyButtons;
  FreeAndNil(FButtons);
  inherited Destroy;
end;

procedure TcxCustomNavigatorButtons.Assign(Source: TPersistent);
var
  I: Integer;
  ASource: TcxCustomNavigatorButtons;
begin
  if Source is TcxCustomNavigatorButtons then
  begin
    ASource := TcxCustomNavigatorButtons(Source);
    for I := 0 to NavigatorButtonCount - 1 do
      Buttons[I].Assign(ASource.Buttons[I]);
    CustomButtons := ASource.CustomButtons;
    ConfirmDelete := ASource.ConfirmDelete;
    Images := ASource.Images;
    OnButtonClick := ASource.OnButtonClick;
  end
  else
    inherited Assign(Source);
end;

procedure TcxCustomNavigatorButtons.ClickButton(Index: Integer);
begin
  Buttons[Index].Click;
end;

procedure TcxCustomNavigatorButtons.ReleaseBookmark;
begin
end;

procedure TcxCustomNavigatorButtons.CreateButtons;
var
  I: Integer;
begin
  for I := 0 to NavigatorButtonCount - 1 do
  begin
    FButtons.Add(TcxNavigatorButton.Create(Self, IsButtonVisibleByDefault(I)));
    Buttons[I].DefaultIndex := I;
  end;
end;

procedure TcxCustomNavigatorButtons.DestroyButtons;
var
  I: Integer;
begin
  for I := 0 to NavigatorButtonCount - 1 do
    Buttons[I].Free;
end;

procedure TcxCustomNavigatorButtons.CustomButtonsChanged;
var
  I, AButtonIndex: Integer;
begin
  for I := FButtons.Count - 1 downto NavigatorButtonCount do
    FButtons.Delete(I);
  for I := 0 to CustomButtons.Count - 1 do
  begin
    AButtonIndex := FButtons.Add(CustomButtons[I].Button);
    Buttons[AButtonIndex].DefaultIndex := AButtonIndex;
  end;
  Changed(nctLayout, True);
end;

procedure TcxCustomNavigatorButtons.DoButtonClick(ADefaultIndex: Integer);
begin
end;

function TcxCustomNavigatorButtons.GetAdornerTargetElementName(ADefaultIndex: Integer): string;
begin
  case ADefaultIndex of
    NBDI_FIRST: Result := 'First';
    NBDI_PRIORPAGE: Result := 'PriorPage';
    NBDI_PRIOR: Result := 'Prior';
    NBDI_LAST: Result := 'Last';
    NBDI_NEXT: Result := 'Next';
    NBDI_NEXTPAGE: Result := 'NextPage';
    NBDI_INSERT: Result := 'Insert';
    NBDI_APPEND: Result := 'Append';
    NBDI_DELETE: Result := 'Delete';
    NBDI_EDIT: Result := 'Edit';
    NBDI_POST: Result := 'Post';
    NBDI_CANCEL: Result := 'Cancel';
    NBDI_REFRESH: Result := 'Refresh';
    NBDI_SAVEBOOKMARK: Result := 'SaveBookmark';
    NBDI_GOTOBOOKMARK: Result := 'GoToBookmark';
    NBDI_FILTER: Result := 'Filter';
    else Result := 'Unknown';
  end;
end;

function TcxCustomNavigatorButtons.GetButtonEnabled(ADefaultIndex: Integer): Boolean;
begin
  Result := IsNavigatorEnabled;
end;

function TcxCustomNavigatorButtons.GetButtonHint(ADefaultIndex: Integer): string;
begin
  case ADefaultIndex of
    NBDI_FIRST: Result := cxGetResourceString(@cxNavigatorHint_First);
    NBDI_PRIORPAGE: Result := cxGetResourceString(@cxNavigatorHint_PriorPage);
    NBDI_PRIOR: Result := cxGetResourceString(@cxNavigatorHint_Prior);
    NBDI_LAST: Result := cxGetResourceString(@cxNavigatorHint_Last);
    NBDI_NEXT: Result := cxGetResourceString(@cxNavigatorHint_Next);
    NBDI_NEXTPAGE: Result := cxGetResourceString(@cxNavigatorHint_NextPage);
    NBDI_INSERT: Result := cxGetResourceString(@cxNavigatorHint_Insert);
    NBDI_APPEND: Result := cxGetResourceString(@cxNavigatorHint_Append);
    NBDI_DELETE: Result := cxGetResourceString(@cxNavigatorHint_Delete);
    NBDI_EDIT: Result := cxGetResourceString(@cxNavigatorHint_Edit);
    NBDI_POST: Result := cxGetResourceString(@cxNavigatorHint_Post);
    NBDI_CANCEL: Result := cxGetResourceString(@cxNavigatorHint_Cancel);
    NBDI_REFRESH: Result := cxGetResourceString(@cxNavigatorHint_Refresh);
    NBDI_SAVEBOOKMARK: Result := cxGetResourceString(@cxNavigatorHint_SaveBookmark);
    NBDI_GOTOBOOKMARK: Result := cxGetResourceString(@cxNavigatorHint_GotoBookmark);
    NBDI_FILTER: Result := cxGetResourceString(@cxNavigatorHint_Filter);
    else Result := '';
  end;
end;

function TcxCustomNavigatorButtons.GetButtonImageOffset: Integer;
begin
  Result := 2;
end;

function TcxCustomNavigatorButtons.IsButtonVisibleByDefault(AIndex: Integer): Boolean;
begin
  Result := AIndex <> NBDI_APPEND;
end;

function TcxCustomNavigatorButtons.GetButton(Index: Integer): TcxNavigatorButton;
begin
  Result := FButtons[Index];
end;

function TcxCustomNavigatorButtons.GetButtonCount: Integer;
begin
  Result := FButtons.Count
end;

function TcxCustomNavigatorButtons.GetDefaultImages: TCustomImageList;
begin
  Result := NavigatorImages;
end;

procedure TcxCustomNavigatorButtons.SetButton(Index: Integer; const Value: TcxNavigatorButton);
begin
  Buttons[Index].Assign(Value);
end;

procedure TcxCustomNavigatorButtons.SetConfirmDelete(const Value: Boolean);
begin
  if FConfirmDelete <> Value then
  begin
    FConfirmDelete := Value;
    Changed(nctProperties);
  end;
end;

procedure TcxCustomNavigatorButtons.SetCustomButtons(
  const Value: TcxNavigatorCustomButtons);
begin
  FCustomButtons.Assign(Value);
end;

procedure TcxCustomNavigatorButtons.SetImages(const Value: TCustomImageList);
begin
  cxSetImageList(Value, FImages, FImageChangeLink, Navigator.GetNavigatorOwner);
  Changed(nctLayout);
end;

procedure TcxCustomNavigatorButtons.SetOnButtonClick(
  const Value: TcxNavigatorButtonClickEvent);
begin
  FOnButtonClick := Value;
  Changed(nctProperties);
end;

procedure TcxCustomNavigatorButtons.ImageListChange(Sender: TObject);
begin
  Changed(nctProperties, True);
end;

{ TcxCustomNavigatorInfoPanel }

constructor TcxCustomNavigatorInfoPanel.Create(ANavigator: IcxNavigatorOwner);
begin
  inherited Create(ANavigator);
  FVisible := False;
  FWidth := 0;
  DisplayMask := cxGetResourceString(@cxNavigatorInfoPanelDefaultDisplayMask);
end;

procedure TcxCustomNavigatorInfoPanel.Assign(Source: TPersistent);
begin
  if Source is TcxCustomNavigatorInfoPanel then
    with TcxCustomNavigatorInfoPanel(Source) do
    begin
      Self.DisplayMask := DisplayMask;
      Self.Visible := Visible;
      Self.Width := Width;
      Self.OnClick := OnClick;
    end
  else
    inherited Assign(Source);
end;

procedure TcxCustomNavigatorInfoPanel.BuildFormatString;
begin
  FFormatString := cxGetNavigatorFormatString(FDisplayMask, FIsNeedRecordIndex,
    FIsNeedRecordCount);
end;

procedure TcxCustomNavigatorInfoPanel.DoClick(Button: TMouseButton);
begin
  if Assigned(FOnClick) then
    FOnClick(Self, Button);
end;

function TcxCustomNavigatorInfoPanel.GetAdornerTargetElementVisible: Boolean;
begin
  Result := Visible;
end;

procedure TcxCustomNavigatorInfoPanel.ScaleFactorChanged(M, D: Integer);
begin
  FWidth := MulDiv(Width, M, D);
end;

function TcxCustomNavigatorInfoPanel.GetFormattedText: string;
var
  ARecordIndex, ARecordCount: string;
begin
  if IRecordPosition = nil then
  begin
    Result := '';
    Exit;
  end;
  if IsNeedRecordIndex then
    ARecordIndex := IntToStr(IRecordPosition.GetRecordIndex + 1)
  else
    ARecordIndex := '';
  if IsNeedRecordCount then
    ARecordCount := IntToStr(IRecordPosition.GetRecordCount)
  else
    ARecordCount := '';
  Result := Format(FFormatString, [ARecordIndex, ARecordCount]);
end;

function TcxCustomNavigatorInfoPanel.GetIRecordPosition: IcxNavigatorRecordPosition;
begin
  if Assigned(FOnGetIRecordPosition) then
    Result := FOnGetIRecordPosition
  else
    Result := nil;
end;

function TcxCustomNavigatorInfoPanel.IsDisplayMaskStored: Boolean;
begin
  Result := cxNavigatorInfoPanelDefaultDisplayMask <> DisplayMask;
end;

procedure TcxCustomNavigatorInfoPanel.SetDisplayMask(const Value: string);
begin
  if FDisplayMask <> Value then
  begin
    FDisplayMask := Value;
    BuildFormatString;
    Changed(nctSize, True);
  end;
end;

procedure TcxCustomNavigatorInfoPanel.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(nctSize, True);
  end;
end;

procedure TcxCustomNavigatorInfoPanel.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed(nctSize, True);
  end;
end;

{ TcxNavigatorButtonViewInfo }

constructor TcxNavigatorButtonViewInfo.Create(AOwner: TcxNavigatorViewInfo);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TcxNavigatorButtonViewInfo.Destroy;
begin
  dxFader.Remove(Self);
  inherited Destroy;
end;

procedure TcxNavigatorButtonViewInfo.Calculate;
begin
  Enabled := Button.GetInternalEnabled;
  Hint := Button.GetInternalHint;
  UpdateState;
end;

function TcxNavigatorButtonViewInfo.CalculateState: TcxButtonState;
begin
  if not Enabled then
    Result := cxbsDisabled
  else
    if (Self = Owner.PressedButtonViewInfo) and (Self = Owner.HotTrackButtonViewInfo) then
      Result := cxbsPressed
    else
      if Self = Owner.HotTrackButtonViewInfo then
        Result := cxbsHot
      else
        Result := cxbsNormal;
end;

function TcxNavigatorButtonViewInfo.CanFade: Boolean;
begin
  Result := Painter.LookAndFeelStyle in [lfsNative, lfsSkin];
end;

procedure TcxNavigatorButtonViewInfo.CorrectButtonBounds(var R: TRect);
begin
  if (State <> cxbsNormal) and Owner.CanButtonOverlapBorder then
  begin
    if Self = Owner.FButtons.First then
      Dec(R.Left);
    if Self = Owner.FButtons.Last then
      Inc(R.Right);
    Dec(R.Top);
    Inc(R.Bottom);
  end;
end;

procedure TcxNavigatorButtonViewInfo.Draw(ACanvas: TcxCanvas);
var
  ABitmap: TcxBitmap;
  ABounds, AContentRect, AImageRect: TRect;
  AImageSize: TSize;
  APainter: TcxCustomLookAndFeelPainter;
  AState: TcxButtonState;
begin
  AState := State;
  ABounds := Bounds;
  APainter := Painter;

  AContentRect := cxRectContent(ABounds, APainter.NavigatorScaledButtonGlyphPadding(Button.ScaleFactor));
  CorrectButtonBounds(ABounds);

  ABitmap := TcxBitmap.CreateSize(ABounds, pf32bit);
  try
    cxDrawTransparentControlBackground(Owner.NavigatorOwner.GetNavigatorControl,
      ABitmap.cxCanvas, ABounds, cxNullPoint);
    if not dxFader.DrawFadeImage(Self, ABitmap.Canvas.Handle, ABitmap.ClientRect) then
      DrawContent(ABitmap.cxCanvas, ABitmap.ClientRect, AState);

    if Button.HasImage then
    begin
      AImageSize := Button.GetImageSize;
      AImageRect := cxRectCenter(AContentRect, AImageSize);
      if AState = cxbsPressed then
        AImageRect := cxRectOffset(AImageRect, APainter.NavigatorButtonPressedGlyphOffset);
      OffsetRect(AImageRect, -ABounds.Left, -ABounds.Top);
      DrawGlyph(ABitmap.cxCanvas, AImageRect, AState);
    end;

    cxBitBlt(ACanvas.Handle, ABitmap.Canvas.Handle, ABounds, cxNullPoint, SRCCOPY);
  finally
    ABitmap.Free;
  end;
  if NeedDrawFocusRect then
    ACanvas.DrawFocusRect(cxRectInflate(ABounds, -2, -2));
end;

procedure TcxNavigatorButtonViewInfo.DrawContent(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  Painter.DrawNavigatorScaledButton(ACanvas, R, AState, clWindow, Button.ScaleFactor);
end;

procedure TcxNavigatorButtonViewInfo.DrawGlyph(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  Painter.DrawNavigatorScaledButtonGlyph(ACanvas, Button.InternalImages,
    Button.InternalImageIndex, R, AState <> cxbsDisabled,
    Button.IsUserImageListUsed, Button.ScaleFactor);
end;

procedure TcxNavigatorButtonViewInfo.GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);

  function PrepareFadingImage(AState: TcxButtonState): TcxBitmap32;
  var
    R: TRect;
  begin
    R := Bounds;
    CorrectButtonBounds(R);
    Result := TcxBitmap32.CreateSize(R, True);
    Result.Canvas.Lock;
    DrawContent(Result.cxCanvas, Result.ClientRect, AState);
    Result.Canvas.Unlock;
  end;

begin
  AFadeInImage := PrepareFadingImage(cxbsHot);
  AFadeOutImage := PrepareFadingImage(cxbsNormal);
end;

function TcxNavigatorButtonViewInfo.HasHintPoint(const P: TPoint): Boolean;
begin
  Result := PtInRect(Bounds, P);
end;

procedure TcxNavigatorButtonViewInfo.Invalidate;
var
  ANavigatorControl: TWinControl;
begin
  ANavigatorControl := Owner.NavigatorOwner.GetNavigatorControl;
  if ANavigatorControl.HandleAllocated then
    cxInvalidateRect(ANavigatorControl.Handle, Bounds, False);
end;

function TcxNavigatorButtonViewInfo.IsHintAtMousePos: Boolean;
begin
  Result := False;
end;

procedure TcxNavigatorButtonViewInfo.UpdateState;
var
  ANewState: TcxButtonState;
begin
  ANewState := CalculateState;
  if ANewState <> State then
  begin
    if (ANewState = cxbsHot) and (State = cxbsNormal) then
      dxFader.FadeIn(Self)
    else
      if (ANewState = cxbsNormal) and (State = cxbsHot) then
        dxFader.FadeOut(Self)
      else
        dxFader.Remove(Self);

    FState := ANewState;
    Invalidate;
  end;
end;

function TcxNavigatorButtonViewInfo.UseHintHidePause: Boolean;
begin
  Result := True;
end;

function TcxNavigatorButtonViewInfo.GetAdornerTargetElementControl: TWinControl;
begin
  Result := Owner.NavigatorOwner.GetNavigatorControl;
end;

function TcxNavigatorButtonViewInfo.GetAdornerTargetElementBounds: TRect;
begin
  Result := Bounds;
end;

function TcxNavigatorButtonViewInfo.GetAdornerTargetElementName: string;
begin
  Result := Button.GetAdornerTargetElementName;
end;

function TcxNavigatorButtonViewInfo.GetAdornerTargetElementVisible: Boolean;
begin
  Result := Button.Visible;
end;

function TcxNavigatorButtonViewInfo.GetNeedDrawFocusRect: Boolean;
begin
  Result := (Self = Owner.FocusedButton) and
    Owner.NavigatorOwner.GetNavigatorTabStop and
    Owner.NavigatorOwner.GetNavigatorFocused;
end;

function TcxNavigatorButtonViewInfo.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Owner.NavigatorOwner.GetNavigatorLookAndFeel.GetAvailablePainter(totButton);
end;

{ TcxNavigatorButtonsViewInfo }

procedure TcxNavigatorButtonsViewInfo.AddAdornerTargetElements(AList: TStrings);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AList.AddObject(Items[I].GetAdornerTargetElementName, Items[I]);
end;

procedure TcxNavigatorButtonsViewInfo.CalculateSize;
var
  I: Integer;
begin
  FSize := cxNullSize;
  for I := 0 to Count - 1 do
  begin
    Inc(FSize.cx, Items[I].Size.cx);
    FSize.cy := Max(FSize.cy, Items[I].Size.cy);
  end;
end;

function TcxNavigatorButtonsViewInfo.GetItem(Index: Integer): TcxNavigatorButtonViewInfo;
begin
  Result := TcxNavigatorButtonViewInfo(inherited GetItem(Index));
end;


{ TcxCustomNavigatorInfoPanelViewInfo }

constructor TcxCustomNavigatorInfoPanelViewInfo.Create(ANavigatorViewInfo: TcxNavigatorViewInfo);
begin
  inherited Create;
  FNavigatorViewInfo := ANavigatorViewInfo;
end;

function TcxCustomNavigatorInfoPanelViewInfo.GetIsPaintOnGlass: Boolean;
begin
  Result := dxIsPaintOnGlass(FNavigatorViewInfo.NavigatorOwner.GetNavigatorControl);
end;

function TcxCustomNavigatorInfoPanelViewInfo.GetNavigatorOwner: IcxNavigatorOwner2;
begin
  Result := FNavigatorViewInfo.NavigatorOwner2;
end;

procedure TcxCustomNavigatorInfoPanelViewInfo.Calculate(const ABounds: TRect; const AText: string);
begin
  FBounds := ABounds;
  FText := AText;
end;

procedure TcxCustomNavigatorInfoPanelViewInfo.Paint;

  procedure DoDraw(ACanvas: TcxCanvas; const R: TRect);
  var
    AViewParams: TcxViewParams;
  begin
    AViewParams := NavigatorOwner.GetNavigatorInfoPanel.GetViewParams;
    ACanvas.FillRect(R, AViewParams);
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font := AViewParams.Font;
    ACanvas.Font.Color := AViewParams.TextColor;
    ACanvas.DrawTexT(FText, R, cxSingleLine or cxAlignCenter);
    ACanvas.Brush.Style := bsSolid;
  end;

var
  ABitmap: TcxBitmap32;
begin
  if IsPaintOnGlass then
  begin
    ABitmap := TcxBitmap32.CreateSize(Bounds);
    try
      DoDraw(ABitmap.cxCanvas, ABitmap.ClientRect);
      ABitmap.MakeOpaque;
      cxBitBlt(NavigatorViewInfo.Canvas.Handle, ABitmap.Canvas.Handle, Bounds, cxNullPoint, SRCCOPY);
    finally
      ABitmap.Free;
    end;
  end
  else
    DoDraw(NavigatorViewInfo.Canvas, Bounds);
end;

function TcxCustomNavigatorInfoPanelViewInfo.GetAdornerTargetElementControl: TWinControl;
begin
  Result := NavigatorOwner.GetNavigatorControl;
end;

function TcxCustomNavigatorInfoPanelViewInfo.GetAdornerTargetElementBounds: TRect;
begin
  Result := Bounds;
end;

function TcxCustomNavigatorInfoPanelViewInfo.GetAdornerTargetElementVisible: Boolean;
begin
  Result := NavigatorOwner.GetNavigatorInfoPanel.GetAdornerTargetElementVisible;
end;

procedure TcxCustomNavigatorInfoPanelViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  FBounds := TdxRightToLeftLayoutConverter.ConvertRect(FBounds, AClientBounds)
end;

{ TcxNavigatorHintHelper }

constructor TcxNavigatorHintHelper.Create(ANavigatorViewInfo: TcxNavigatorViewInfo);
begin
  inherited Create;
  FNavigatorViewInfo := ANavigatorViewInfo;
end;

procedure TcxNavigatorHintHelper.MouseDown;
begin
  CancelHint;
end;

procedure TcxNavigatorHintHelper.CorrectHintWindowRect(var ARect: TRect);
begin
  inherited CorrectHintWindowRect(ARect);
  ARect := cxRectSetOrigin(ARect, GetMouseCursorPos);
  OffsetRect(ARect, 0, cxGetCursorSize.cy);
end;

function TcxNavigatorHintHelper.GetOwnerControl: TcxControl;
begin
  Result := FNavigatorViewInfo.NavigatorOwner.GetNavigatorControl as TcxControl;
end;

{ TcxNavigatorViewInfo }

constructor TcxNavigatorViewInfo.Create(
  ANavigator: IcxNavigatorOwner;
  AIsInplace: Boolean = True);
begin
  inherited Create;
  FNavigatorOwner := ANavigator;
  MakeDirty;
  FIsInplace := AIsInplace;
  FButtons := TcxNavigatorButtonsViewInfo.Create;
  FButtonPressTimer := cxCreateTimer(DoButtonPressTimer, 1000, False);
  FHintHelper := TcxNavigatorHintHelper.Create(Self);
  FCanvas := TcxCanvas.Create(nil);
  Supports(ANavigator, IcxNavigatorOwner2, FNavigatorOwner2);
  FInfoPanelViewInfo := TcxCustomNavigatorInfoPanelViewInfo.Create(Self);
end;

destructor TcxNavigatorViewInfo.Destroy;
begin
  cxClearObjectLinks(Self);
  FreeAndNil(FInfoPanelViewInfo);
  FreeAndNil(FCanvas);
  FreeAndNil(FHintHelper);
  FreeAndNil(FButtonPressTimer);
  FreeAndNil(FButtons);
  inherited Destroy;
end;

procedure TcxNavigatorViewInfo.Calculate;
var
  AHeight, AWidth: Integer;
  ANavigatorBounds: TRect;
begin
  ANavigatorBounds := NavigatorOwner.GetNavigatorBounds;
  AWidth := cxRectWidth(ANavigatorBounds);
  AHeight := cxRectHeight(ANavigatorBounds);
  CheckSize(AWidth, AHeight);
  CalculateButtons;
  FIsDirty := False;
end;


procedure TcxNavigatorViewInfo.CheckSize(var AWidth, AHeight: Integer);
var
  AButtonIndent, AInfoPanelWidth: Integer;
  AInfoPanelText: string;
begin
  MakeDirty;
  ValidateButtonsViewInfos;
  AInfoPanelText := GetInfoPanelText;
  CalculateSize(AWidth, AHeight, AInfoPanelText, AButtonIndent, AInfoPanelWidth);
  CalculateBounds(AInfoPanelText, AInfoPanelWidth, AHeight, AButtonIndent);
end;

procedure TcxNavigatorViewInfo.Clear;
begin
  MakeDirty;
  FInfoPanelPressed := False;
  ClearButtons;
end;

procedure TcxNavigatorViewInfo.DoEnter;
begin
  InvalidateButton(FocusedButton);
  UpdateSelected;
end;

procedure TcxNavigatorViewInfo.DoExit;
begin
  InvalidateButton(FocusedButton);
  UpdateSelected;
end;

function TcxNavigatorViewInfo.GetButtonAt(const pt: TPoint): TcxNavigatorButton;
var
  AViewInfo: TcxNavigatorButtonViewInfo;
begin
  AViewInfo := GetButtonViewInfoAt(pt);
  if AViewInfo <> nil then
    Result := AViewInfo.Button
  else
    Result := nil;
end;

function TcxNavigatorViewInfo.GetButtonViewInfoAt(const pt: TPoint): TcxNavigatorButtonViewInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ButtonCount - 1 do
    if PtInRect(Buttons[I].Bounds, pt) then
    begin
      Result := Buttons[I];
      Break;
    end;
end;

function TcxNavigatorViewInfo.GetButtonViewInfoByButton(
  AButton: TcxNavigatorButton): TcxNavigatorButtonViewInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ButtonCount - 1 do
    if Buttons[I].Button = AButton then
    begin
      Result := Buttons[I];
      break;
    end;
end;

procedure TcxNavigatorViewInfo.MakeDirty;
begin
  FIsDirty := True;
end;

procedure TcxNavigatorViewInfo.MouseDown(X, Y: Integer);
var
  AButtonViewInfo: TcxNavigatorButtonViewInfo;
begin
  FHintHelper.MouseDown;
  FInfoPanelPressed := HasInfoPanel and cxRectPtIn(InfoPanelViewInfo.Bounds, X, Y);
  if not FInfoPanelPressed then
  begin
    AButtonViewInfo := GetButtonViewInfoAt(Point(X, Y));
    if (AButtonViewInfo <> nil) and AButtonViewInfo.Enabled then
    begin
      FButtonPressTimer.Interval := cxButtonsRepeatInitPause;
      FButtonPressTimer.Enabled := True;
      PressedButtonViewInfo := AButtonViewInfo;
      FocusedButton := AButtonViewInfo;
      PressedButtonViewInfo.UpdateState;
    end;
  end;
  UpdateSelected;
end;

procedure TcxNavigatorViewInfo.MouseLeave;
begin
  MouseMove(-1, -1);
  FHintHelper.MouseLeave;
end;

procedure TcxNavigatorViewInfo.MouseMove(X, Y: Integer);
var
  AButtonViewInfo: TcxNavigatorButtonViewInfo;
  APrevButtonViewInfo: TcxNavigatorButtonViewInfo;
begin
  AButtonViewInfo := GetButtonViewInfoAt(Point(X, Y));
  if AButtonViewInfo <> HotTrackButtonViewInfo then
  begin
    APrevButtonViewInfo := HotTrackButtonViewInfo;
    FHotTrackButtonViewInfo := AButtonViewInfo;
    if HotTrackButtonViewInfo <> nil then
      HotTrackButtonViewInfo.UpdateState;
    if APrevButtonViewInfo <> nil then
      APrevButtonViewInfo.UpdateState;
  end;
  CheckHint;
  UpdateSelected;
end;

procedure TcxNavigatorViewInfo.MouseUp(Button: TMouseButton; X, Y: Integer);
var
  AButtonViewInfo: TcxNavigatorButtonViewInfo;
begin
  FButtonPressTimer.Enabled := False;
  if FInfoPanelPressed then
  begin
    FInfoPanelPressed := False;
    if cxRectPtIn(InfoPanelViewInfo.Bounds, X, Y) then
      InfoPanel.DoClick(Button);
  end;
  AButtonViewInfo := PressedButtonViewInfo;
  FPressedButtonViewInfo := nil;
  if AButtonViewInfo <> nil then
    AButtonViewInfo.UpdateState;
  UpdateSelected;
  if (AButtonViewInfo <> nil) and PtInRect(AButtonViewInfo.Bounds, Point(X, Y)) and AButtonViewInfo.Enabled then
    AButtonViewInfo.Button.DoClick;
end;

procedure TcxNavigatorViewInfo.Paint;
begin
  CheckCalculate;
  PaintButtons;
  if HasInfoPanel then
    InfoPanelViewInfo.Paint;
  if FIsInplace then
    Canvas.ExcludeClipRect(NavigatorOwner.GetNavigatorBounds);
end;

procedure TcxNavigatorViewInfo.PaintBorder(const ABounds: TRect);
var
  ANavigatorControl: TWinControl;
begin
  CheckCalculate;
  ANavigatorControl := NavigatorOwner.GetNavigatorControl;
  NavigatorOwner.GetNavigatorLookAndFeel.GetAvailablePainter(totButton).DrawNavigatorBorder(Canvas,
    ABounds, FIsSelected or ((csDesigning in ANavigatorControl.ComponentState) and ANavigatorControl.Enabled));
end;

procedure TcxNavigatorViewInfo.PressArrowKey(ALeftKey: Boolean);
var
  AIndex: Integer;
begin
  AIndex := FButtons.IndexOf(FocusedButton);
  if AIndex < 0 then exit;
  if ALeftKey then
  begin
    if AIndex > 0 then
      FocusedButton := Buttons[AIndex - 1];
  end
  else
  begin
    if AIndex < ButtonCount - 1 then
      FocusedButton := Buttons[AIndex + 1];
  end;
end;

procedure TcxNavigatorViewInfo.Update;
var
  ALink: TcxObjectLink;
begin
  ALink := cxAddObjectLink(Self);
  try
    UpdateInfoPanel;
    if ALink.Ref <> nil then
      UpdateButtonsEnabled;
  finally
    cxRemoveObjectLink(ALink);
  end;
end;

procedure TcxNavigatorViewInfo.UpdateButtonsEnabled;
var
  I: Integer;
begin
  for I := 0 to ButtonCount - 1 do
  begin
    Buttons[I].Enabled := Buttons[I].Button.GetInternalEnabled;
    Buttons[I].UpdateState;
  end;
end;

procedure TcxNavigatorViewInfo.UpdateInfoPanel;
var
  ANewText: string;
  ANavigatorControl: TWinControl;
  ASaveSize: TSize;
begin
  ANavigatorControl := NavigatorOwner.GetNavigatorControl;
  if ANavigatorControl.HandleAllocated and (GetInfoPanelFont <> nil) then
  begin
    ANewText := GetInfoPanelText;
    if ANewText <> InfoPanelViewInfo.Text then
    begin
      ASaveSize := GetInfoPanelSize(InfoPanelViewInfo.Text);
      if not cxSizeIsEqual(ASaveSize, GetInfoPanelSize(ANewText)) then
      begin
        NavigatorOwner.RefreshNavigator;
        Exit;
      end;
      InfoPanelViewInfo.Calculate(InfoPanelViewInfo.Bounds, ANewText);
    end;
    Windows.InvalidateRect(ANavigatorControl.Handle, @(InfoPanelViewInfo.FBounds), False);
  end;
end;

procedure TcxNavigatorViewInfo.CalculateButtons;
var
  AButtonViewInfo: TcxNavigatorButtonViewInfo;
  APressedButton: TcxNavigatorButton;
  I: Integer;
  AHotTrackButton: TcxNavigatorButton;
begin
  AHotTrackButton := nil;
  APressedButton := nil;
  if FHotTrackButtonViewInfo <> nil then
    AHotTrackButton := FHotTrackButtonViewInfo.Button;
  if FPressedButtonViewInfo <> nil then
    APressedButton := FPressedButtonViewInfo.Button;
  FHotTrackButtonViewInfo := nil;
  FPressedButtonViewInfo := nil;
  for I := 0 to ButtonCount - 1 do
  begin
    AButtonViewInfo := Buttons[I];
    if AButtonViewInfo.Button = APressedButton then
      FPressedButtonViewInfo := AButtonViewInfo;
    if AButtonViewInfo.Button = AHotTrackButton then
      FHotTrackButtonViewInfo := AButtonViewInfo;
  end;
  for I := 0 to ButtonCount - 1 do
    Buttons[I].Calculate;
end;

procedure TcxNavigatorViewInfo.AddAdornerTargetElements(AList: TStrings);
begin
  AList.AddObject('InfoPanel', InfoPanelViewInfo);
  Buttons.AddAdornerTargetElements(AList);
end;

function TcxNavigatorViewInfo.FindInfoPanelPositionIndex(const AInfoPanelText: string): Integer;
var
  I: Integer;
begin
  if AInfoPanelText = '' then
    Exit(-1);
  Result := 0;
  for I := 0 to ButtonCount - 1 do
    if Buttons[I].Button.DefaultIndex < NBDI_NEXT then
      Inc(Result);
end;

procedure TcxNavigatorViewInfo.CalculateBounds(
  const AInfoPanelText: string; AInfoPanelWidth, AHeight, AButtonIndent: Integer);
var
  AButtonBounds, ANavigatorBounds, AInfoPanelBounds: TRect;
  I, AInfoPanelIndex: Integer;
begin
  FIsRightToLeftConverted := False;
  FButtonIndent := AButtonIndent;
  ANavigatorBounds := NavigatorOwner.GetNavigatorBounds;
  AButtonBounds.InitSize(ANavigatorBounds.Left, ANavigatorBounds.Top, 0, AHeight);
  AInfoPanelBounds := ANavigatorBounds;
  if ButtonCount = 0 then
    AInfoPanelWidth := ANavigatorBounds.Width;
  AInfoPanelIndex := FindInfoPanelPositionIndex(AInfoPanelText);
  for I := 0 to ButtonCount - 1 do
  begin
    if I = AInfoPanelIndex then
    begin
      AInfoPanelBounds.Left := AButtonBounds.Right;
      AInfoPanelBounds.Width := AInfoPanelWidth;
      AButtonBounds.Right := AInfoPanelBounds.Right;
    end;
    AButtonBounds.Left := AButtonBounds.Right;
    AButtonBounds.Width := Buttons[I].Size.cx + AButtonIndent;
    Buttons[I].Bounds := AButtonBounds;
  end;
  if AInfoPanelIndex >= 0 then
  begin
    if AInfoPanelIndex = ButtonCount then
    begin
      AInfoPanelBounds.Left := AButtonBounds.Right;
      AInfoPanelBounds.Width := AInfoPanelWidth;
    end;
    FInfoPanelViewInfo.Calculate(AInfoPanelBounds, AInfoPanelText);
  end;
end;

function TcxNavigatorViewInfo.CalculateMinSize: TSize;
var
  AButtonIndent, AInfoPanelWidth: Integer;
begin
  MakeDirty;
  ValidateButtonsViewInfos;
  Result := cxNullSize;
  CalculateSize(Result.cx, Result.cy, GetInfoPanelText, AButtonIndent, AInfoPanelWidth);
end;

procedure TcxNavigatorViewInfo.CalculateSize(var AWidth, AHeight: Integer;
  const AInfoPanelText: string; out AButtonIndent, AInfoPanelWidth: Integer);
var
  AInfoPanelSize: TSize;
  AMinWidth: Integer;
begin
  AInfoPanelSize := GetInfoPanelSize(AInfoPanelText);
  AInfoPanelWidth := AInfoPanelSize.cx;
  AMinWidth := AInfoPanelWidth + Buttons.Size.cx;

  if ButtonCount > 0 then
    AButtonIndent := (Max(AWidth, AMinWidth) - AMinWidth) div ButtonCount
  else
    AButtonIndent := 0;

  AWidth := AMinWidth + AButtonIndent * ButtonCount;
  AHeight := Max(AHeight, Max(AInfoPanelSize.cy, Buttons.Size.cy));
end;

procedure TcxNavigatorViewInfo.InvalidateButton(AButton: TcxNavigatorButtonViewInfo);
begin
  if AButton <> nil then
    AButton.Invalidate;
end;

procedure TcxNavigatorViewInfo.PaintButton(AButtonIndex: Integer);
var
  AButtonViewInfo: TcxNavigatorButtonViewInfo;
  ABounds: TRect;
begin
  AButtonViewInfo := TcxNavigatorButtonViewInfo(FButtons[AButtonIndex]);
  if Canvas.RectVisible(AButtonViewInfo.Bounds) then
  begin
    ABounds := AButtonViewInfo.Bounds;
    if InRange(AButtonIndex, NBDI_FIRST, NBDI_LAST) then
      cxRightToLeftDependentDraw(Canvas.Handle, ABounds, IsRightToLeftConverted,
        procedure
        begin
          AButtonViewInfo.Draw(Canvas);
        end)
    else
      AButtonViewInfo.Draw(Canvas);
    Canvas.ExcludeClipRect(AButtonViewInfo.Bounds);
  end;
end;

procedure TcxNavigatorViewInfo.PaintButtons;
var
  I: Integer;
begin
  for I := 0 to ButtonCount - 1 do
    PaintButton(I);
end;

procedure TcxNavigatorViewInfo.RightToLeftConversion;
begin
  if not FIsRightToLeftConverted then
    DoRightToLeftConversion;
  FIsRightToLeftConverted := True;
end;

procedure TcxNavigatorViewInfo.ValidateButtonsViewInfos;
var
  ANavigatorButtons: TcxCustomNavigatorButtons;
  AButtonViewInfo: TcxNavigatorButtonViewInfo;
  I: Integer;
begin
  ClearButtons;
  ANavigatorButtons := NavigatorOwner.GetNavigatorButtons;
  for I := 0 to ANavigatorButtons.ButtonCount - 1 do
    if ANavigatorButtons[I].Visible then
    begin
      AButtonViewInfo := TcxNavigatorButtonViewInfo.Create(Self);
      AButtonViewInfo.Button := ANavigatorButtons[I];
      FButtons.Add(AButtonViewInfo);
      AButtonViewInfo.FSize := GetMinButtonSize(ButtonCount - 1);
    end;
  FButtons.CalculateSize;
end;

function TcxNavigatorViewInfo.HasInfoPanel: Boolean;
begin
  Result := GetInfoPanelText <> '';
end;

function TcxNavigatorViewInfo.GetButtonCount: Integer;
begin
  Result := FButtons.Count;
end;

function TcxNavigatorViewInfo.GetCanvas: TcxCanvas;
begin
  FCanvas.Canvas := NavigatorOwner.GetNavigatorCanvas;
  Result := FCanvas;
end;

function TcxNavigatorViewInfo.GetFocusedButton: TcxNavigatorButtonViewInfo;
begin
  if (FFocusedButton <> nil) and not FFocusedButton.Visible then
    FFocusedButton := nil;
  if (FFocusedButton = nil) and (ButtonCount > 0) then
    FFocusedButton := Buttons[0].Button;
  if (FFocusedButton <> nil) then
    Result := GetButtonViewInfoByButton(FFocusedButton)
  else Result := nil;
end;

function TcxNavigatorViewInfo.GetInfoPanel: TcxCustomNavigatorInfoPanel;
begin
  Result := nil;
  if NavigatorOwner2 <> nil then
    Result := NavigatorOwner2.GetNavigatorInfoPanel;
end;

procedure TcxNavigatorViewInfo.SetFocusedButton(Value: TcxNavigatorButtonViewInfo);
var
  AOldButtonViewInfo: TcxNavigatorButtonViewInfo;
begin
  AOldButtonViewInfo := GetFocusedButton;
  if AOldButtonViewInfo <> Value then
  begin
    FFocusedButton := Value.Button;
    InvalidateButton(AOldButtonViewInfo);
    InvalidateButton(Value);
  end;
end;

procedure TcxNavigatorViewInfo.CancelHint;
begin
  FHintHelper.CancelHint;
end;

function TcxNavigatorViewInfo.CanButtonOverlapBorder: Boolean;
begin
  Result := False;
end;

procedure TcxNavigatorViewInfo.CheckCalculate;
begin
  if FIsDirty then
    Calculate;
end;

procedure TcxNavigatorViewInfo.ClearButtons;
begin
  FButtons.Clear;
  FHotTrackButtonViewInfo := nil;
  FPressedButtonViewInfo := nil;
end;

procedure TcxNavigatorViewInfo.CheckHint;

  function NeedShowHint(const AHint: string): Boolean;
  begin
    Result := NavigatorOwner.GetNavigatorShowHint and (AHint <> '') and
      cxCanShowHint(NavigatorOwner.GetNavigatorControl);
  end;

var
  AHint: string;
  AHintAreaBounds: TRect;
begin
  if (HotTrackButtonViewInfo <> nil) then
  begin
    AHint := GetShortHint(HotTrackButtonViewInfo.Hint);
    AHintAreaBounds := HotTrackButtonViewInfo.Bounds;
  end
  else
  begin
    AHint := '';
    AHintAreaBounds := cxNullRect;
  end;
  if NeedShowHint(AHint) then
    FHintHelper.ShowHint(AHintAreaBounds, AHintAreaBounds, AHint, False, HotTrackButtonViewInfo);
end;

procedure TcxNavigatorViewInfo.DoRightToLeftConversion;
var
  I: Integer;
  ABounds: TRect;
  AButtonViewInfo: TcxNavigatorButtonViewInfo;
begin
  ABounds := cxNullRect;
  for I := 0 to ButtonCount - 1 do
  begin
    AButtonViewInfo := Buttons[I];
    if AButtonViewInfo.Button.Visible and InRange(AButtonViewInfo.Button.DefaultIndex, NBDI_FIRST, NBDI_LAST) then
      if cxRectIsNull(ABounds) then
        ABounds := AButtonViewInfo.Bounds
      else
        ABounds.Right := AButtonViewInfo.Bounds.Right;

    if AButtonViewInfo.Button.DefaultIndex >= NBDI_LAST then
      Break;
  end;

  for I := 0 to ButtonCount - 1 do
  begin
    AButtonViewInfo := Buttons[I];
    if AButtonViewInfo.Button.Visible and InRange(AButtonViewInfo.Button.DefaultIndex, NBDI_FIRST, NBDI_LAST) then
      AButtonViewInfo.Bounds := TdxRightToLeftLayoutConverter.ConvertRect(AButtonViewInfo.Bounds, ABounds);

    if AButtonViewInfo.Button.DefaultIndex >= NBDI_LAST then
      Break;
  end;
  if (InfoPanel <> nil) and InfoPanel.Visible then
    FInfoPanelViewInfo.DoRightToLeftConversion(ABounds);
end;

function TcxNavigatorViewInfo.GetInfoPanelFont: TFont;
begin
  Result := InfoPanel.GetViewParams.Font;
end;

function TcxNavigatorViewInfo.GetInfoPanelText: string;
var
  AInfoPanel: TcxCustomNavigatorInfoPanel;
begin
  Result := '';
  AInfoPanel := InfoPanel;
  if AInfoPanel <> nil then
  begin
    if AInfoPanel.Visible and (AInfoPanel.IRecordPosition <> nil) then
      Result := AInfoPanel.GetFormattedText
  end;
end;

function TcxNavigatorViewInfo.GetInfoPanelSize(AText: string): TSize;
var
  I: Integer;
  AInfoPanel: TcxCustomNavigatorInfoPanel;
  AFont: TFont;
begin
  AInfoPanel := InfoPanel;
  if (AText <> '') and (AInfoPanel <> nil) then
  begin
    AFont := GetInfoPanelFont;
    if AInfoPanel.Width > 0 then
      Result := cxSize(AInfoPanel.Width, cxTextHeight(AFont))
    else
    begin
      for I := 1 to Length(AText) do
        if (AText[I] >= '0') and (AText[I] <= '9') then
          AText[I] := '0';
      Result := cxTextExtent(AFont, AText + '00');
    end;
  end
  else
    Result := cxNullSize;
end;

function TcxNavigatorViewInfo.GetMinButtonSize(AButtonIndex: Integer): TSize;
var
  AMinSize: TSize;
  APadding: TRect;
  APainter: TcxCustomLookAndFeelPainter;
begin
  Result := TcxNavigatorButtonViewInfo(FButtons[AButtonIndex]).Button.GetImageSize;
  APainter := NavigatorOwner.GetNavigatorLookAndFeel.GetAvailablePainter(totButton);
  APadding := APainter.NavigatorScaledButtonGlyphPadding(ScaleFactor);
  Inc(Result.cx, APadding.Left + APadding.Right);
  Inc(Result.cy, APadding.Top + APadding.Bottom);
  AMinSize := APainter.NavigatorScaledButtonMinSize(ScaleFactor);
  Result.cx := Max(Result.cx, AMinSize.cx);
  Result.cy := Max(Result.cy, AMinSize.cy);
end;

function TcxNavigatorViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := dxGetScaleFactorForInterface(NavigatorOwner);
end;

procedure TcxNavigatorViewInfo.DoButtonPressTimer(Sender: TObject);

  function NeedStopTimer: Boolean;
  begin
    Result := not (ssLeft in KeyboardStateToShiftState);
  end;

begin
  if NeedStopTimer then
  begin
    FButtonPressTimer.Enabled := False;
    MouseUp(mbLeft, -1, -1);
    Exit;
  end;

  FButtonPressTimer.Interval := cxButtonsRepeatPause;
  if (HotTrackButtonViewInfo <> nil) and (HotTrackButtonViewInfo = PressedButtonViewInfo) and
      PressedButtonViewInfo.Enabled and (PressedButtonViewInfo.Button.DefaultIndex in [NBDI_PRIOR, NBDI_NEXT]) then
    try
      PressedButtonViewInfo.Button.DoClick;
    except
      FButtonPressTimer.Enabled := False;
      raise;
    end;
end;

procedure TcxNavigatorViewInfo.UpdateSelected;
var
  AIsSelected: Boolean;
  ANavigatorControl: TWinControl;
  R1, R2: TRect;
begin
  if not NavigatorOwner.GetNavigatorLookAndFeel.Painter.IsButtonHotTrack then
    Exit;
  ANavigatorControl := NavigatorOwner.GetNavigatorControl;
  AIsSelected := (HotTrackButtonViewInfo <> nil) or (PressedButtonViewInfo <> nil) or
    NavigatorOwner.GetNavigatorFocused or
    (not FIsInplace and HasInfoPanel and ANavigatorControl.HandleAllocated and
    PtInRect(InfoPanelViewInfo.Bounds, ANavigatorControl.ScreenToClient(GetMouseCursorPos)));
  if (AIsSelected <> FIsSelected) and ANavigatorControl.HandleAllocated then
  begin
    FIsSelected := AIsSelected;
    R1 := NavigatorOwner.GetNavigatorBounds;
    R2 := cxRectInflate(R1, 1, 1);
    InternalInvalidate(ANavigatorControl.Handle, R2, R1);
  end;
end;

{ TcxNavigatorControlViewInfo }

function TcxNavigatorControlViewInfo.CanButtonOverlapBorder: Boolean;
var
  ANavigator: TcxCustomNavigator;
begin
  ANavigator := NavigatorOwner.GetNavigatorControl as TcxCustomNavigator;
  Result := (ANavigator.BorderStyle = nbsDefault) and
    ANavigator.LookAndFeelPainter.NavigatorBorderOverlap;
end;

{ TcxInplaceNavigatorViewInfo }

procedure TcxInplaceNavigatorViewInfo.Calculate;
var
  AButtonIndent, AInfoPanelWidth: Integer;
  AInfoPanelText: string;
begin
  MakeDirty;
  ValidateButtonsViewInfos;
  AInfoPanelText := GetInfoPanelText;
  FNavigatorSize := cxNullSize;
  CalculateSize(FNavigatorSize.cx, FNavigatorSize.cy, AInfoPanelText, AButtonIndent, AInfoPanelWidth);
  UpdateNavigatorSiteBounds(GetNavigatorSiteBounds);
  CalculateBounds(AInfoPanelText, AInfoPanelWidth, FNavigatorSize.cy, AButtonIndent);
  CalculateButtons;
  FIsDirty := False;
end;

function TcxInplaceNavigatorViewInfo.GetNavigatorBounds: TRect;
begin
  Result := GetNavigatorSiteBounds;
  if NavigatorOwner.GetNavigatorControl.UseRightToLeftAlignment then
    Result.Left := Result.Right - FNavigatorSize.cx
  else
    Result.Right := Result.Left + FNavigatorSize.cx;
end;

function TcxInplaceNavigatorViewInfo.GetControl: TcxControl;
begin
  dxAbstractError;
  Result := nil;
end;

function TcxInplaceNavigatorViewInfo.GetHeight: Integer;
begin
  Result := Max(Max(FNavigatorSize.cy,
    GetLookAndFeelPainter.NavigatorScaledButtonMinSize(ScaleFactor).cy),
    GetSystemMetrics(SM_CYHSCROLL));
  if (InfoPanel <> nil) and InfoPanel.Visible then
    Result := Max(Result, cxTextHeight(GetInfoPanelFont));
  if FIsDirty then
    ValidateButtonsViewInfos;
  Result := Max(Result, Buttons.Size.cy);
end;

function TcxInplaceNavigatorViewInfo.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := NavigatorOwner.GetNavigatorLookAndFeel.Painter;
end;

function TcxInplaceNavigatorViewInfo.GetMinButtonSize(AButtonIndex: Integer): TSize;
begin
  Result := inherited GetMinButtonSize(AButtonIndex);
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TcxInplaceNavigatorViewInfo.GetNavigatorOffset: Integer;
begin
  dxAbstractError;
  Result := 0;
end;

function TcxInplaceNavigatorViewInfo.GetNavigatorSiteBounds: TRect;
var
  AOwnerControl: TcxControlAccess;
begin
  AOwnerControl := TcxControlAccess(Control);
  if not AOwnerControl.IsPopupScrollBars and AOwnerControl.HScrollBarVisible then
  begin
    Result := AOwnerControl.GetHScrollBarBounds;
    if Control.UseRightToLeftAlignment then
    begin
      Result.Left := Result.Right;
      Result.Right := Result.Left + GetWidth;
    end
    else
    begin
      Result.Right := Result.Left;
      Result.Left := Result.Right - GetWidth;
    end;
  end
  else
  begin
    Result := AOwnerControl.ClientBounds;
    Result.Top := Result.Bottom;
    Inc(Result.Bottom, GetHeight);
  end;
end;

function TcxInplaceNavigatorViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := TcxControlAccess(Control).ScaleFactor;
end;

function TcxInplaceNavigatorViewInfo.GetWidth: Integer;
begin
  Result := FNavigatorSize.cx + GetNavigatorOffset;
end;

function TcxInplaceNavigatorViewInfo.IsNavigatorSizeChanged: Boolean;
begin
  Result := not cxSizeIsEqual(FNavigatorSize, CalculateMinSize);
end;

procedure TcxInplaceNavigatorViewInfo.UpdateNavigatorSiteBounds(const ABounds: TRect);
begin
  dxAbstractError;
end;

{ TcxCustomNavigator }

constructor TcxCustomNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  FButtons := CreateButtons;
  FInfoPanel := CreateInfoPanel;
  FViewInfo := GetViewInfoClass.Create(Self, False);
  FBorderStyle := nbsDefault;
  TabStop := False;
  Height := 25;
end;

destructor TcxCustomNavigator.Destroy;
begin
  EndMouseTracking(Self);
  FreeAndNil(FViewInfo);
  FreeAndNil(FButtons);
  FreeAndNil(FInfoPanel);
  inherited Destroy;
end;

procedure TcxCustomNavigator.ClickButton(Index: Integer);
begin
  CustomButtons.ClickButton(Index);
end;

procedure TcxCustomNavigator.RestoreButtons;
var
  I: Integer;
begin
  with CustomButtons do
    for I := 0 to NavigatorButtonCount - 1 do
      Buttons[I].FIsVisibleAssigned := False;
  InitButtons;
end;

procedure TcxCustomNavigator.SetBorderStyle(const Value: TcxNavigatorBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RefreshNavigator;
  end;
end;

function TcxCustomNavigator.CanFocusOnClick: Boolean;
begin
  Result := inherited CanFocusOnClick and TabStop;
end;

function TcxCustomNavigator.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := inherited CanResize(NewWidth, NewHeight);
  if Result and (FViewInfo <> nil) and HandleAllocated and not IsScaleChanging then
    CheckSize(NewWidth, NewHeight);
end;

function TcxCustomNavigator.CreateButtons: TcxCustomNavigatorButtons;
begin
  Result := TcxCustomNavigatorButtons.Create(Self);
end;

procedure TcxCustomNavigator.DoPaint;
begin
  case BorderStyle of
    nbsDefault:
      FViewInfo.PaintBorder(Bounds);
    nbsAlways:
      begin
        LookAndFeelPainter.DrawBorder(Canvas, Bounds);
        Canvas.IntersectClipRect(ClientBounds);
      end;
  end;
  FViewInfo.Paint;
end;

procedure TcxCustomNavigator.DoEnter;
begin
  inherited DoEnter;
  FViewInfo.DoEnter;
end;

procedure TcxCustomNavigator.DoExit;
begin
  inherited DoExit;
  FViewInfo.DoExit;
end;

procedure TcxCustomNavigator.EnabledChanged;
begin
  RefreshNavigator;
end;

procedure TcxCustomNavigator.FontChanged;
begin
  inherited;
  RefreshNavigator;
end;

function TcxCustomNavigator.GetBorderSize: Integer;
begin
  case BorderStyle of
    nbsDefault:
      Result := LookAndFeelPainter.NavigatorBorderSize;
    nbsAlways:
      Result := LookAndFeelPainter.BorderSize;
  else
    // nbsNone
    Result := 0;
  end;
end;

procedure TcxCustomNavigator.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_LEFT) or (Key = VK_RIGHT) then
    FViewInfo.PressArrowKey(Key = VK_LEFT);
  if (Key = VK_SPACE) and (FViewInfo.FocusedButton <> nil)and FViewInfo.FocusedButton.Enabled then
    FViewInfo.FocusedButton.Button.DoClick;
end;

procedure TcxCustomNavigator.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
    FViewInfo.MouseDown(X, Y);
end;

procedure TcxCustomNavigator.MouseEnter(AControl: TControl);
begin
  inherited MouseEnter(AControl);
  if IsDesigning then
    Exit;
  BeginMouseTracking(Self, Bounds, Self);
//  Invalidate;   //#TODO:
end;

procedure TcxCustomNavigator.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  if IsDesigning then
    Exit;
  EndMouseTracking(Self);
  FViewInfo.MouseLeave;
end;

procedure TcxCustomNavigator.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  FViewInfo.MouseMove(X, Y);
  BeginMouseTracking(Self, Bounds, Self);
end;

procedure TcxCustomNavigator.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FViewInfo.MouseUp(Button, X, Y);
end;

procedure TcxCustomNavigator.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  StoreLayoutParams;
  try
    inherited LookAndFeelChanged(Sender, AChangedValues);
    RecreateWnd;
  finally
    RestoreLayoutParams;
  end;
end;

procedure TcxCustomNavigator.NavigatorMouseLeave;
begin
  FViewInfo.MouseMove(-1, -1);
end;

procedure TcxCustomNavigator.NavigatorChanged(AChangeType: TcxNavigatorChangeType);
begin
end;

function TcxCustomNavigator.GetNavigatorBounds: TRect;
begin
  Result := ClientBounds;
end;

function TcxCustomNavigator.GetNavigatorButtons: TcxCustomNavigatorButtons;
begin
  Result := CustomButtons;
end;

function TcxCustomNavigator.GetNavigatorCanvas: TCanvas;
begin
  Result := Canvas.Canvas;
end;

function TcxCustomNavigator.GetNavigatorControl: TWinControl;
begin
  Result := Self;
end;

function TcxCustomNavigator.GetNavigatorLookAndFeel: TcxLookAndFeel;
begin
  Result := LookAndFeel;
end;

function TcxCustomNavigator.GetNavigatorFocused: Boolean;
begin
  Result := Focused;
end;

function TcxCustomNavigator.GetNavigatorShowHint: Boolean;
begin
  Result := ShowHint;
end;

function TcxCustomNavigator.GetNavigatorTabStop: Boolean;
begin
  Result := TabStop;
end;

function TcxCustomNavigator.GetNavigatorInfoPanel: TcxCustomNavigatorInfoPanel;
begin
  Result := CustomInfoPanel;
end;

procedure TcxCustomNavigator.GetAdornerTargetElements(AList: TStrings);
begin
  inherited GetAdornerTargetElements(AList);
  ViewInfo.AddAdornerTargetElements(AList);
end;

procedure TcxCustomNavigator.CheckSize(var AWidth, AHeight: Integer);
var
  ABorderSize: Integer;
begin
  ABorderSize := BorderSize;
  Dec(AWidth, 2 * ABorderSize);
  Dec(AHeight, 2 * ABorderSize);
  FViewInfo.CheckSize(AWidth, AHeight);
  Inc(AWidth, 2 * ABorderSize);
  Inc(AHeight, 2 * ABorderSize);
end;

function TcxCustomNavigator.GetPaintBlackOpaqueOnGlass: Boolean;
begin
  Result := LookAndFeelPainter.LookAndFeelStyle in [lfsNative, lfsSkin];
end;

function TcxCustomNavigator.GetViewInfoClass: TcxNavigatorViewInfoClass;
begin
  Result := TcxNavigatorControlViewInfo;
end;

procedure TcxCustomNavigator.InitButtons;
var
  I: Integer;
begin
  for I := 0 to NavigatorButtonCount - 1 do
    FButtons[I].RestoreDefaultVisible(True);
end;

function TcxCustomNavigator.GetNavigatorOwner: TComponent;
begin
  Result := Self;
end;

procedure TcxCustomNavigator.NavigatorStateChanged;
begin
  FViewInfo.Update;
end;

procedure TcxCustomNavigator.RefreshNavigator;
var
  AWidth, AHeight: Integer;
begin
  if HandleAllocated and not IsScaleChanging then
  begin
    AWidth := Width;
    AHeight := Height;
    CheckSize(AWidth, AHeight);
    if (AWidth <> Width) or (AHeight <> Height) then
      SetBounds(Left, Top, AWidth, AHeight)
    else
      Invalidate;
  end;
end;

procedure TcxCustomNavigator.RestoreLayoutParams;
var
  AMinSize: TSize;
begin
  if FSavedButtonIndent >= 0 then
  begin
    AMinSize := cxNullSize;
    CheckSize(AMinSize.cx, AMinSize.cy);
    Width := AMinSize.cx + FSavedButtonIndent * ViewInfo.ButtonCount;
  end;
end;

procedure TcxCustomNavigator.StoreLayoutParams;
begin
  if HandleAllocated then
    FSavedButtonIndent := ViewInfo.ButtonIndent
  else
    FSavedButtonIndent := -1;
end;

procedure TcxCustomNavigator.CreateWnd;
begin
  inherited CreateWnd;
  RefreshNavigator;
end;

procedure TcxCustomNavigator.WndProc(var Message: TMessage);
begin
  if FViewInfo <> nil then
    with Message do
      if ((Msg >= WM_KEYFIRST) and (Msg <= WM_KEYLAST)) or
        ((Msg = CM_ACTIVATE) or (Msg = CM_DEACTIVATE)) or
        (Msg = CM_APPKEYDOWN) or (Msg = CM_APPSYSCOMMAND) or
        (Msg = WM_COMMAND) or ((Msg > WM_MOUSEMOVE) and
        (Msg <= WM_MOUSELAST)) or (Msg = WM_NCMOUSEMOVE) then
         FViewInfo.CancelHint;
  inherited WndProc(Message);
end;

procedure TcxCustomNavigator.ScaleFactorChanging;
begin
  inherited ScaleFactorChanging;
  StoreLayoutParams;
end;

procedure TcxCustomNavigator.ScaleFactorChanged;
begin
  inherited ScaleFactorChanged;
  RestoreLayoutParams;
end;

procedure TcxCustomNavigator.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TcxCustomNavigator.CMHintShow(var Message: TCMHintShow);
begin
  Message.Result := 1;
end;

{ TcxNavigatorControlNotifier }

constructor TcxNavigatorControlNotifier.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TcxNavigatorControlNotifier.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TcxNavigatorControlNotifier.AddNavigator(ANavigator: IcxNavigatorOwner);
begin
  if FList.IndexOf(Pointer(ANavigator)) < 0 then
    FList.Add(Pointer(ANavigator));
end;

procedure TcxNavigatorControlNotifier.RemoveNavigator(ANavigator: IcxNavigatorOwner);
begin
  FList.Remove(Pointer(ANavigator));
end;

procedure TcxNavigatorControlNotifier.RefreshNavigatorButtons;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    IcxNavigatorOwner(FList[I]).NavigatorStateChanged;
end;

{ TcxNavigatorControlButtons }

procedure TcxNavigatorControlButtons.ReleaseBookmark;
var
  ANavigatorControl: IcxNavigator;
begin
  ANavigatorControl := Control;
  if (ANavigatorControl <> nil) and ANavigatorControl.IsBookmarkAvailable then
    ANavigatorControl.ClearBookmark;
end;

function TcxNavigatorControlButtons.GetControl: IcxNavigator;
begin
  if Assigned(FOnGetControl) then
    Result := FOnGetControl
  else
    Result := nil;
end;

procedure TcxNavigatorControlButtons.DoButtonClick(ADefaultIndex: Integer);
var
  ANavigatorControl: IcxNavigator;
begin
  ANavigatorControl := Control;
  if (ANavigatorControl <> nil) and ((ADefaultIndex <> NBDI_DELETE) or not ConfirmDelete or
      ((MessageDlg(cxGetResourceString(@cxNavigator_DeleteRecordQuestion),
        mtConfirmation, mbOKCancel, 0) = ID_OK))) then
    ANavigatorControl.DoAction(ADefaultIndex);
  Navigator.NavigatorStateChanged;
end;

function TcxNavigatorControlButtons.GetButtonEnabled(ADefaultIndex: Integer): Boolean;
var
  ANavigatorControl: IcxNavigator;
begin
  ANavigatorControl := Control;
  Result := (ANavigatorControl <> nil) and IsNavigatorEnabled and
    (ANavigatorControl.IsActive or (ADefaultIndex = NBDI_FILTER));
  if Result then
    case ADefaultIndex of
      NBDI_FIRST, NBDI_PRIOR, NBDI_PRIORPAGE:
        Result := not ANavigatorControl.IsBof;
      NBDI_LAST, NBDI_NEXT, NBDI_NEXTPAGE:
        Result := not ANavigatorControl.IsEof;
      NBDI_INSERT:
        Result := ANavigatorControl.CanInsert;
      NBDI_APPEND:
        Result := ANavigatorControl.CanAppend;
      NBDI_DELETE:
        Result := ANavigatorControl.CanDelete;
      NBDI_EDIT:
        Result := ANavigatorControl.CanEdit and not ANavigatorControl.IsEditing;
      NBDI_POST, NBDI_CANCEL:
        Result := ANavigatorControl.IsEditing;
      NBDI_GOTOBOOKMARK:
        Result := ANavigatorControl.IsBookmarkAvailable;
    end;
end;

{ TcxNavigatorControlInfoPanel }

procedure TcxNavigatorControlInfoPanel.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxNavigatorControlInfoPanel then
    Font := TcxNavigatorControlInfoPanel(Source).Font;
end;

function TcxNavigatorControlInfoPanel.GetControl: TcxCustomNavigator;
begin
  Result := Navigator.GetNavigatorControl as TcxCustomNavigator;
end;

function TcxNavigatorControlInfoPanel.GetFont: TFont;
begin
  Result := Control.Font;
end;

function TcxNavigatorControlInfoPanel.GetParentFont: Boolean;
begin
  Result := Control.ParentFont;
end;

function TcxNavigatorControlInfoPanel.GetViewParams: TcxViewParams;
begin
  Result.Font := Font;
  if ParentFont then
    Result.TextColor := Navigator.GetNavigatorLookAndFeel.Painter.NavigatorInfoPanelTextColor
  else
    Result.TextColor := Result.Font.Color;

  Result.Bitmap := nil;
  Result.Color := Navigator.GetNavigatorLookAndFeel.Painter.NavigatorInfoPanelColor;
end;

procedure TcxNavigatorControlInfoPanel.ResetFont;
begin
  Font.Color := clDefault;
end;

function TcxNavigatorControlInfoPanel.IsFontStored: Boolean;
begin
  Result := not ParentFont;
end;

procedure TcxNavigatorControlInfoPanel.SetFont(Value: TFont);
begin
  Control.Font := Value;
end;

procedure TcxNavigatorControlInfoPanel.SetParentFont(const Value: Boolean);
begin
  if ParentFont <> Value then
  begin
    Control.ParentFont := Value;
    if not Value then
      ResetFont;
  end;
end;

{ TcxCustomNavigatorControl }

destructor TcxCustomNavigatorControl.Destroy;
begin
  Control := nil;
  inherited Destroy;
end;

function TcxCustomNavigatorControl.CreateButtons: TcxCustomNavigatorButtons;
begin
  Result := GetButtonsClass.Create(Self);
  TcxNavigatorControlButtons(Result).OnGetControl := GetIcxNavigator;
end;

function TcxCustomNavigatorControl.CreateInfoPanel: TcxCustomNavigatorInfoPanel;
begin
  Result := GetInfoPanelClass.Create(Self);
  Result.OnGetIRecordPosition := GetIRecordPosition;
end;

function TcxCustomNavigatorControl.GetButtonsClass: TcxNavigatorControlButtonsClass;
begin
  Result := TcxNavigatorControlButtons;
end;

function TcxCustomNavigatorControl.GetInfoPanelClass: TcxNavigatorControlInfoPanelClass;
begin
  Result := TcxNavigatorControlInfoPanel;
end;

procedure TcxCustomNavigatorControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Control) then
    Control := nil;
end;

procedure TcxCustomNavigatorControl.InitButtons;
var
  ANavigator: IcxNavigator;
  I: Integer;
begin
  if FControl = nil then
  begin
    for I := 0 to NavigatorButtonCount - 1 do
      FButtons[I].RestoreDefaultVisible(True);
  end
  else
  begin
    Supports(FControl, IcxNavigator, ANavigator);
    for I := 0 to NavigatorButtonCount - 1 do
      FButtons[I].RestoreDefaultVisible(ANavigator.IsActionSupported(FButtons[I].DefaultIndex));
  end;
end;

function TcxCustomNavigatorControl.GetButtons: TcxNavigatorControlButtons;
begin
  Result := TcxNavigatorControlButtons(CustomButtons);
end;

function TcxCustomNavigatorControl.GetIcxNavigator: IcxNavigator;
begin
  if Control <> nil then
    Supports(Control, IcxNavigator, Result)
  else
    Result := nil;
end;

function TcxCustomNavigatorControl.GetIRecordPosition: IcxNavigatorRecordPosition;
begin
  if Control <> nil then
    Supports(Control, IcxNavigatorRecordPosition, Result)
  else
    Result := nil;
end;

function TcxCustomNavigatorControl.GetInfoPanel: TcxNavigatorControlInfoPanel;
begin
  Result := TcxNavigatorControlInfoPanel(CustomInfoPanel);
end;

procedure TcxCustomNavigatorControl.SetButtons(Value: TcxNavigatorControlButtons);
begin
  CustomButtons.Assign(Value);
end;

procedure TcxCustomNavigatorControl.SetControl(Value: TComponent);
var
  ANavigator: IcxNavigator;
begin
  if (Value <> FControl) and ((Value = nil) or Supports(Value, IcxNavigator, ANavigator)) then
  begin
    if FControl <> nil then
    begin
      FControl.RemoveFreeNotification(Self);
      Supports(FControl, IcxNavigator, ANavigator);
      if ANavigator.GetNotifier <> nil then
        ANavigator.GetNotifier.RemoveNavigator(Self);
    end;
    FControl := Value;
    if (FControl <> nil) then
    begin
      FControl.FreeNotification(Self);
      Supports(FControl, IcxNavigator, ANavigator);
      if ANavigator.GetNotifier <> nil then
        ANavigator.GetNotifier.AddNavigator(Self);
    end;
    InitButtons;
    RefreshNavigator;
  end;
end;

procedure TcxCustomNavigatorControl.SetInfoPanel(Value: TcxNavigatorControlInfoPanel);
begin
  CustomInfoPanel.Assign(Value);
end;

initialization

finalization
  FreeAndNil(FNavigatorImages);

end.

