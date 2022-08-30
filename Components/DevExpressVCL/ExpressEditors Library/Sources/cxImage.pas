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

unit cxImage;

{$I cxVer.inc}

interface

uses
  Types, Variants,
  Windows, Messages, ExtDlgs, SysUtils, Classes, Clipbrd, Controls, Dialogs,
  ExtCtrls, Forms, Graphics, Menus, StdCtrls,
  dxCore, dxCoreClasses, cxClasses, cxContainer, cxControls, cxGraphics, cxLookAndFeels, cxGeometry,
  cxLookAndFeelPainters, cxDataUtils, cxVariants, dxTouch, dxSkinsCore, dxCoreGraphics, dxTypeHelpers,
  cxEdit, cxEditConsts, ImgList, dxZoomTrackBar, cxTrackBar, dxGDIPlusApi, dxGDIPlusClasses;

const
  cxImageDefaultInplaceHeight = 15;

type
  TcxCustomImage = class;
  TcxPopupMenuItem = (pmiCut, pmiCopy, pmiPaste, pmiDelete, pmiLoad, pmiWebCam, pmiSave,
    pmiCustom);
  TcxPopupMenuItemClick = procedure(Sender: TObject;
    MenuItem: TcxPopupMenuItem) of object;
  TcxPopupMenuItems = set of TcxPopupMenuItem;

  { TcxPopupMenuLayout }

  TcxPopupMenuLayout = class(TPersistent)
  private
    FCustomMenuItemCaption: string;
    FCustomMenuItemGlyph: TBitmap;
    FImage: TcxCustomImage;
    FMenuItems: TcxPopupMenuItems;
    function GetCustomMenuItemGlyph: TBitmap; virtual;
    procedure SetCustomMenuItemGlyph(Value: TBitmap);
  public
    constructor Create(AImage: TcxCustomImage);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property MenuItems: TcxPopupMenuItems read FMenuItems write FMenuItems default
      [pmiCut, pmiCopy, pmiPaste, pmiDelete, pmiLoad, pmiWebCam, pmiSave];
    property CustomMenuItemCaption: string read FCustomMenuItemCaption write FCustomMenuItemCaption;
    property CustomMenuItemGlyph: TBitmap read GetCustomMenuItemGlyph write SetCustomMenuItemGlyph;
  end;

  { TcxImageViewInfo }

  TcxImageViewInfo = class(TcxCustomEditViewInfo)
  private
    FFreePicture: Boolean;
    FUseRightToLeftScrollBar: Boolean;
    FZoomTrackBarViewInfo: TdxCustomZoomTrackBarViewInfo;

    procedure DrawTransparentBackground(ACanvas: TcxCanvas; const R: TRect);
  protected
    function CreateZoomTrackBarViewInfo: TdxCustomZoomTrackBarViewInfo;
    procedure InternalPaint(ACanvas: TcxCanvas); override;
    function IsRepaintOnStateChangingNeeded: Boolean; override;

    function GetRealStretch: Boolean;
  public
    ShowFocusRect: Boolean;
    TopLeft: TPoint;
    Caption: string;
    Center: Boolean;
    Picture: TPicture;
    FitMode: TcxImageFitMode;
    ZoomPercent: Integer;
    Scalable: Boolean;
    ShowZoomTrackBar: Boolean;

    constructor Create; override;
    destructor Destroy; override;
    //
    property ZoomTrackBarViewInfo: TdxCustomZoomTrackBarViewInfo read FZoomTrackBarViewInfo;
  end;

  { TcxImageViewData }

  TcxImageViewData = class(TcxCustomEditViewData)
  protected
    procedure CalculateZoomTrackBar(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint; Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean; AZoomPercent: Integer);
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
      AIsMouseEvent: Boolean); override;
    procedure EditValueToDrawValue(const AEditValue: TcxEditValue;
      AViewInfo: TcxCustomEditViewInfo); override;
    function GetEditContentSize(ACanvas: TcxCanvas;
      const AEditValue: TcxEditValue;
      const AEditSizeProperties: TcxEditSizeProperties; AErrorData: TcxEditValidateInfo = nil): TSize; override;
  end;

  { TcxCustomImageProperties }

  TcxImageAssignPictureEvent = procedure(Sender: TObject;
    const Picture: TPicture) of object;
  TcxImageGraphicClassEvent = procedure(AItem: TObject; ARecordIndex: Integer;
    APastingFromClipboard: Boolean; var AGraphicClass: TGraphicClass) of object;
  TcxImageEditGraphicClassEvent = procedure(Sender: TObject;
    APastingFromClipboard: Boolean; var AGraphicClass: TGraphicClass) of object;

  TcxImageTransparency = (gtDefault, gtOpaque, gtTransparent);

  TcxCustomImageProperties = class(TcxCustomEditProperties)
  private
    FCaption: string;
    FCustomFilter: string;
    FDefaultHeight: Integer;
    FGraphicClass: TGraphicClass;
    FGraphicTransparency: TcxImageTransparency;
    FNeedUpdateImage: Boolean;
    FPopupMenuLayout: TcxPopupMenuLayout;
    FShowFocusRect: Boolean;
    FZoomTrackBarProperties: TdxCustomZoomTrackBarProperties;

    FCenter: Boolean;
    FFitMode: TcxImageFitMode;

    FOnAssignPicture: TcxImageAssignPictureEvent;
    FOnCustomClick: TNotifyEvent;
    FOnGetGraphicClass: TcxImageGraphicClassEvent;

    function GetGraphicClassName: string;
    function IsGraphicClassNameStored: Boolean;
    procedure ReadIsGraphicClassNameEmpty(Reader: TReader);
    procedure SetCaption(const Value: string);
    procedure SetGraphicClass(const Value: TGraphicClass);
    procedure SetGraphicClassName(const Value: string);
    procedure SetGraphicTransparency(Value: TcxImageTransparency);
    procedure SetPopupMenuLayout(Value: TcxPopupMenuLayout);
    procedure SetShowFocusRect(Value: Boolean);
    procedure WriteIsGraphicClassNameEmpty(Writer: TWriter);

    function CalculateFitMode(AProportional, AStretch: Boolean): TcxImageFitMode;
    function GetProportional: Boolean;
    function GetStretch: Boolean;
    procedure SetCenter(AValue: Boolean);
    procedure SetProportional(AValue: Boolean);
    procedure SetStretch(AValue: Boolean);
    procedure SetFitMode(AValue: TcxImageFitMode);
    procedure SetZoomTrackBarProperties(AValue: TdxCustomZoomTrackBarProperties);
  protected
    function CanValidate: Boolean; override;
    function CreateZoomBarProperties: TdxCustomZoomTrackBarProperties; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;

    function IsDesigning: Boolean;
    function GetDefaultGraphicClass: TGraphicClass; virtual;
    function GetRealStretch(const APictureSize, ABoundsSize: TSize): Boolean;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function IsScrollbarsNeeded(APicture: TPicture): Boolean;


    property DefaultHeight: Integer read FDefaultHeight write FDefaultHeight default cxImageDefaultInplaceHeight;
    property ZoomTrackBarProperties: TdxCustomZoomTrackBarProperties read FZoomTrackBarProperties write SetZoomTrackBarProperties;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetDisplayText(const AEditValue: TcxEditValue; AFullText: Boolean = False; AIsInplace: Boolean = True): string; override;
    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; override;
    function GetGraphicClass(AItem: TObject; ARecordIndex: Integer; APastingFromClipboard: Boolean = False): TGraphicClass; virtual;
    function GetSpecialFeatures: TcxEditSpecialFeatures; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function IsResetEditClass: Boolean; override;
    procedure ValidateDisplayValue(var DisplayValue: TcxEditValue; var ErrorText: TCaption; var Error: Boolean; AEdit: TcxCustomEdit); override;
    property GraphicClass: TGraphicClass read FGraphicClass write SetGraphicClass;
    // !!!
    property Caption: string read FCaption write SetCaption;
    property Center: Boolean read FCenter write SetCenter default True;
    property CustomFilter: string read FCustomFilter write FCustomFilter;

    property FitMode: TcxImageFitMode read FFitMode write SetFitMode default ifmFit;

    property GraphicClassName: string read GetGraphicClassName write SetGraphicClassName stored IsGraphicClassNameStored;
    property GraphicTransparency: TcxImageTransparency read FGraphicTransparency write SetGraphicTransparency default gtDefault;
    property PopupMenuLayout: TcxPopupMenuLayout read FPopupMenuLayout write SetPopupMenuLayout;
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect default True;
    property OnAssignPicture: TcxImageAssignPictureEvent
      read FOnAssignPicture write FOnAssignPicture;
    property OnCustomClick: TNotifyEvent read FOnCustomClick
      write FOnCustomClick;
    property OnGetGraphicClass: TcxImageGraphicClassEvent read FOnGetGraphicClass
      write FOnGetGraphicClass;

    //obsolete
    property Proportional: Boolean read GetProportional write SetProportional stored False;
    property Stretch: Boolean read GetStretch write SetStretch stored False;
  end;

  { TcxImageProperties }

  TcxImageProperties = class(TcxCustomImageProperties)
  published
    property AssignedValues;
    property Caption;
    property Center;
    property ClearKey;
    property CustomFilter;
    property FitMode;
    property GraphicClassName;
    property GraphicTransparency;
    property ImmediatePost;
    property PopupMenuLayout;
    property Proportional;
    property ReadOnly;
    property ShowFocusRect;
    property Stretch;
    property OnAssignPicture;
    property OnChange;
    property OnCustomClick;
    property OnEditValueChanged;
    property OnGetGraphicClass;
  end;

  { TcxImageZoomingOptions }

  TcxImageZoomingOptions = class(TPersistent)
  private
    FMaxZoom: Integer;
    FMinZoom: Integer;
    FZoomPercent: Integer;
    FShowZoomTrackBar: Boolean;
    FOnChanged: TNotifyEvent;

    procedure SetMaxZoom(AValue: Integer);
    procedure SetMinZoom(AValue: Integer);
    procedure SetZoomPercent(AValue: Integer);
    procedure SetShowZoomTrackBar(AValue: Boolean);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Changed; virtual;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property MaxZoom: Integer read FMaxZoom write SetMaxZoom default 500;
    property MinZoom: Integer read FMinZoom write SetMinZoom default 0;
    property ZoomPercent: Integer read FZoomPercent write SetZoomPercent default 100;
    property ShowZoomTrackBar: Boolean read FShowZoomTrackBar write SetShowZoomTrackBar default False;
  end;

  { TcxImageAnimationOptions }

  TcxImageAnimationOptions = class(TcxOwnedPersistent)
  private
    FAnimation: TdxDefaultBoolean;
    FAnimationLoop: TdxDefaultBoolean;
    FGraphic: TdxGPImage;
    function GetGraphic: TdxGPImage;
    function GetOwnerImage: TcxCustomImage;
    procedure SetAnimation(AValue: TdxDefaultBoolean);
    procedure SetAnimationLoop(AValue: TdxDefaultBoolean);
  protected
    procedure CheckGraphic;
    procedure Changed; virtual;

    property Graphic: TdxGPImage read GetGraphic;
    property Owner: TcxCustomImage read GetOwnerImage;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Animation: TdxDefaultBoolean read FAnimation write SetAnimation default bDefault;
    property AnimationLoop: TdxDefaultBoolean read FAnimationLoop write SetAnimationLoop default bDefault;
  end;

  { TcxCustomImage }

  TcxCustomImage = class(TcxCustomEdit, IdxScrollingControl, IdxZoomClient, IcxTrackBar)
  private
    FAnimationOptions: TcxImageAnimationOptions;
    FClipboardFormat: Word;
    FInternalPopupMenu: TPopupMenu;
    FIsDialogShowed: Boolean;
    FPicture: TPicture;
    FOnGetGraphicClass: TcxImageEditGraphicClassEvent;
    FLastDragPos: TPoint;
    FZoomPercent: Integer;
    FZoomingOptions: TcxImageZoomingOptions;
    FZoomTrackBarController: TdxZoomTrackBarController;
    FZoomTrackBarSliding: Boolean;

    function AllowScaling: Boolean;
    function CanZoomPercentByMouseWheel(AShift: TShiftState; const AMousePos: TPoint): Boolean;
    procedure DecZoomPercent;
    procedure EditAndClear;
    procedure EditPopupMenuClick(Sender: TObject);
    function GetProperties: TcxCustomImageProperties;
    function GetViewInfo: TcxImageViewInfo;
    function GetActiveProperties: TcxCustomImageProperties;
    procedure IncZoomPercent;
    procedure MenuItemClick(Sender: TObject; MenuItem: TcxPopupMenuItem);
    procedure PictureChanged(Sender: TObject);
    procedure CheckZoomPercent;
    procedure SetAnimationOptions(Value: TcxImageAnimationOptions);
    procedure SetPicture(Value: TPicture);
    procedure SetProperties(const Value: TcxCustomImageProperties);
  protected
    // TControl
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    // TcxControl
    function AllowPan(AScrollKind: TScrollBarKind): Boolean; override;
    procedure BoundsChanged; override;
    function GetDefaultPanOptions: Integer; override;
    function InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function IsDefaultGesture(AGestureID: Integer): Boolean; override;
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function IsPanArea(const APoint: TPoint): Boolean; override;

    // TcxEdit
    function CanAutoSize: Boolean; override;
    function CanAutoWidth: Boolean; override;
    procedure Initialize; override;
    function GetEditValue: TcxEditValue; override;
    procedure InternalSetEditValue(const Value: TcxEditValue; AIsValueValid: Boolean); override;
    procedure PropertiesChanged(Sender: TObject); override;

    { IdxScrollingControl }
    function GetLeftPos: Integer;
    procedure SetLeftPos(Value: Integer);
    function GetTopPos: Integer;
    procedure SetTopPos(Value: Integer);
    function GetContentSize: TSize;
    function GetClientSize: TSize;
    function GetInstance: TcxControl;

    { IdxZoomClient }
    procedure Zoom(ADelta: Integer; var AHandled: Boolean);

    { IdxTrackBar }
    function GetZoomPercent: Integer;
    function GetZoomTrackBarActiveProperties: TcxCustomTrackBarProperties;
    function GetZoomTrackBarMouseDownPos: TPoint;
    function GetZoomTrackBarProperties: TcxCustomTrackBarProperties;
    function GetZoomTrackBarViewInfo: TcxCustomTrackBarViewInfo;
    function IsZoomTrackBarInplace: Boolean;
    procedure InternalSetZoomPercent(AValue: Integer);
    function IcxTrackBar.GetActiveProperties = GetZoomTrackBarActiveProperties;
    function IcxTrackBar.GetMouseDownPos = GetZoomTrackBarMouseDownPos;
    function IcxTrackBar.GetPosition = GetZoomPercent;
    function IcxTrackBar.GetProperties= GetZoomTrackBarProperties;
    function IcxTrackBar.GetViewInfo = GetZoomTrackBarViewInfo;
    function IcxTrackBar.IsInplace = IsZoomTrackBarInplace;
    procedure IcxTrackBar.SetPosition = InternalSetZoomPercent;

    // scrolling
    function AllowHybridScrollbarMode: Boolean; override;
    function AllowTouchScrollUIMode: Boolean; override;
    procedure Centre;
    procedure CheckScrollbars(AResetPosition: Boolean = False);
    procedure DoScrollUIModeChanged; override;
    procedure InitScrollBarsParameters; override;
    function IsScrollbarsNeeded: Boolean;
    function NeedsScrollBars: Boolean; override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;

    // creating internal objects
    function CreateAnimationOptions: TcxImageAnimationOptions;
    function CreateZoomingOptions: TcxImageZoomingOptions; virtual;
    function CreateZoomTrackBarController: TdxZoomTrackBarController; virtual;

    // own methods
    procedure AssignFromWebCam;
    function CanPasteFromClipboard: Boolean; virtual;
    procedure CustomClick; virtual;
    procedure DoOnAssignPicture;
    procedure DoPictureLoadedFromFile(AFileName: string); virtual;
    function GetGraphicClass(APastingFromClipboard: Boolean = False): TGraphicClass; virtual;
    function IsAnimationNotification: Boolean;
    procedure PreparePopup;
    procedure ZoomingOptionsChangedHandler(Sender: TObject); virtual;
    procedure SetupZoomTrackBarProperties; virtual;

    // default override
    property AutoSize default False;
    property ParentColor default False;

    // properties
    property AnimationOptions: TcxImageAnimationOptions read FAnimationOptions write SetAnimationOptions;
    property InternalPopupMenu: TPopupMenu read FInternalPopupMenu;
    property IsDialogShowed: Boolean read FIsDialogShowed;
    property ZoomPercent: Integer read GetZoomPercent write FZoomPercent;
    property ZoomingOptions: TcxImageZoomingOptions read FZoomingOptions write FZoomingOptions;
    property OnGetGraphicClass: TcxImageEditGraphicClassEvent read FOnGetGraphicClass write FOnGetGraphicClass;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyToClipboard; override;
    procedure CutToClipboard; override;
    function Focused: Boolean; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure LoadFromFile;
    procedure PasteFromClipboard; override;
    procedure SaveToFile;

    property ActiveProperties: TcxCustomImageProperties read GetActiveProperties;
    property ClipboardFormat: Word read FClipboardFormat write FClipboardFormat;
    property Picture: TPicture read FPicture write SetPicture;
    property Properties: TcxCustomImageProperties read GetProperties write SetProperties;
    property ViewInfo: TcxImageViewInfo read GetViewInfo;
  end;

  { TcxImage }

  TcxImage = class(TcxCustomImage)
  private
    function GetActiveProperties: TcxImageProperties;
    function GetProperties: TcxImageProperties;
    procedure SetProperties(Value: TcxImageProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxImageProperties read GetActiveProperties;
  published
    property Anchors;
    property AnimationOptions;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentShowHint;
    property Picture;
    property PopupMenu;
    property Properties: TcxImageProperties read GetProperties write SetProperties;
    property ZoomingOptions;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
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
    property OnGetGraphicClass;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure LoadPicture(APicture: TPicture; AGraphicClass: TGraphicClass; const AValue: Variant);
procedure SavePicture(APicture: TPicture; var AValue: AnsiString); overload;
procedure SavePicture(APicture: TPicture; var AValue: Variant); overload;

function GetGraphicClassByName(const AClassName: string): TGraphicClass;
function GetRegisteredGraphicClasses: TList;
procedure RegisterGraphicClass(AGraphicClass: TGraphicClass);
procedure UnRegisterGraphicClass(AGraphicClass: TGraphicClass);

implementation

{$R cxImage.res}

uses
  Math, cxEditUtils, dxCameraControl, dxCameraDialog, dxSmartImage;

type
  TdxCustomSmartImageAccess = class(TdxCustomSmartImage);
  TMemoryStreamAccess = class(TMemoryStream);
  TPictureAccess = class(TPicture);

  { TcxImageCommonResources }

  TcxImageCommonResources = class
  private
    FPopupMenuImageList: TCustomImageList;
  protected
    procedure InitializePopupMenuImageList;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property PopupMenuImageList: TCustomImageList read FPopupMenuImageList;
  end;

var
  cxRegisteredGraphicClasses: TList;
  FCommonResources: TcxImageCommonResources = nil;

function cxImageCommonResources: TcxImageCommonResources;
begin
  if FCommonResources = nil then
    FCommonResources := TcxImageCommonResources.Create;
  Result := FCommonResources;
end;

function GetGraphicClassByName(const AClassName: string): TGraphicClass;
var
  I: Integer;
begin
  Result := nil;
  for i := 0 to GetRegisteredGraphicClasses.Count - 1 do
    if InternalCompareString(AClassName, TClass(GetRegisteredGraphicClasses[I]).ClassName, False) then
    begin
      Result := TGraphicClass(GetRegisteredGraphicClasses[I]);
      Break;
    end;
end;

function GetRegisteredGraphicClasses: TList;
begin
  if cxRegisteredGraphicClasses = nil then
  begin
    cxRegisteredGraphicClasses := TList.Create;
    RegisterGraphicClass(TBitmap);
    RegisterGraphicClass(TIcon);
    RegisterGraphicClass(TMetaFile);
    if GetClass(TdxPNGImage.ClassName) <> nil then
      RegisterGraphicClass(TdxPNGImage);
    if GetClass(TdxSmartImage.ClassName) <> nil then
      RegisterGraphicClass(TdxSmartImage);
  end;
  Result := cxRegisteredGraphicClasses
end;

procedure RegisterGraphicClass(AGraphicClass: TGraphicClass);
begin
  if cxRegisteredGraphicClasses.IndexOf(TObject(AGraphicClass)) = -1 then
    cxRegisteredGraphicClasses.Add(TObject(AGraphicClass));
end;

procedure UnRegisterGraphicClass(AGraphicClass: TGraphicClass);
var
  I: Integer;
begin
  I := cxRegisteredGraphicClasses.IndexOf(TObject(AGraphicClass));
  if I <> -1 then
    cxRegisteredGraphicClasses.Delete(I);
end;

procedure LoadPicture(APicture: TPicture; AGraphicClass: TGraphicClass; const AValue: Variant);
{ Paradox graphic BLOB header - see DB.pas}
type
  TGraphicHeader = record
    Count: Word;                { Fixed at 1 }
    HType: Word;                { Fixed at $0100 }
    Size: Longint;              { Size not including header }
  end;
var
  AGraphic: TGraphic;
  AHeader: TGraphicHeader;
  ASize: Longint;
  AStream: TMemoryStream;
  AValueAsString: AnsiString;
begin
  if dxVarIsBlob(AValue) then
  begin
    AStream := TMemoryStream.Create;
    try
      AValueAsString := dxVariantToAnsiString(AValue);
      ASize := Length(AValueAsString);
      if ASize >= SizeOf(AHeader) then
      begin
        TMemoryStreamAccess(AStream).SetPointer(@AValueAsString[1], ASize);
        AStream.Position := 0;
        AStream.Read(AHeader, SizeOf(AHeader));
        if (AHeader.Count <> 1) or (AHeader.HType <> $0100) or (AHeader.Size <> ASize - SizeOf(AHeader)) then
          AStream.Position := 0;
      end;
      if AStream.Size > 0 then
      try
        if AGraphicClass <> nil then
        begin
          AGraphic := AGraphicClass.Create;
          try
            AGraphic.LoadFromStream(AStream);
            if not dxGraphicIsEquals(APicture.Graphic, AGraphic) then
            begin
              if AGraphic is TdxCustomSmartImage then
                TdxCustomSmartImageAccess(AGraphic).CheckIsImageDataValid;
              APicture.Graphic := AGraphic;
            end;
          finally
            AGraphic.Free;
          end;
        end
        else
          APicture.Bitmap.LoadFromStream(AStream);
      except
        APicture.Assign(nil);
      end
      else
        APicture.Assign(nil);
    finally
      AStream.Free;
    end;
  end
  else
    APicture.Assign(nil);
end;

procedure SavePicture(APicture: TPicture; var AValue: AnsiString);
var
  AStream: TMemoryStream;
begin
  if IsPictureAssigned(APicture) then
  begin
    AStream := TMemoryStream.Create;
    try
      APicture.Graphic.SaveToStream(AStream);
      AStream.Position := 0;
      SetLength(AValue, AStream.Size);
      AStream.ReadBuffer(AValue[1], AStream.Size);
    finally
      AStream.Free;
    end;
  end
  else
    AValue := '';
end;

procedure SavePicture(APicture: TPicture; var AValue: Variant); overload;
var
  AStream: TBytesStream;
  ASize: Int64;
  P: Pointer;
begin
  if IsPictureAssigned(APicture) then
  begin
    AStream := TBytesStream.Create;
    try
      APicture.Graphic.SaveToStream(AStream);
      ASize := AStream.Size;
      AValue := VarArrayCreate([0, ASize - 1], varByte);
      P := VarArrayLock(AValue);
      try
        Move(AStream.Bytes[0], P^, ASize);
      finally
        VarArrayUnlock(AValue);
      end;
    finally
      AStream.Free;
    end;
  end
  else
    AValue := Null;
end;

{ TcxImageCommonResources }

constructor TcxImageCommonResources.Create;
begin
  inherited Create;
  InitializePopupMenuImageList;
end;

destructor TcxImageCommonResources.Destroy;
begin
  FreeAndNil(FPopupMenuImageList);
  inherited Destroy;
end;

procedure TcxImageCommonResources.InitializePopupMenuImageList;

  function GetResourceName(APopupMenuItem: TcxPopupMenuItem): string;
  begin
    case APopupMenuItem of
      pmiCut:
        Result := 'CXMENUIMAGE_CUT';
      pmiCopy:
        Result := 'CXMENUIMAGE_COPY';
      pmiPaste:
        Result := 'CXMENUIMAGE_PASTE';
      pmiDelete:
        Result := 'CXMENUIMAGE_DELETE';
      pmiLoad:
        Result := 'CXMENUIMAGE_LOAD';
      pmiWebCam:
        Result := 'CXMENUIMAGE_WEBCAM';
      pmiSave:
        Result := 'CXMENUIMAGE_SAVE';
    else
      Result := '';
    end;
  end;

  procedure LoadBitmapFromResource(ABitmap: TBitmap; APopupMenuItem: TcxPopupMenuItem);
  begin
    ABitmap.LoadFromResourceName(HInstance, GetResourceName(APopupMenuItem));
  end;

var
  ABitmap: TBitmap;
  APopupMenuItem: TcxPopupMenuItem;
begin
  ABitmap := TBitmap.Create;
  try
    FPopupMenuImageList := TcxImageList.Create(nil);
    for APopupMenuItem := Low(TcxPopupMenuItem) to High(TcxPopupMenuItem) do
    begin
      if APopupMenuItem <> pmiCustom then
      begin
        LoadBitmapFromResource(ABitmap, APopupMenuItem);
        FPopupMenuImageList.AddMasked(ABitmap, clDefault);
      end;
    end;
  finally
    ABitmap.Free;
  end;
end;

{ TcxPopupMenuLayout }

constructor TcxPopupMenuLayout.Create(AImage: TcxCustomImage);
begin
  inherited Create;
  FImage := AImage;
  FMenuItems := [pmiCut, pmiCopy, pmiPaste, pmiDelete, pmiLoad, pmiWebCam, pmiSave];
end;

destructor TcxPopupMenuLayout.Destroy;
begin
  FreeAndNil(FCustomMenuItemGlyph);
  inherited Destroy;
end;

function TcxPopupMenuLayout.GetCustomMenuItemGlyph: TBitmap;
begin
  if FCustomMenuItemGlyph = nil then
    FCustomMenuItemGlyph := TBitmap.Create;
  Result := FCustomMenuItemGlyph;
end;

procedure TcxPopupMenuLayout.SetCustomMenuItemGlyph(Value: TBitmap);
begin
  if (Value = nil) then
  begin
    FCustomMenuItemGlyph.Free;
    FCustomMenuItemGlyph := nil;
  end
  else
    CustomMenuItemGlyph.Assign(Value);
end;

procedure TcxPopupMenuLayout.Assign(Source: TPersistent);
begin
  if Source is TcxPopupMenuLayout then
    with TcxPopupMenuLayout(Source) do
    begin
      Self.MenuItems := MenuItems;
      Self.CustomMenuItemCaption := CustomMenuItemCaption;
      Self.CustomMenuItemGlyph.Assign(CustomMenuItemGlyph);
    end
  else
    inherited Assign(Source);

end;

{ TcxCustomImageProperties }

constructor TcxCustomImageProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FCenter := True;
  FDefaultHeight := cxImageDefaultInplaceHeight;
  FGraphicTransparency := gtDefault;
  FShowFocusRect := True;
  FFitMode := ifmFit;
  FZoomTrackBarProperties := CreateZoomBarProperties;
  FPopupMenuLayout := TcxPopupMenuLayout.Create(nil);
  FGraphicClass := GetDefaultGraphicClass;
end;

destructor TcxCustomImageProperties.Destroy;
begin
  FreeAndNil(FPopupMenuLayout);
  FreeAndNil(FZoomTrackBarProperties);
  inherited Destroy;
end;

function TcxCustomImageProperties.GetGraphicClassName: string;
begin
  if FGraphicClass = nil then
    Result := ''
  else
    Result := FGraphicClass.ClassName;
end;

function TcxCustomImageProperties.IsGraphicClassNameStored: Boolean;
begin
  Result := GraphicClass <> GetDefaultGraphicClass;
end;

procedure TcxCustomImageProperties.ReadIsGraphicClassNameEmpty(Reader: TReader);
begin
  Reader.ReadBoolean;
  GraphicClassName := '';
end;

procedure TcxCustomImageProperties.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TcxCustomImageProperties.SetCenter(AValue: Boolean);
begin
  if FCenter <> AValue then
  begin
    FCenter := AValue;
    Changed;
  end;
end;

procedure TcxCustomImageProperties.SetGraphicClass(
  const Value: TGraphicClass);
begin
  if FGraphicClass <> Value then
  begin
    FGraphicClass := Value;
    Changed;
  end;
end;

procedure TcxCustomImageProperties.SetGraphicClassName(
  const Value: string);
var
  AGraphicClass: TGraphicClass;
begin
  if Value = '' then
    GraphicClass := nil
  else
  begin
    AGraphicClass := GetGraphicClassByName(Value);
    if (AGraphicClass = nil) and (Value <> '') then
      AGraphicClass := TdxSmartImage;
    if AGraphicClass <> nil then
      GraphicClass := AGraphicClass;
  end;
end;

procedure TcxCustomImageProperties.SetGraphicTransparency(
  Value: TcxImageTransparency);
begin
  if FGraphicTransparency <> Value then
  begin
    FGraphicTransparency := Value;
    FNeedUpdateImage := True;
    Changed;
  end;
end;

procedure TcxCustomImageProperties.SetPopupMenuLayout(
  Value: TcxPopupMenuLayout);
begin
  FPopupMenuLayout.Assign(Value);
end;

procedure TcxCustomImageProperties.SetProportional(AValue: Boolean);
begin
  if Proportional <> AValue then
    FitMode := CalculateFitMode(AValue, Stretch);
end;

procedure TcxCustomImageProperties.SetStretch(AValue: Boolean);
begin
  if Stretch <> AValue then
    FitMode := CalculateFitMode(Proportional, AValue);
end;

procedure TcxCustomImageProperties.SetFitMode(AValue: TcxImageFitMode);
begin
  if FitMode <> AValue then
  begin
    FFitMode := AValue;
    Changed;
  end;
end;

procedure TcxCustomImageProperties.SetZoomTrackBarProperties(AValue: TdxCustomZoomTrackBarProperties);
begin
  FZoomTrackBarProperties.Assign(AValue);
end;

procedure TcxCustomImageProperties.SetShowFocusRect(Value: Boolean);
begin
  if FShowFocusRect <> Value then
  begin
    FShowFocusRect := Value;
    Changed;
  end;
end;

function TcxCustomImageProperties.CalculateFitMode(AProportional, AStretch: Boolean): TcxImageFitMode;
begin
  if AProportional then
    if AStretch then
      Result := ifmProportionalStretch
    else
      Result := ifmFit
  else
    if AStretch then
      Result := ifmStretch
    else
      Result := ifmNormal;
end;

function TcxCustomImageProperties.GetProportional: Boolean;
begin
  Result := FitMode in [ifmProportionalStretch, ifmFit];
end;

function TcxCustomImageProperties.GetStretch: Boolean;
begin
  Result := FitMode in [ifmProportionalStretch, ifmStretch];
end;

procedure TcxCustomImageProperties.WriteIsGraphicClassNameEmpty(Writer: TWriter);
begin
  Writer.WriteBoolean(True);
end;

function TcxCustomImageProperties.CanValidate: Boolean;
begin
  Result := True;
end;

function TcxCustomImageProperties.CreateZoomBarProperties: TdxCustomZoomTrackBarProperties;
begin
  Result := TdxCustomZoomTrackBarProperties.Create(Self);
end;

procedure TcxCustomImageProperties.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('IsGraphicClassNameEmpty', ReadIsGraphicClassNameEmpty,
    WriteIsGraphicClassNameEmpty, GraphicClassName = '');
end;

procedure TcxCustomImageProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomImageProperties then
    with TcxCustomImageProperties(AProperties) do
    begin
      Self.Caption := Caption;
      Self.Center := Center;
      Self.CustomFilter := CustomFilter;
      Self.GraphicClass := GraphicClass;
      Self.GraphicTransparency := GraphicTransparency;
      Self.PopupMenuLayout := PopupMenuLayout;
      Self.ShowFocusRect := ShowFocusRect;
      Self.Proportional := Proportional;
      Self.Stretch := Stretch;
      Self.FitMode := FitMode;
      Self.ZoomTrackBarProperties := ZoomTrackBarProperties;
      Self.OnAssignPicture := OnAssignPicture;
      Self.OnCustomClick := OnCustomClick;
      Self.OnGetGraphicClass := OnGetGraphicClass;
    end;
end;

function TcxCustomImageProperties.IsDesigning: Boolean;
var
  AOwner: TPersistent;
begin
  AOwner := GetOwner;
  Result := (AOwner is TComponent) and
    (csDesigning in (AOwner as TComponent).ComponentState);
end;

function TcxCustomImageProperties.GetDefaultGraphicClass: TGraphicClass;
begin
  if GetRegisteredGraphicClasses.Count > 0 then
    Result := TGraphicClass(GetRegisteredGraphicClasses[0])
  else
    Result := nil;
end;

function TcxCustomImageProperties.GetRealStretch(const APictureSize, ABoundsSize: TSize): Boolean;
begin
  Result := Stretch or (Proportional and
    ((APictureSize.cy > ABoundsSize.cy) or (APictureSize.cx > ABoundsSize.cx)));
end;

class function TcxCustomImageProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxImageViewData;
end;

function TcxCustomImageProperties.IsScrollbarsNeeded(APicture: TPicture): Boolean;
begin
  Result := not Center and (FitMode = ifmNormal) and IsPictureAssigned(APicture);
end;

class function TcxCustomImageProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxImage;
end;

function TcxCustomImageProperties.GetDisplayText(const AEditValue: TcxEditValue;
  AFullText: Boolean = False; AIsInplace: Boolean = True): string;
begin
  if VarIsNull(AEditValue) then
    Result := ''
  else
    Result := Caption;
end;

function TcxCustomImageProperties.GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource;
begin
  Result := evsValue;
end;

function TcxCustomImageProperties.GetGraphicClass(AItem: TObject;
  ARecordIndex: Integer; APastingFromClipboard: Boolean = False): TGraphicClass;
begin
  Result := FGraphicClass;
  if Result = nil then
  begin
    if APastingFromClipboard then
      Result := TBitmap;
    if Assigned(FOnGetGraphicClass) then
      FOnGetGraphicClass(AItem, ARecordIndex, APastingFromClipboard, Result);
  end;
end;

function TcxCustomImageProperties.GetSpecialFeatures: TcxEditSpecialFeatures;
begin
  Result := inherited GetSpecialFeatures + [esfMultiRow, esfBlobEditValue];
end;

function TcxCustomImageProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := inherited GetSupportedOperations + [esoAutoHeight, esoEditing];
end;

class function TcxCustomImageProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxImageViewInfo;
end;

function TcxCustomImageProperties.IsResetEditClass: Boolean;
begin
  Result := True;
end;

procedure TcxCustomImageProperties.ValidateDisplayValue(var DisplayValue: TcxEditValue;
  var ErrorText: TCaption; var Error: Boolean; AEdit: TcxCustomEdit);
begin
  with TcxCustomImage(AEdit) do
  begin
    LockEditValueChanging(True);
    try
      DoOnAssignPicture;
      SaveModified;
      try
        EditModified := False;
        DoEditing;
      finally
        RestoreModified;
      end;
    finally
      LockEditValueChanging(False);
    end;
  end;
end;

{ TcxImageZoomingOptions }

constructor TcxImageZoomingOptions.Create;
begin
  FZoomPercent := 100;
  FMaxZoom := 500;
  FMinZoom := 0;
  FShowZoomTrackBar := False;
end;

procedure TcxImageZoomingOptions.Assign(Source: TPersistent);
var
  AImageZoomingOptions: TcxImageZoomingOptions;
begin
  if Source is TcxImageZoomingOptions then
  begin
    AImageZoomingOptions := TcxImageZoomingOptions(Source);
    MaxZoom := AImageZoomingOptions.MaxZoom;
    MinZoom := AImageZoomingOptions.MinZoom;
    ZoomPercent := AImageZoomingOptions.ZoomPercent;
    OnChanged := AImageZoomingOptions.OnChanged;
  end
  else
    inherited Assign(Source);
end;

procedure TcxImageZoomingOptions.Changed;
begin
  dxCallNotify(OnChanged, Self);
end;

procedure TcxImageZoomingOptions.SetMaxZoom(AValue: Integer);
begin
  AValue := Max(AValue, 100);
  if AValue <> FMaxZoom then
  begin
    FMaxZoom := AValue;
    ZoomPercent := Min(ZoomPercent, FMaxZoom);
    Changed;
  end;
end;

procedure TcxImageZoomingOptions.SetMinZoom(AValue: Integer);
begin
  AValue := Max(0, Min(AValue, 100));
  if AValue <> FMinZoom then
  begin
    FMinZoom := AValue;
    ZoomPercent := Max(ZoomPercent, FMinZoom);
    Changed;
  end;
end;

procedure TcxImageZoomingOptions.SetZoomPercent(AValue: Integer);
begin
  AValue := Min(MaxZoom, Max(0, Max(MinZoom, AValue)));
  if ZoomPercent <> AValue then
  begin
    FZoomPercent := AValue;
    Changed;
  end;
end;

procedure TcxImageZoomingOptions.SetShowZoomTrackBar(AValue: Boolean);
begin
  if AValue <> FShowZoomTrackBar then
  begin
    FShowZoomTrackBar := AValue;
    Changed;
  end;
end;

{ TcxImageAnimationOptions }

constructor TcxImageAnimationOptions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAnimation := bDefault;
  FAnimationLoop := bDefault;
end;

procedure TcxImageAnimationOptions.Assign(Source: TPersistent);
begin
  if Source is TcxImageAnimationOptions then
  begin
    FAnimation := TcxImageAnimationOptions(Source).Animation;
    FAnimationLoop := TcxImageAnimationOptions(Source).AnimationLoop;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TcxImageAnimationOptions.CheckGraphic;
var
  APrevGraphic: TObject;
begin
  if FGraphic <> Owner.Picture.Graphic then
  begin
    APrevGraphic := FGraphic;
    if Owner.Picture.Graphic is TdxGPImage then
      FGraphic := TdxGPImage(Owner.Picture.Graphic)
    else
      FGraphic := nil;
    if APrevGraphic <> FGraphic then
      Changed;
  end;
end;

procedure TcxImageAnimationOptions.Changed;
begin
  if Graphic = nil then Exit;
  Graphic.Animation := Animation <> bFalse;
  Graphic.AnimationLoop := AnimationLoop;
end;

function TcxImageAnimationOptions.GetGraphic: TdxGPImage;
begin
  CheckGraphic;
  Result := FGraphic;
end;

function TcxImageAnimationOptions.GetOwnerImage: TcxCustomImage;
begin
  Result := (inherited Owner) as TcxCustomImage;
end;

procedure TcxImageAnimationOptions.SetAnimation(AValue: TdxDefaultBoolean);
begin
  if FAnimation <> AValue then
  begin
    FAnimation := AValue;
    Changed;
  end;
end;

procedure TcxImageAnimationOptions.SetAnimationLoop(AValue: TdxDefaultBoolean);
begin
  if FAnimationLoop <> AValue then
  begin
    FAnimationLoop := AValue;
    Changed;
  end;
end;

{ TcxCustomImage }

constructor TcxCustomImage.Create(AOwner: TComponent);
begin
  inherited;
  FZoomTrackBarController := CreateZoomTrackBarController;
  FZoomingOptions := CreateZoomingOptions;
  FZoomingOptions.OnChanged := ZoomingOptionsChangedHandler;
  FAnimationOptions := CreateAnimationOptions;
  SetupZoomTrackBarProperties;
  DoubleBuffered := True;
end;

destructor TcxCustomImage.Destroy;
begin
  FreeAndNil(FAnimationOptions);
  FreeAndNil(FInternalPopupMenu);
  FreeAndNil(FPicture);
  FreeAndNil(FZoomingOptions);
  FreeAndNil(FZoomTrackBarController);
  inherited Destroy;
end;

function TcxCustomImage.AllowHybridScrollbarMode: Boolean;
begin
  Result := False;
end;

function TcxCustomImage.AllowScaling: Boolean;
begin
  Result := (ActiveProperties.FitMode = ifmNormal);
end;

function TcxCustomImage.CanZoomPercentByMouseWheel(AShift: TShiftState; const AMousePos: TPoint): Boolean;
begin
  Result := (ssCtrl in AShift);
end;

procedure TcxCustomImage.DecZoomPercent;
var
  APrevZoomPercent: Integer;
  AZoomPercent: Integer;
begin
  APrevZoomPercent := ZoomPercent;
  if ZoomPercent <= 100 then
    AZoomPercent := ZoomPercent - ActiveProperties.ZoomTrackBarProperties.FirstRange.LineSize
  else
    AZoomPercent := ZoomPercent - ActiveProperties.ZoomTrackBarProperties.SecondRange.LineSize;

  if (APrevZoomPercent > 100) and (AZoomPercent < 100) then
    ZoomingOptions.ZoomPercent := 100
  else
    ZoomingOptions.ZoomPercent := AZoomPercent;
end;

procedure TcxCustomImage.EditAndClear;
begin
  if DoEditing then
    FPicture.Graphic := nil;
end;

procedure TcxCustomImage.EditPopupMenuClick(Sender: TObject);
begin
  MenuItemClick(Sender, TcxPopupMenuItem(TMenuItem(Sender).Tag));
end;

function TcxCustomImage.GetProperties: TcxCustomImageProperties;
begin
  Result := TcxCustomImageProperties(inherited Properties);
end;

function TcxCustomImage.GetViewInfo: TcxImageViewInfo;
begin
  Result := TcxImageViewInfo(inherited ViewInfo);
end;

function TcxCustomImage.GetActiveProperties: TcxCustomImageProperties;
begin
  Result := TcxCustomImageProperties(InternalGetActiveProperties);
end;

procedure TcxCustomImage.IncZoomPercent;
var
  APrevZoomPercent: Integer;
  AZoomPercent: Integer;
begin
  APrevZoomPercent := ZoomPercent;
  if ZoomPercent < 100 then
    AZoomPercent := ZoomPercent + ActiveProperties.ZoomTrackBarProperties.FirstRange.LineSize
  else
    AZoomPercent := ZoomPercent + ActiveProperties.ZoomTrackBarProperties.SecondRange.LineSize;

  if (APrevZoomPercent < 100) and (AZoomPercent > 100) then
    ZoomingOptions.ZoomPercent := 100
  else
    ZoomingOptions.ZoomPercent := AZoomPercent;
end;

procedure TcxCustomImage.MenuItemClick(Sender: TObject; MenuItem: TcxPopupMenuItem);
begin
  BeginUserAction;
  try
    case MenuItem of
      pmiCut: CutToClipboard;
      pmiCopy: CopyToClipboard;
      pmiPaste: PasteFromClipboard;
      pmiDelete: EditAndClear;
      pmiLoad: LoadFromFile;
      pmiWebCam: AssignFromWebCam;
      pmiSave: SaveToFile;
      pmiCustom: CustomClick;
    end;
  finally
    EndUserAction;
  end;
end;

procedure TcxCustomImage.PictureChanged(Sender: TObject);
var
  APrevEvent: TNotifyEvent;
begin
  if IsLoading then
    Exit;
  LockChangeEvents(True);
  try
    if Picture.Graphic is TIcon then // Otherwise the Icon returns the incorrect sizes
      TIcon(Picture.Graphic).Handle; // HandleNeeded;

    if ActiveProperties.GraphicTransparency <> gtDefault then
    begin
      APrevEvent := Picture.OnChange;
      try
        Picture.OnChange := nil;
        if IsPictureAssigned(Picture) then
          Picture.Graphic.Transparent := ActiveProperties.GraphicTransparency = gtTransparent;
      finally
        Picture.OnChange := APrevEvent;
      end;
    end;

    SetSize;
    if IsUserAction then
      ModifiedAfterEnter := True
    else
      EditModified := False;

    DoChange;
    ShortRefreshContainer(False);

    if ActiveProperties.ImmediatePost and CanPostEditValue and InternalValidateEdit then
      InternalPostEditValue;
  finally
    LockChangeEvents(False, not IsAnimationNotification);
    AnimationOptions.CheckGraphic;
  end;
  CheckScrollbars(True);
  ActiveProperties.FNeedUpdateImage := False;
end;

procedure TcxCustomImage.PreparePopup;

  procedure RefreshCaptions;
  begin
    with FInternalPopupMenu do
    begin
      Items[0].Caption := cxGetResourceString(@cxSMenuItemCaptionCut);
      Items[1].Caption := cxGetResourceString(@cxSMenuItemCaptionCopy);
      Items[2].Caption := cxGetResourceString(@cxSMenuItemCaptionPaste);
      Items[3].Caption := cxGetResourceString(@cxSMenuItemCaptionDelete);
      Items[5].Caption := cxGetResourceString(@cxSMenuItemCaptionLoad);
      Items[6].Caption := cxGetResourceString(@cxSMenuItemCaptionAssignFromWebCam);
      Items[7].Caption := cxGetResourceString(@cxSMenuItemCaptionSave);
    end;
  end;

  function NewItem(const ACaption: string; ABitmap: TBitmap; ATag: TcxTag): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    with Result do
    begin
      Caption := ACaption;
      if Assigned(ABitmap) then
        Bitmap := ABitmap
      else
        ImageIndex := ATag;
      Tag := ATag;
      OnClick := EditPopupMenuClick;
    end;
  end;

  procedure AddItem(AItems: TMenuItem; AMenuItem: TcxPopupMenuItem);
  begin
    with AItems do
    begin
      if AMenuItem = pmiCustom then
      begin
        ActiveProperties.PopupMenuLayout.CustomMenuItemGlyph.Transparent := True;
        Add(NewItem(ActiveProperties.PopupMenuLayout.CustomMenuItemCaption,
          ActiveProperties.PopupMenuLayout.CustomMenuItemGlyph, TdxNativeInt(AMenuItem)));
      end
      else
        Add(NewItem('', nil, TdxNativeInt(AMenuItem)));
      if AMenuItem in [pmiDelete, pmiSave] then
        Add(NewItem('-', nil, -1));
    end;
  end;

var
  I: TcxPopupMenuItem;
  AFlagRO, AFlagEmpty, AIsIcon, ACanPaste: Boolean;
begin
  with ActiveProperties.PopupMenuLayout do
  begin
    if FInternalPopupMenu = nil then
    begin
      FInternalPopupMenu := TPopupMenu.Create(nil);
      FInternalPopupMenu.Images := cxImageCommonResources.PopupMenuImageList;
      for I := Low(TcxPopupMenuItem) to High(TcxPopupMenuItem) do
        AddItem(FInternalPopupMenu.Items, I);
    end;
    RefreshCaptions;
    // visible
    with FInternalPopupMenu do
    begin
      Items[0].Visible := pmiCut in MenuItems;
      Items[1].Visible := pmiCopy in MenuItems;
      Items[2].Visible := pmiPaste in MenuItems;
      Items[3].Visible := pmiDelete in MenuItems;
      Items[5].Visible := pmiLoad in MenuItems;
      Items[6].Visible := pmiWebCam in MenuItems;
      Items[7].Visible := pmiSave in MenuItems;
      Items[9].Visible := pmiCustom in MenuItems;
      // Separators
      Items[4].Visible := Items[5].Visible or Items[6].Visible;
      Items[8].Visible := Items[9].Visible;

      AIsIcon := ActiveProperties.GraphicClass = TIcon;

      ACanPaste := CanPasteFromClipboard;
      // Custom Item
      with Items[9] do
      begin
        Caption := CustomMenuItemCaption;
        Bitmap := CustomMenuItemGlyph;
      end;

      AFlagRO := not CanModify;
      AFlagEmpty := not IsPictureAssigned(FPicture);
      Items[0].Enabled := not (AFlagEmpty or AFlagRO or AIsIcon);
      Items[1].Enabled := not AFlagEmpty and not AIsIcon;
      Items[2].Enabled := not AFlagRO and ACanPaste;
      Items[3].Enabled := not AFlagEmpty and not AFlagRO;
      Items[5].Enabled := not AFlagRO;
      Items[6].Enabled := not AFlagRO and dxIsCameraAvailable;
      Items[7].Enabled := not AFlagEmpty;
    end;
  end;
end;

procedure TcxCustomImage.CheckZoomPercent;
var
  APrevZoomPercent: Integer;
  Acx, Acy: Extended;
begin
  if AllowScaling then
  begin
    APrevZoomPercent := ZoomPercent;
    ZoomPercent := ZoomingOptions.ZoomPercent;
    if APrevZoomPercent <> 0 then
    begin
      Acx := (GetLeftPos + GetClientSize.cx / 2) * FZoomPercent / APrevZoomPercent;
      Acy := (GetTopPos + GetClientSize.cy / 2) * FZoomPercent / APrevZoomPercent;
    end
    else
    begin
      Acx := 0;
      Acy := 0;
    end;

    TdxScrollHelper.SetPos(Self, Round(Acx - GetClientSize.cx / 2), Round(Acy - GetClientSize.cy / 2));
  end
  else
    ZoomPercent := 100;
end;

procedure TcxCustomImage.SetAnimationOptions(Value: TcxImageAnimationOptions);
begin
  FAnimationOptions.Assign(Value);
end;

procedure TcxCustomImage.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TcxCustomImage.SetProperties(const Value: TcxCustomImageProperties);
begin
  Properties.Assign(Value);
end;

function TcxCustomImage.CreateAnimationOptions: TcxImageAnimationOptions;
begin
  Result := TcxImageAnimationOptions.Create(Self);
end;

function TcxCustomImage.CreateZoomingOptions: TcxImageZoomingOptions;
begin
  Result := TcxImageZoomingOptions.Create;
end;

function TcxCustomImage.CreateZoomTrackBarController: TdxZoomTrackBarController;
begin
  Result := TdxZoomTrackBarController.Create(Self);
end;

function TcxCustomImage.AllowPan(AScrollKind: TScrollBarKind): Boolean;
begin
  if AScrollKind = sbHorizontal then
    Result := IsScrollBarActive(sbHorizontal)
  else
    Result := IsScrollBarActive(sbVertical);
end;

function TcxCustomImage.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

function TcxCustomImage.CanAutoSize: Boolean;
begin
  Result := inherited CanAutoSize and IsPictureAssigned(Picture);
end;

function TcxCustomImage.CanAutoWidth: Boolean;
begin
  Result := True;
end;

procedure TcxCustomImage.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
var
  P: TPoint;
begin
  inherited;
  if (PopupMenu = nil) and (ActiveProperties.PopupMenuLayout.MenuItems <> []) and not Handled then
  begin
    Handled := True;
    P := MousePos;
    if (P.X = -1) and (P.Y = -1) then
    begin
      P.X := 10;
      P.Y := 10;
    end;
    // Popup
    PreparePopup;
    P := ClientToScreen(P);
    FInternalPopupMenu.Popup(P.X, P.Y);
  end;
end;

function TcxCustomImage.GetDefaultPanOptions: Integer;
begin
  Result := inherited GetDefaultPanOptions and not GC_PAN_WITH_GUTTER;
end;

function TcxCustomImage.InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := CanZoomPercentByMouseWheel(Shift, MousePos);
  if Result then
  begin
    if Sign(WheelDelta) > 0 then
      IncZoomPercent
    else
      DecZoomPercent;
  end
  else
    Result := inherited InternalMouseWheel(Shift, WheelDelta, MousePos);
end;

function TcxCustomImage.IsAnimationNotification: Boolean;
begin
  Result := (AnimationOptions.FGraphic = Picture.Graphic) and (AnimationOptions.FGraphic <> nil) and
    (AnimationOptions.FGraphic.AnimationFrameCount > 0);
end;

function TcxCustomImage.IsDefaultGesture(AGestureID: Integer): Boolean;
begin
  Result := inherited IsDefaultGesture(AGestureID) or (AGestureID = GID_ZOOM);
end;

function TcxCustomImage.IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited IsMouseWheelHandleNeeded(Shift, WheelDelta, MousePos) or
    CanZoomPercentByMouseWheel(Shift, MousePos);
end;

function TcxCustomImage.IsPanArea(const APoint: TPoint): Boolean;
begin
  Result := inherited IsPanArea(APoint);
end;

procedure TcxCustomImage.AssignFromWebCam;
var
  APicture: TPicture;
begin
  APicture := TPicture.Create;
  try
    if dxShowCameraDialog(APicture) and DoEditing then
    begin
      Picture.Assign(APicture);
      DoClosePopup(crEnter);
    end
    else
      DoClosePopup(crCancel);
  finally
    APicture.Free;
  end;
end;

procedure TcxCustomImage.Initialize;
begin
  inherited Initialize;
  AutoSize := False;
  Width := 140;
  Height := 100;
  FClipboardFormat := CF_PICTURE;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  ViewInfo.Picture := FPicture;
end;

procedure TcxCustomImage.Centre;
begin
  if ActiveProperties.IsScrollbarsNeeded(Picture) then
  begin
    TdxScrollHelper.SetPos(Self,
      (GetContentSize.cx - cxSize(ClientBounds).cx) div 2,
      (GetContentSize.cy - cxSize(ClientBounds).cy) div 2);
  end;
end;

procedure TcxCustomImage.CheckScrollbars(AResetPosition: Boolean);
begin
  CheckZoomPercent;
  UpdateScrollBars;
  if IsScrollbarsNeeded then
  begin
    if AResetPosition then
      TdxScrollHelper.SetPos(Self, 0, 0)
    else
      TdxScrollHelper.CheckPositions(Self);
  end;
end;

procedure TcxCustomImage.DoScrollUIModeChanged;
begin
  UpdateScrollBars;
end;

procedure TcxCustomImage.InitScrollBarsParameters;
begin
  if HandleAllocated then
    CalculateViewInfo(False);
  if IsInplace or AutoSize or IsRectEmpty(ClientBounds) or // TODO
    not IsScrollBarsNeeded or ViewInfo.GetRealStretch then // TODO
      Exit;

  TdxScrollHelper.InitScrollBarsParameters(Self);
end;

procedure TcxCustomImage.Loaded;
begin
  inherited Loaded;
  LockChangeEvents(True);
  try
    PictureChanged(nil);
  finally
    LockChangeEvents(False, False);
  end;
end;

procedure TcxCustomImage.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  BeginUserAction;
  try
    case Key of
      VK_INSERT:
        if ssShift in Shift then
          PasteFromClipBoard
        else
          if ssCtrl in Shift then
            CopyToClipBoard;
      VK_DELETE:
        if ssShift in Shift then
          CutToClipBoard;
    end;
  finally
    EndUserAction;
  end;
end;

procedure TcxCustomImage.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  BeginUserAction;
  try
    case Key of
      ^X: CutToClipBoard;
      ^C: CopyToClipBoard;
      ^V: PasteFromClipBoard;
    end;
  finally
    EndUserAction;
  end;
end;

procedure TcxCustomImage.MouseEnter(AControl: TControl);
begin
  DoMouseEnter(Self);
  Invalidate;
end;

procedure TcxCustomImage.MouseLeave(AControl: TControl);
begin
  ViewInfo.ShowZoomTrackBar := False;
  if ViewInfo.ShowZoomTrackBar and (ViewInfo.ZoomTrackBarViewInfo.MouseStates <> []) then
    FZoomTrackBarController.MouseLeave;

  DoMouseLeave(Self);
  ShortRefreshContainer(True);
  Invalidate;
end;

procedure TcxCustomImage.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AZoomTrackBarBounds: TRect;
begin
  AZoomTrackBarBounds := ViewInfo.ZoomTrackBarViewInfo.Bounds;
  if (Button = mbLeft) and not IsScrollBarsArea(Point(X, Y)) and ViewInfo.ShowZoomTrackBar then
  begin
    if cxRectPtIn(AZoomTrackBarBounds, Point(X, Y)) then
    begin
      FZoomTrackBarController.MouseDown(Button, Shift, X - AZoomTrackBarBounds.Left, Y - AZoomTrackBarBounds.Top);
      FZoomTrackBarSliding := True;
    end
    else
      FLastDragPos := Point(X, Y);
  end
  else
    FLastDragPos := cxInvalidPoint;

  inherited;
end;

procedure TcxCustomImage.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AZoomTrackBarBounds: TRect;
  APrevShowZoomTrackBar: Boolean;
  dx, dy: Integer;
begin
  APrevShowZoomTrackBar := ViewInfo.ShowZoomTrackBar;
  ViewInfo.ShowZoomTrackBar := ZoomingOptions.ShowZoomTrackBar and AllowScaling and cxRectPtIn(Bounds, X, Y);
  AZoomTrackBarBounds := ViewInfo.ZoomTrackBarViewInfo.Bounds;
  if ViewInfo.ShowZoomTrackBar and (cxRectPtIn(AZoomTrackBarBounds, Point(X, Y)) or FZoomTrackBarSliding) then
  begin
    if ViewInfo.ZoomTrackBarViewInfo.MouseStates = [] then
      FZoomTrackBarController.MouseEnter
    else
      FZoomTrackBarController.MouseMove(Shift, X - AZoomTrackBarBounds.Left, Y - AZoomTrackBarBounds.Top);

    InvalidateRect(AZoomTrackBarBounds, True);
  end
  else
    if (ViewInfo.FZoomTrackBarViewInfo.MouseStates <> []) or (APrevShowZoomTrackBar <> ViewInfo.ShowZoomTrackBar) then
    begin
      FZoomTrackBarController.MouseLeave;
      InvalidateRect(AZoomTrackBarBounds, True);
    end;

  if (ssLeft in Shift) and not cxRectPtIn(AZoomTrackBarBounds, Point(X, Y)) and
    not cxPointIsInvalid(FLastDragPos) and ActiveProperties.IsScrollbarsNeeded(Picture) and
    not FZoomTrackBarSliding then
  begin
    dx := FLastDragPos.X - X;
    dy := FLastDragPos.Y - Y;
    TdxScrollHelper.SetPos(Self, GetLeftPos + dx, GetTopPos + dy);
    FLastDragPos := Point(X, Y);
  end;
  inherited;
end;

procedure TcxCustomImage.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AZoomTrackBarBounds: TRect;
begin
  AZoomTrackBarBounds := ViewInfo.FZoomTrackBarViewInfo.Bounds;
  FZoomTrackBarSliding := False;
  if (Button = mbLeft) and ViewInfo.ShowZoomTrackBar and not cxPointIsInvalid(FLastDragPos) and AllowScaling then
    FZoomTrackBarController.MouseUp(Button, Shift, X - AZoomTrackBarBounds.Left, Y - AZoomTrackBarBounds.Top);

  inherited;
end;

procedure TcxCustomImage.SetupZoomTrackBarProperties;
var
  AProperties: TdxCustomZoomTrackBarProperties;
begin
  AProperties := ActiveProperties.ZoomTrackBarProperties;
  AProperties.Max := ZoomingOptions.MaxZoom;
  AProperties.Min := ZoomingOptions.MinZoom;
  AProperties.FirstRange.LineSize := Ceil((100 - AProperties.Min) / 10);
  AProperties.SecondRange.LineSize := Ceil((AProperties.Max - 100) / 10);
end;

function TcxCustomImage.IsScrollbarsNeeded: Boolean;
begin
  Result := (ActiveProperties <> nil) and ActiveProperties.IsScrollbarsNeeded(Picture);
end;

function TcxCustomImage.NeedsScrollBars: Boolean;
begin
  Result := True;
end;

procedure TcxCustomImage.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  TdxScrollHelper.Scroll(Self, AScrollBarKind, AScrollCode, AScrollPos);
end;

function TcxCustomImage.GetEditValue: TcxEditValue;
begin
  SavePicture(FPicture, Result);
end;

procedure TcxCustomImage.InternalSetEditValue(const Value: TcxEditValue; AIsValueValid: Boolean);

  function InternalVarEquals(const V1, V2: Variant): Boolean;
  var
    P1, P2: Pointer;
  begin
    Result := VarType(V1) = VarType(V2);
    if Result then
    begin
      if VarType(V1) = varArray + varByte then
      begin
        Result := (VarArrayLowBound(V1, 1) = VarArrayLowBound(V2, 1)) and (VarArrayLowBound(V1, 1) = 0) and
          (VarArrayHighBound(V1, 1) = VarArrayHighBound(V2, 1));
        if Result then
        begin
          P1 := VarArrayLock(V1);
          P2 := VarArrayLock(V2);
          try
            Result := CompareMem(P1, P2, VarArrayHighBound(V1, 1) - 1);
          finally
            VarArrayUnlock(V2);
            VarArrayUnlock(V1);
          end;
        end;
      end
      else
        Result := VarEquals(V1, V2);
    end;
  end;

begin
  if not InternalVarEquals(EditValue, Value) then
  begin
    if dxVarIsBlob(Value) then
      LoadPicture(Picture, GetGraphicClass, Value)
    else
      Picture.Assign(nil);
  end;
end;

procedure TcxCustomImage.PropertiesChanged(Sender: TObject);
begin
  if not PropertiesChangeLocked and not IsLoading then
  begin
    if ActiveProperties.FNeedUpdateImage then
      PictureChanged(nil);

    CheckScrollbars;
    inherited;
  end;
end;

procedure TcxCustomImage.BoundsChanged;
begin
  inherited;
  CheckScrollbars;
end;

procedure TcxCustomImage.ZoomingOptionsChangedHandler(Sender: TObject);
begin
  SetupZoomTrackBarProperties;
  CheckScrollbars;
  ShortRefreshContainer(False);
end;

function TcxCustomImage.GetLeftPos: Integer;
begin
  Result := ViewInfo.TopLeft.X;
end;

procedure TcxCustomImage.SetLeftPos(Value: Integer);
begin
  ViewInfo.TopLeft.X := Value;
end;

function TcxCustomImage.GetTopPos: Integer;
begin
  Result := ViewInfo.TopLeft.Y;
end;

procedure TcxCustomImage.SetTopPos(Value: Integer);
begin
  ViewInfo.TopLeft.Y := Value;
end;

function TcxCustomImage.GetContentSize: TSize;
var
  AImageSize: TSize;
  AImageRect: TRect;
begin
  AImageSize := dxGetImageSize(Picture, ScaleFactor);
  AImageRect := cxGetImageRect(ClientBounds, AImageSize, ActiveProperties.FitMode, ActiveProperties.Center, ZoomPercent);
  Result := cxSize(AImageRect);
end;

function TcxCustomImage.GetClientSize: TSize;
begin
  Result := TdxScrollHelper.GetClientSize(Self);
end;

function TcxCustomImage.GetInstance: TcxControl;
begin
  Result := Self;
end;

procedure TcxCustomImage.Zoom(ADelta: Integer; var AHandled: Boolean);
begin
  if AllowScaling then
    ZoomingOptions.ZoomPercent := ZoomingOptions.ZoomPercent + ADelta div 4;
  AHandled := True;
end;

function TcxCustomImage.GetZoomPercent: Integer;
begin
  Result := FZoomPercent;
end;

function TcxCustomImage.GetZoomTrackBarActiveProperties: TcxCustomTrackBarProperties;
begin
  Result := ActiveProperties.ZoomTrackBarProperties;
end;

function TcxCustomImage.GetZoomTrackBarMouseDownPos: TPoint;
begin
  Result := cxPointOffset(MouseDownPos, ViewInfo.FZoomTrackBarViewInfo.Bounds.TopLeft, False);
end;

function TcxCustomImage.GetZoomTrackBarProperties: TcxCustomTrackBarProperties;
begin
  Result := Properties.ZoomTrackBarProperties;
end;

function TcxCustomImage.GetZoomTrackBarViewInfo: TcxCustomTrackBarViewInfo;
begin
  Result := ViewInfo.FZoomTrackBarViewInfo;
end;

function TcxCustomImage.IsZoomTrackBarInplace: Boolean;
begin
  Result := True;
end;

procedure TcxCustomImage.InternalSetZoomPercent(AValue: Integer);
begin
  ZoomingOptions.ZoomPercent := AValue;
end;

function TcxCustomImage.CanPasteFromClipboard: Boolean;
var
  AGraphicClass: TGraphicClass;
begin
  AGraphicClass := ActiveProperties.GraphicClass;
  if AGraphicClass = TBitmap then
    Result := Clipboard.HasFormat(CF_BITMAP)
  else if AGraphicClass = TIcon then
    Result := False
  else if AGraphicClass = TMetafile then
    Result := Clipboard.HasFormat(CF_METAFILEPICT)
  else if AGraphicClass = nil then
    Result := Clipboard.HasFormat(CF_PICTURE)
  else
    Result := Clipboard.HasFormat(ClipboardFormat);
end;

procedure TcxCustomImage.CustomClick;
begin
  with Properties do
    if Assigned(OnCustomClick) then
      OnCustomClick(Self);
  if RepositoryItem <> nil then
    with ActiveProperties do
      if Assigned(OnCustomClick) then
        OnCustomClick(Self);
end;

procedure TcxCustomImage.DoOnAssignPicture;
begin
  with Properties do
    if Assigned(OnAssignPicture) then
      OnAssignPicture(Self, Picture);
  if RepositoryItem <> nil then
    with ActiveProperties do
      if Assigned(OnAssignPicture) then
        OnAssignPicture(Self, Picture);
end;

procedure TcxCustomImage.DoPictureLoadedFromFile(AFileName: string);
begin
end;

function TcxCustomImage.GetGraphicClass(APastingFromClipboard: Boolean = False): TGraphicClass;
begin
  if IsInplace then
    Result := ActiveProperties.GetGraphicClass(InplaceParams.Position.Item,
      InplaceParams.Position.RecordIndex, APastingFromClipboard)
  else
  begin
    Result := ActiveProperties.GraphicClass;
    if Result = nil then
    begin
      if APastingFromClipboard then
        Result := TBitmap;
      if Assigned(FOnGetGraphicClass) then
        FOnGetGraphicClass(Self, APastingFromClipboard, Result);
    end;
  end;
end;

procedure TcxCustomImage.CopyToClipboard;
begin
  if (FPicture <> nil) and (FPicture.Graphic <> nil) then
    Clipboard.Assign(FPicture);
end;

procedure TcxCustomImage.CutToClipboard;
begin
  CopyToClipboard;
  EditAndClear;
end;

function TcxCustomImage.Focused: Boolean;
begin
  Result := FIsDialogShowed or inherited Focused;
end;

class function TcxCustomImage.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomImageProperties;
end;

procedure TcxCustomImage.LoadFromFile;

  function GetDialogFilter: string;
  var
    AGraphicClass: TGraphicClass;
  begin
    if ActiveProperties.CustomFilter <> '' then
      Result := ActiveProperties.CustomFilter
    else
    begin
      AGraphicClass := ActiveProperties.GraphicClass;
      if AGraphicClass = nil then
        AGraphicClass := TGraphic;
      Result := cxGraphicFilter(AGraphicClass);
    end;
  end;

var
  ADialog: TOpenPictureDialog;
begin
  if not CanModify then
    Exit;
  ADialog := TOpenPictureDialog.Create(nil);
  try
    FIsDialogShowed := True;
    ADialog.Filter := GetDialogFilter;
    if ADialog.Execute and DoEditing then
    begin
      FPicture.LoadFromFile(ADialog.FileName);
      DoClosePopup(crEnter);
      DoPictureLoadedFromFile(ADialog.FileName);
    end
    else
      DoClosePopup(crCancel);
    Application.ProcessMessages;
  finally
    FIsDialogShowed := False;
    ADialog.Free;
  end;
end;

procedure TcxCustomImage.PasteFromClipboard;
var
  AGraphic: TdxSmartImage;
  AGraphicClass: TGraphicClass;
  APrevEvent: TNotifyEvent;
begin
  if CanPasteFromClipboard and DoEditing then
  begin
    APrevEvent := Picture.OnChange;
    try
      Picture.OnChange := nil;
      if Clipboard.HasFormat(CF_BITMAP) then
      begin
        AGraphicClass := GetGraphicClass(True);
        if (AGraphicClass <> nil) and AGraphicClass.InheritsFrom(TdxSmartImage) then
        begin
          AGraphic := AGraphicClass.Create as TdxSmartImage;
          try
            AGraphic.PasteFromClipboard;
            Picture.Graphic := AGraphic;
          finally
            AGraphic.Free;
          end;
        end
        else
          Picture.Bitmap.Assign(Clipboard);
      end
      else
        Picture.Assign(Clipboard);
    finally
      Picture.OnChange := APrevEvent;
    end;
    TPictureAccess(Picture).Changed(Picture);
  end;
end;

procedure TcxCustomImage.SaveToFile;
var
  ADialog: TSavePictureDialog;
begin
  if (FPicture = nil) or (FPicture.Graphic = nil) then
    Exit;
  ADialog := TSavePictureDialog.Create(Application);
  FIsDialogShowed := True;
  try
    if ActiveProperties.CustomFilter <> '' then
      ADialog.Filter := ActiveProperties.CustomFilter
    else
      ADialog.Filter := cxGraphicFilter(FPicture.Graphic, True);

    ADialog.DefaultExt := cxGraphicExtension(FPicture.Graphic);
    if ADialog.Execute then
      FPicture.SaveToFile(ADialog.FileName);
    Application.ProcessMessages;
  finally
    FIsDialogShowed := False;
    ADialog.Free;
  end;
end;

{ TcxImage }

class function TcxImage.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxImageProperties;
end;

function TcxImage.GetActiveProperties: TcxImageProperties;
begin
  Result := TcxImageProperties(InternalGetActiveProperties);
end;

function TcxImage.GetProperties: TcxImageProperties;
begin
  Result := TcxImageProperties(inherited Properties);
end;

procedure TcxImage.SetProperties(Value: TcxImageProperties);
begin
  Properties.Assign(Value);
end;

{ TcxImageViewInfo }

constructor TcxImageViewInfo.Create;
begin
  inherited;
  FZoomTrackBarViewInfo := CreateZoomTrackBarViewInfo;
end;

destructor TcxImageViewInfo.Destroy;
begin
  if FFreePicture then
    FreeAndNil(Picture);

  FreeAndNil(FZoomTrackBarViewInfo);
  inherited Destroy;
end;

function TcxImageViewInfo.CreateZoomTrackBarViewInfo: TdxCustomZoomTrackBarViewInfo;
begin
  Result := TdxCustomZoomTrackBarViewInfo.Create;
end;

procedure TcxImageViewInfo.DrawTransparentBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(R);
    cxDrawTransparentControlBackground(Edit, ACanvas, Bounds);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TcxImageViewInfo.InternalPaint(ACanvas: TcxCanvas);
var
  CR, R: TRect;
begin
  if Transparent and not IsInplace then
    DrawTransparentBackground(ACanvas, Bounds);

  CR := ClientRect;
  inherited InternalPaint(ACanvas);
  if ShowFocusRect then
  begin
    ACanvas.FrameRect(CR, clBlack);
    InflateRect(CR, -1, -1);
  end;

  if not IsPictureAssigned(Picture) then
  begin
    inherited InternalPaint(ACanvas);
    ACanvas.Font := Font;
    ACanvas.Font.Color := TextColor;
    cxDrawText(ACanvas, Caption, CR, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end
  else
  begin
    R := cxGetImageRect(CR, dxGetImageSize(Picture, ScaleFactor), FitMode, Center, ZoomPercent);
    if FUseRightToLeftScrollBar then
    begin
      if not Center then
        R := cxRectOffsetHorz(R, CR.Width - R.Width);
      R := cxRectOffset(R, TopLeft.X, -TopLeft.Y);
    end
    else
      R := cxRectOffset(R, TopLeft, False);

    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(CR);
      ACanvas.StretchDraw(R, Picture.Graphic);
      if ShowZoomTrackBar and not IsDesigning then
        FZoomTrackBarViewInfo.Paint(ACanvas);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

function TcxImageViewInfo.IsRepaintOnStateChangingNeeded: Boolean;
begin
  Result := not IsPictureAssigned(Picture) and (Caption <> '');
end;

function TcxImageViewInfo.GetRealStretch: Boolean;
begin
  Result := TcxCustomImageProperties(EditProperties).GetRealStretch(
    Size(Picture.Width, Picture.Height),
    Size(cxRectWidth(ClientRect), cxRectHeight(ClientRect)));
end;

{ TcxImageViewData }

procedure TcxImageViewData.Calculate(ACanvas: TcxCanvas;
  const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
  Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
var
  AProperties: TcxCustomImageProperties;
  AImageViewInfo: TcxImageViewInfo;
begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo, AIsMouseEvent);
  if IsRectEmpty(ABounds) then
    Exit;

  AProperties := TcxCustomImageProperties(Properties);
  AImageViewInfo := TcxImageViewInfo(AViewInfo);
  AImageViewInfo.Caption := AProperties.Caption;
  AImageViewInfo.Center := AProperties.Center;
  AImageViewInfo.ShowFocusRect := AProperties.ShowFocusRect and Focused and not IsInplace;
  AImageViewInfo.FitMode := AProperties.FitMode;
  AImageViewInfo.Scalable := (AImageViewInfo.Edit <> nil) and TcxCustomImage(AImageViewInfo.Edit).AllowScaling;
  AImageViewInfo.FUseRightToLeftScrollBar := UseRightToLeftScrollBar;

  if AImageViewInfo.Scalable then
  begin
    if not IsDesigning then
      CalculateZoomTrackBar(ACanvas, AViewInfo.ClientRect, P, Button, Shift, AImageViewInfo.FZoomTrackBarViewInfo, AIsMouseEvent, AImageViewInfo.ZoomPercent);

    AImageViewInfo.ZoomPercent := TcxCustomImage(AImageViewInfo.Edit).ZoomPercent;
  end
  else
    AImageViewInfo.ZoomPercent := 100;

  if AImageViewInfo.Center or AProperties.Stretch then
    AImageViewInfo.TopLeft := Point(0, 0);
end;

procedure TcxImageViewData.EditValueToDrawValue(
  const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
var
  AGraphicClass: TGraphicClass;
begin
  with TcxImageViewInfo(AViewInfo) do
    if Length(dxVariantToAnsiString(AEditValue)) > 0 then
    begin
      if not Assigned(Picture) then
      begin
        Picture := TPicture.Create;
        FFreePicture := True;
      end;
      AGraphicClass := TcxCustomImageProperties(Properties).GetGraphicClass(
        InplaceEditParams.Position.Item, InplaceEditParams.Position.RecordIndex);
      LoadPicture(Picture, AGraphicClass, AEditValue);
      if TcxCustomImageProperties(Properties).GraphicTransparency <> gtDefault then
        Picture.Graphic.Transparent :=
          TcxCustomImageProperties(Properties).GraphicTransparency = gtTransparent;
    end
    else
      if Assigned(Picture) then
        Picture.Assign(nil);
end;

function TcxImageViewData.GetEditContentSize(ACanvas: TcxCanvas;
  const AEditValue: TcxEditValue;
  const AEditSizeProperties: TcxEditSizeProperties; AErrorData: TcxEditValidateInfo): TSize;
var
  ABorderExtent: TRect;
  AGraphicClass: TGraphicClass;
  APicture: TPicture;
begin
  if IsInplace then
  begin
    if Edit <> nil then
    begin
      Result := Size(Edit.Width, Edit.Height);
      ABorderExtent := GetBorderExtent;
      Result.cx := Result.cx - (ABorderExtent.Left + ABorderExtent.Right);
      Result.cy := Result.cy - (ABorderExtent.Top + ABorderExtent.Bottom);
    end
    else
      with TcxCustomImageProperties(Properties) do
      begin
        Result := Size(0, DefaultHeight);
        if dxVarIsBlob(AEditValue) then
        begin
          AGraphicClass := GetGraphicClass(InplaceEditParams.Position.Item, InplaceEditParams.Position.RecordIndex);
          APicture := TPicture.Create;
          try
            LoadPicture(APicture, AGraphicClass, AEditValue);
            Result := dxGetImageSize(APicture, ScaleFactor);
          finally
            APicture.Free;
          end;
          if GetRealStretch(Result, Size(AEditSizeProperties.Width, AEditSizeProperties.Height)) then
          begin
            if (AEditSizeProperties.Width > 0) and (Result.cx > 0) and
                ((AEditSizeProperties.Height > 0) or (AEditSizeProperties.Width < Result.cx)) then
              Result := Size(AEditSizeProperties.Width, Round(Result.cy * AEditSizeProperties.Width / Result.cx))
            else
              if (AEditSizeProperties.Height > 0) and (Result.cy > 0) and
                  ((AEditSizeProperties.Width > 0) or (AEditSizeProperties.Height < Result.cy)) then
                Result := Size(Round(Result.cx * AEditSizeProperties.Height / Result.cy), AEditSizeProperties.Height);
          end;
        end
        else
          if Length(Caption) <> 0 then
          begin
            ACanvas.Font := Style.GetVisibleFont;
            Result := ACanvas.TextExtent(Caption);
          end;
      end;
  end
  else
    if Edit <> nil then
      Result := dxGetImageSize(TcxCustomImage(Edit).Picture, ScaleFactor)
    else
      Result := Size(0, 0);
end;

procedure TcxImageViewData.CalculateZoomTrackBar(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;  Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean; AZoomPercent: Integer);
var
  ARect: TRect;
  AZoomTrackBarViewData: TdxCustomZoomTrackBarViewData;
begin
  ARect := cxRectCenterHorizontally(ABounds, Min(ScaleFactor.Apply(200), (cxRectWidth(ABounds))));
  ARect := cxRectSetHeight(ARect, ScaleFactor.Apply(30));
  ARect := cxRectSetTop(ARect, Min(cxRectHeight(ABounds) - ScaleFactor.Apply(40), Round(7 / 8 * cxRectHeight(ABounds))));
  TdxCustomZoomTrackBarViewInfo(AViewInfo).Position := AZoomPercent;

  AZoomTrackBarViewData := TdxCustomZoomTrackBarViewData.Create(
    TcxCustomImageProperties(Properties).ZoomTrackBarProperties, Edit.Style, True);
  try
    AZoomTrackBarViewData.ScaleFactor.Assign(ScaleFactor);
    AViewInfo.Transparent := True;
    AZoomTrackBarViewData.UseRightToLeftAlignment := UseRightToLeftAlignment;
    AZoomTrackBarViewData.Calculate(ACanvas, cxRectSetNullOrigin(ARect), P, Button, Shift, AViewInfo, AIsMouseEvent);
    AViewInfo.Bounds := ARect;
  finally
    AZoomTrackBarViewData.Free;
  end;
end;

initialization
  GetRegisteredEditProperties.Register(TcxImageProperties, scxSEditRepositoryImageItem);

finalization
  FreeAndNil(cxRegisteredGraphicClasses);
  FreeAndNil(FCommonResources);
end.

