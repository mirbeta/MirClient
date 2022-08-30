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

unit cxBlobEdit;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, ExtDlgs, Types, SysUtils, Classes, Graphics, Controls,
  Forms, ExtCtrls, Dialogs, StdCtrls, ImgList, Clipbrd,
  cxClasses, cxControls, cxContainer, cxGraphics, cxDataStorage, cxDataUtils,
  cxEdit, cxDropDownEdit, cxEditConsts, cxTextEdit, cxButtons, cxImage, cxMemo,
  cxFilterControlUtils, dxUxTheme, dxGDIPlusClasses;

type
  TcxBlobPaintStyle = (bpsDefault, bpsIcon, bpsText);
  TcxBlobEditKind = (bekAuto, bekMemo, bekPict, bekOle, bekBlob);

  TcxCustomBlobEdit = class;

  { TcxBlobEditViewData }

  TcxBlobEditViewData = class(TcxCustomDropDownEditViewData)
  protected
    function InternalGetEditConstantPartSize(ACanvas: TcxCanvas; AIsInplace: Boolean;
      AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: TSize;
      AViewInfo: TcxCustomEditViewInfo): TSize; override;
    function InternalEditValueToDisplayText(AEditValue: TcxEditValue): string; override;
    procedure PrepareDrawTextFlags(ACanvas: TcxCanvas; AViewInfo: TcxCustomEditViewInfo); override;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState;
      AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); override;
    procedure EditValueToDrawValue(const AEditValue: TcxEditValue;
      AViewInfo: TcxCustomEditViewInfo); override;
  end;

  { TcxBlobEditViewInfo }

  TcxBlobEditViewInfo = class(TcxCustomTextEditViewInfo)
  protected
    function GetBackgroundPaintingStyle: TcxEditBackgroundPaintingStyle; override;
    procedure InternalPaint(ACanvas: TcxCanvas); override;
  public
    IconRect: TRect;
    ImageIndex: TcxImageIndex;
    function NeedShowHint(ACanvas: TcxCanvas;
      const P: TPoint; const AVisibleBounds: TRect; out AText: TCaption;
      out AIsMultiLine: Boolean; out ATextRect: TRect; AMaxLineCount: Integer = 0): Boolean; override;
    procedure Offset(DX, DY: Integer); override;
  end;

  { TcxCustomBlobEditProperties }

  TcxCustomBlobEditProperties = class(TcxCustomPopupEditProperties)
  private
    // common
    FAlwaysSaveData: Boolean;
    FApplyStyleToButtons: Boolean;
    FBlobEditKind: TcxBlobEditKind;
    FBlobPaintStyle: TcxBlobPaintStyle;
    // memo
    FMemoAutoReplace: Boolean;
    FMemoWantReturns: Boolean;
    FMemoWantTabs: Boolean;
    FMemoOEMConvert: Boolean;
    FMemoWordWrap: Boolean;
    FMemoMaxLength: Integer;
    FMemoCharCase: TEditCharCase;
    FMemoScrollBars: TcxScrollStyle;
    // picture
    FPictureAutoSize: Boolean;
    FPictureClipboardFormat: Word;
    FPictureFilter: string;
    FPictureGraphicClass: TGraphicClass;
    FPictureTransparency: TcxImageTransparency;
    FShowExPopupItems: Boolean;
    FShowPicturePopup: Boolean;

    FOnAssignPicture: TcxImageAssignPictureEvent;
    FOnGetGraphicClass: TcxImageGraphicClassEvent;
    function GetPictureGraphicClassName: string;
    function IsPictureGraphicClassNameStored: Boolean;
    procedure ReadIsPictureGraphicClassNameEmpty(Reader: TReader);
    procedure SetBlobEditKind(const Value: TcxBlobEditKind);
    procedure SetBlobPaintStyle(const Value: TcxBlobPaintStyle);
    procedure SetPictureGraphicClass(Value: TGraphicClass);
    procedure SetPictureGraphicClassName(const Value: string);
    procedure WriteIsPictureGraphicClassNameEmpty(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function CanValidate: Boolean; override;
    function DefaultButtonKind: TcxEditButtonKind; override;
    function DropDownButtonVisibleIndex: Integer; override;
    function DropDownOnClick: Boolean; override;
    function GetDisplayFormatOptions: TcxEditDisplayFormatOptions; override;
    function HasDisplayValue: Boolean; override;
    class function GetPopupWindowClass: TcxCustomEditPopupWindowClass; override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;

    procedure CorrectBlobEditKind;
    function GetDefaultPictureGraphicClass: TGraphicClass; virtual;
    function GetPictureGraphicClass(AItem: TObject; ARecordIndex: Integer;
      APastingFromClipboard: Boolean = False): TGraphicClass;
    //
    property ApplyStyleToButtons: Boolean read FApplyStyleToButtons write FApplyStyleToButtons;
    property PictureClipboardFormat: Word read FPictureClipboardFormat write FPictureClipboardFormat;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    function CompareDisplayValues(const AEditValue1, AEditValue2: TcxEditValue): Boolean; override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetDisplayText(const AEditValue: TcxEditValue; AFullText: Boolean = False; AIsInplace: Boolean = True): string; override;
    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; override;
    function GetSpecialFeatures: TcxEditSpecialFeatures; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function IsEditValueValid(var AEditValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;

    property PictureGraphicClass: TGraphicClass read FPictureGraphicClass write SetPictureGraphicClass;
    // !!!
    // Common
    property AlwaysSaveData: Boolean read FAlwaysSaveData write FAlwaysSaveData default True;
    property BlobEditKind: TcxBlobEditKind read FBlobEditKind write SetBlobEditKind default bekAuto;
    property BlobPaintStyle: TcxBlobPaintStyle read FBlobPaintStyle write SetBlobPaintStyle default bpsIcon;
    property ImmediateDropDownWhenActivated default True;
    // Memo
    property MemoAutoReplace: Boolean read FMemoAutoReplace write FMemoAutoReplace default False;
    property MemoCharCase: TEditCharCase read FMemoCharCase write FMemoCharCase default ecNormal;
    property MemoMaxLength: Integer read FMemoMaxLength write FMemoMaxLength default 0;
    property MemoOEMConvert: Boolean read FMemoOEMConvert write FMemoOEMConvert default False;
    property MemoScrollBars: TcxScrollStyle read FMemoScrollBars write FMemoScrollBars default ssNone;
    property MemoWantReturns: Boolean read FMemoWantReturns write FMemoWantReturns default True;
    property MemoWantTabs: Boolean read FMemoWantTabs write FMemoWantTabs default True;
    property MemoWordWrap: Boolean read FMemoWordWrap write FMemoWordWrap default True;
    // Picture
    property PictureAutoSize: Boolean read FPictureAutoSize write FPictureAutoSize default True;
    property PictureFilter: string read FPictureFilter write FPictureFilter;
    property PictureGraphicClassName: string read GetPictureGraphicClassName write SetPictureGraphicClassName stored IsPictureGraphicClassNameStored;
    property PictureTransparency: TcxImageTransparency read FPictureTransparency write FPictureTransparency default gtDefault;
    property ShowExPopupItems: Boolean read FShowExPopupItems write FShowExPopupItems default True;
    property ShowPicturePopup: Boolean read FShowPicturePopup write FShowPicturePopup default True;
    property OnAssignPicture: TcxImageAssignPictureEvent read FOnAssignPicture write FOnAssignPicture;
    property OnGetGraphicClass: TcxImageGraphicClassEvent read FOnGetGraphicClass write FOnGetGraphicClass;
  end;

  { TcxBlobEditProperties }

  TcxBlobEditProperties = class(TcxCustomBlobEditProperties)
  published
    property AlwaysSaveData;
    property AssignedValues;
    property BlobEditKind;
    property BlobPaintStyle;
    property ClearKey;
    property ImeMode;
    property ImeName;
    property ImmediateDropDownWhenActivated;
    property ImmediateDropDownWhenKeyPressed;
    property ImmediatePost;
    property MemoAutoReplace;
    property MemoCharCase;
    property MemoMaxLength;
    property MemoOEMConvert;
    property MemoScrollBars;
    property MemoWantReturns;
    property MemoWantTabs;
    property MemoWordWrap;
    property PictureAutoSize;
    property PictureFilter;
    property PictureGraphicClassName;
    property PictureTransparency;
    property PopupHeight;
    property PopupWidth;
    property ReadOnly;
    property ShowExPopupItems;
    property ShowPicturePopup;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property OnAssignPicture;
    property OnButtonClick;
    property OnChange;
    property OnCloseQuery;
    property OnCloseUp;
    property OnEditValueChanged;
    property OnGetGraphicClass;
    property OnInitPopup;
    property OnPopup;
    property OnValidate;
  end;

  { TcxPopupMemo }

  TcxPopupMemo = class(TcxMemo)
  private
    FBlobEdit: TcxCustomBlobEdit;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SpellCheckerSetValue(const AValue: Variant); override;
  end;

  { TcxPopupImage }

  TcxPopupImage = class(TcxImage)
  private
    FBlobEdit: TcxCustomBlobEdit;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

  { TcxCustomBlobEdit }

  TcxCustomBlobEdit = class(TcxCustomPopupEdit)
  private
    FButtonWidth: Integer;
    FCancelButton: TcxButton;
    FOkButton: TcxButton;
    FStorage: TcxCustomEdit;
    FOnGetGraphicClass: TcxImageEditGraphicClassEvent;
    procedure DoPopupImageGetGraphicClass(Sender: TObject;
      APastingFromClipboard: Boolean; var AGraphicClass: TGraphicClass);
    procedure DoPopupImagePropertiesGetGraphicClass(AItem: TObject;
      ARecordIndex: Integer; APastingFromClipboard: Boolean;
      var AGraphicClass: TGraphicClass);
    function GetActiveProperties: TcxCustomBlobEditProperties;
    function GetProperties: TcxCustomBlobEditProperties;
    function GetStorageMemo: TcxPopupMemo;
    function GetStorageImage: TcxPopupImage;
    procedure InternalChanged(Sender: TObject);
    procedure PictureClosePopup(Sender: TcxControl; AReason: TcxEditCloseUpReason);
    procedure SaveStorage;
    procedure SetProperties(Value: TcxCustomBlobEditProperties);
  protected
    procedure ColorChanged; override;
    procedure DoInitPopup; override;
    procedure DoFinalizePopup; override;
    procedure FontChanged; override;
    function GetDisplayText: string; override;
    function GetEditingValue: TcxEditValue; override;
    function GetPopupFocusedControl: TWinControl; override;
    function GetPopupWindowClientPreferredSize: TSize; override;
    procedure Initialize; override;
    function InternalGetText: string; override;
    function InternalSetText(const Value: string): Boolean; override;
    procedure InternalValidateDisplayValue(const ADisplayValue: TcxEditValue); override;
    procedure PopupWindowClosed(Sender: TObject); override;
    procedure PropertiesChanged(Sender: TObject); override;
    procedure SetEditingText(const Value: TCaption); override;
    procedure SetupPopupWindow; override;

    procedure SynchronizeDisplayValue; override;
    procedure SynchronizeEditValue; override;

    procedure CreatePopupControls; virtual;
    procedure DestroyPopupControls; virtual;
    procedure DoAssignPicture;
    procedure DoOnAssignPicture(APicture: TPicture);
    function GetPictureGraphicClass(APastingFromClipboard: Boolean = False): TGraphicClass;
    procedure StorageEditingHandler(Sender: TObject; var CanEdit: Boolean); virtual;

    property StorageMemo: TcxPopupMemo read GetStorageMemo;
    property StorageImage: TcxPopupImage read GetStorageImage;
    property TabStop default True;
    property OnGetGraphicClass: TcxImageEditGraphicClassEvent read FOnGetGraphicClass write FOnGetGraphicClass;
  public
    destructor Destroy; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function IsEditClass: Boolean; override;
    property ActiveProperties: TcxCustomBlobEditProperties
      read GetActiveProperties;
    property Properties: TcxCustomBlobEditProperties read GetProperties
      write SetProperties;
  end;

  { TcxBlobEdit }

  TcxBlobEdit = class(TcxCustomBlobEdit)
  private
    function GetActiveProperties: TcxBlobEditProperties;
    function GetProperties: TcxBlobEditProperties;
    procedure SetProperties(Value: TcxBlobEditProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxBlobEditProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
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
    property Properties: TcxBlobEditProperties read GetProperties
      write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
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
    property OnEndDock;
    property OnStartDock;
  end;

  { TcxFilterBlobEditHelper }

  TcxFilterBlobEditHelper = class(TcxFilterTextEditHelper)
  public
    class function GetSupportedFilterOperators(
      AProperties: TcxCustomEditProperties;
      AValueTypeClass: TcxValueTypeClass;
      AExtendedSet: Boolean = False): TcxFilterControlOperators; override;
  end;

  { TcxBlobEditPopupWindow }

  TcxBlobEditPopupWindow = class(TcxPopupEditPopupWindow)
  protected
    procedure DoPopupControlKey(Key: Char); override;
    procedure ModalCloseUp; override;
  end;

var
  imgBlobImages: TcxImageList = nil;

function GetBlobImages: TcxImageList;
function GetBlobText(const Value: TcxEditValue; AProperties: TcxCustomBlobEditProperties; AFullText: Boolean): string;

implementation

uses
  Variants, Math, dxTypeHelpers, cxDrawTextUtils, cxEditUtils, cxGeometry, cxVariants, dxCore, dxThemeConsts,
  cxLookAndFeelPainters, dxDPIAwareUtils;

const
  cxbmBlobNull = 'CXBMBLOB_BLOB_NULL';
  cxbmBlob     = 'CXBMBLOB_BLOB';
  cxbmMemoNull = 'CXBMBLOB_MEMO_NULL';
  cxbmMemo     = 'CXBMBLOB_MEMO';
  cxbmPictNull = 'CXBMBLOB_PICT_NULL';
  cxbmPict     = 'CXBMBLOB_PICT';
  cxbmOleNull  = 'CXBMBLOB_OLE_NULL';
  cxbmOle      = 'CXBMBLOB_OLE';

type
  TcxEditStyleAccess = class(TcxEditStyle);

function GetBlobText(const Value: TcxEditValue; AProperties: TcxCustomBlobEditProperties; AFullText: Boolean): string;
begin
  Result := '';
  if AFullText and (AProperties.BlobEditKind = bekMemo) and VarIsStr(Value) then
    Result := Value
  else
    if AProperties.BlobPaintStyle = bpsDefault then
    begin
      case AProperties.BlobEditKind of
        bekMemo:
          if not VarIsNull(Value) then
            Result := cxGetResourceString(@cxSBlobMemo)
          else
            Result := cxGetResourceString(@cxSBlobMemoEmpty);
        bekPict:
          if not VarIsNull(Value) then
            Result := cxGetResourceString(@cxSBlobPicture)
          else
            Result := cxGetResourceString(@cxSBlobPictureEmpty);
      end;
    end
    else
      if (AProperties.BlobPaintStyle = bpsText) and (AProperties.BlobEditKind = bekMemo) and dxVarIsBlob(Value) then
      begin
        Result := dxVariantToString(Value);
        ExtractFirstLine(Result);
      end;
end;

{ TcxBlobEditViewData }

procedure TcxBlobEditViewData.Calculate(ACanvas: TcxCanvas;
  const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
  Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo, AIsMouseEvent);
  TcxBlobEditViewInfo(AViewInfo).IconRect := cxRectCenter(
    AViewInfo.ClientRect, dxGetImageSize(GetBlobImages, ScaleFactor));
  if Edit <> nil then
    EditValueToDrawValue(Edit.EditValue, AViewInfo);
  PrepareDrawTextFlags(ACanvas, AViewInfo);
end;

procedure TcxBlobEditViewData.EditValueToDrawValue(
  const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
begin
  with TcxCustomBlobEditProperties(Properties) do
  begin
    CorrectBlobEditKind;
    if BlobPaintStyle = bpsIcon then
    begin
      TcxBlobEditViewInfo(AViewInfo).Text := '';
      case BlobEditKind of
        bekMemo:
          TcxBlobEditViewInfo(AViewInfo).ImageIndex := 2;
        bekPict:
          TcxBlobEditViewInfo(AViewInfo).ImageIndex := 4;
        bekOle:
          TcxBlobEditViewInfo(AViewInfo).ImageIndex := 6;
        bekBlob:
          TcxBlobEditViewInfo(AViewInfo).ImageIndex := 0;
      end;
      if not VarIsSoftNull(AEditValue) then
        Inc(TcxBlobEditViewInfo(AViewInfo).ImageIndex);
    end
    else
    begin
      inherited;
      TcxBlobEditViewInfo(AViewInfo).ImageIndex := -1;
    end;
  end;
end;

function TcxBlobEditViewData.InternalGetEditConstantPartSize(
  ACanvas: TcxCanvas; AIsInplace: Boolean; AEditSizeProperties: TcxEditSizeProperties;
  var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize;
begin
  Result := inherited InternalGetEditConstantPartSize(ACanvas, AIsInplace, AEditSizeProperties, MinContentSize, AViewInfo);
  if TcxCustomBlobEditProperties(Properties).BlobPaintStyle = bpsIcon then
    Inc(Result.cx, dxGetImageSize(GetBlobImages, ScaleFactor).cx);
end;

function TcxBlobEditViewData.InternalEditValueToDisplayText(AEditValue: TcxEditValue): string;
begin
  Result := GetBlobText(AEditValue, TcxCustomBlobEditProperties(Properties), False);
end;

procedure TcxBlobEditViewData.PrepareDrawTextFlags(ACanvas: TcxCanvas;
  AViewInfo: TcxCustomEditViewInfo);
var
  AEditViewInfo: TcxBlobEditViewInfo;
begin
  AEditViewInfo := TcxBlobEditViewInfo(AViewInfo);
  AEditViewInfo.DrawTextFlags := CXTO_CENTER_VERTICALLY or CXTO_SINGLELINE;
  if not IsInplace or (epoShowEndEllipsis in PaintOptions) then
    AEditViewInfo.DrawTextFlags := AEditViewInfo.DrawTextFlags or CXTO_END_ELLIPSIS;
end;

{ TcxBlobEditViewInfo }

function TcxBlobEditViewInfo.NeedShowHint(ACanvas: TcxCanvas;
  const P: TPoint; const AVisibleBounds: TRect; out AText: TCaption;
  out AIsMultiLine: Boolean; out ATextRect: TRect; AMaxLineCount: Integer = 0): Boolean;
var
  AProperties: TcxCustomBlobEditProperties;
begin
  AProperties := TcxCustomBlobEditProperties(EditProperties);
  if (AProperties.BlobEditKind = bekMemo) and (AProperties.BlobPaintStyle = bpsText) then
    Result := inherited NeedShowHint(ACanvas, P, AVisibleBounds, AText, AIsMultiLine, ATextRect, AMaxLineCount)
  else
    Result := False;
end;

procedure TcxBlobEditViewInfo.Offset(DX, DY: Integer);
begin
  inherited Offset(DX, DY);
  OffsetRect(IconRect, DX, DY);
end;

function TcxBlobEditViewInfo.GetBackgroundPaintingStyle: TcxEditBackgroundPaintingStyle;
begin
  Result := bpsComboListEdit;
end;

procedure TcxBlobEditViewInfo.InternalPaint(ACanvas: TcxCanvas);
var
  R: TRect;
begin
  if not RectVisible(ACanvas.Handle, Bounds) then
    Exit;

  if (ImageIndex = -1) and (Text <> '') then
  begin
    DrawCustomEdit(ACanvas, True, True);
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font := Font;
    ACanvas.Font.Color := TextColor;
    cxTextOut(ACanvas.Canvas, Text, TextRect, DrawTextFlags);
    ACanvas.Brush.Style := bsSolid;
  end
  else
  begin
    if IsImageAssigned(GetBlobImages, ImageIndex) then
    begin
      ACanvas.SaveClipRegion;
      try
        IntersectRect(R, IconRect, ClientRect);
        ACanvas.IntersectClipRect(R);
        if not Transparent then
          ACanvas.FillRect(IconRect, BackgroundColor);
        cxDrawImage(ACanvas.Handle, IconRect, IconRect, nil, GetBlobImages, ImageIndex,
          EnabledImageDrawModeMap[Enabled], True, 0, clDefault, False, nil);
      finally
        ACanvas.RestoreClipRegion;
      end;
      if not Transparent then
        ACanvas.ExcludeClipRect(R);
    end;
    DrawCustomEdit(ACanvas, True, True);
  end;
  if Focused and not IsInplace and not HasPopupWindow then
  begin
    R := ClientRect;
    InflateRect(R, -1, -1);
    ACanvas.Font.Color := clWhite;
    ACanvas.Brush.Color := clBlack;
    ACanvas.DrawFocusRect(R);
  end;
end;

{ TcxCustomBlobEditProperties }

constructor TcxCustomBlobEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  ImmediateDropDownWhenActivated := True;
  PopupMinWidth := 160;
  PopupMinHeight := 140;
  FAlwaysSaveData := True;
  FBlobEditKind := bekAuto;
  FBlobPaintStyle := bpsIcon;
  // Memo
  FMemoCharCase := ecNormal;
  FMemoMaxLength := 0;
  FMemoOEMConvert := False;
  FMemoScrollBars := ssNone;
  FMemoWantReturns := True;
  FMemoWantTabs := True;
  FMemoWordWrap := True;
  // Picture
  FPictureAutoSize := True;
  FPictureGraphicClass := GetDefaultPictureGraphicClass;
  FPictureTransparency := gtDefault;
  FShowExPopupItems := True;
  FShowPicturePopup := True;
  FApplyStyleToButtons := True;
end;

function TcxCustomBlobEditProperties.GetPictureGraphicClassName: string;
begin
  if FPictureGraphicClass = nil then
    Result := ''
  else
    Result := FPictureGraphicClass.ClassName;
end;

function TcxCustomBlobEditProperties.IsPictureGraphicClassNameStored: Boolean;
begin
  Result := PictureGraphicClass <> GetDefaultPictureGraphicClass;
end;

procedure TcxCustomBlobEditProperties.ReadIsPictureGraphicClassNameEmpty(Reader: TReader);
begin
  Reader.ReadBoolean;
  PictureGraphicClassName := '';
end;

procedure TcxCustomBlobEditProperties.SetBlobEditKind(const Value: TcxBlobEditKind);
begin
  if FBlobEditKind <> Value then
  begin
    FBlobEditKind := Value;
    if ButtonGlyph.Empty then
      Buttons[0].Kind := DefaultButtonKind;
    Changed;
  end;
end;

procedure TcxCustomBlobEditProperties.SetBlobPaintStyle(
  const Value: TcxBlobPaintStyle);
begin
  if FBlobPaintStyle <> Value then
  begin
    FBlobPaintStyle := Value;
    Changed;
  end;
end;

procedure TcxCustomBlobEditProperties.SetPictureGraphicClass(Value: TGraphicClass);
begin
  if FPictureGraphicClass <> Value then
  begin
    FPictureGraphicClass := Value;
    Changed;
  end;
end;

procedure TcxCustomBlobEditProperties.SetPictureGraphicClassName(const Value: string);
var
  APictureGraphicClass: TGraphicClass;
begin
  if Value = '' then
    PictureGraphicClass := nil
  else
  begin
    APictureGraphicClass := GetGraphicClassByName(Value);
    if (APictureGraphicClass = nil) and (Value <> '') then
      APictureGraphicClass := TdxSmartImage;
    if APictureGraphicClass <> nil then
      PictureGraphicClass := APictureGraphicClass;
  end;
end;

procedure TcxCustomBlobEditProperties.WriteIsPictureGraphicClassNameEmpty(Writer: TWriter);
begin
  Writer.WriteBoolean(True);
end;

function TcxCustomBlobEditProperties.CanValidate: Boolean;
begin
  Result := BlobEditKind = bekMemo;
end;

function TcxCustomBlobEditProperties.DefaultButtonKind: TcxEditButtonKind;
begin
  if BlobEditKind in [bekPict, bekMemo] then
    Result := bkDown
  else
    Result := bkEllipsis;
end;

function TcxCustomBlobEditProperties.DropDownButtonVisibleIndex: Integer;
begin
  if BlobEditKind in [bekPict, bekMemo] then
    Result := 0
  else
    Result := -1;
end;

procedure TcxCustomBlobEditProperties.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('IsPictureGraphicClassNameEmpty', ReadIsPictureGraphicClassNameEmpty,
    WriteIsPictureGraphicClassNameEmpty, PictureGraphicClassName = '');
end;

function TcxCustomBlobEditProperties.DropDownOnClick: Boolean;
begin
  Result := True;
end;

function TcxCustomBlobEditProperties.GetDisplayFormatOptions: TcxEditDisplayFormatOptions;
begin
  Result := [];
end;

class function TcxCustomBlobEditProperties.GetPopupWindowClass: TcxCustomEditPopupWindowClass;
begin
  Result := TcxBlobEditPopupWindow;
end;

class function TcxCustomBlobEditProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxBlobEditViewData;
end;

function TcxCustomBlobEditProperties.HasDisplayValue: Boolean;
begin
  Result := False;
end;

procedure TcxCustomBlobEditProperties.CorrectBlobEditKind;
const
  ABlobEditKindCorrectionA: array[TcxBlobKind] of TcxBlobEditKind =
    (bekBlob, bekBlob, bekPict, bekMemo, bekOle);
begin
  if (BlobEditKind = bekAuto) and
    not((IDefaultValuesProvider <> nil) and not IDefaultValuesProvider.IsDataAvailable) then
  begin
    LockUpdate(True);
    try
      if IDefaultValuesProvider <> nil then
        BlobEditKind := ABlobEditKindCorrectionA[IDefaultValuesProvider.DefaultBlobKind]
      else
        BlobEditKind := bekBlob;
    finally
      LockUpdate(False);
    end;
  end;
end;

function TcxCustomBlobEditProperties.GetDefaultPictureGraphicClass: TGraphicClass;
begin
  if GetRegisteredGraphicClasses.Count > 0 then
    Result := TGraphicClass(GetRegisteredGraphicClasses[0])
  else
    Result := nil;
end;

function TcxCustomBlobEditProperties.GetPictureGraphicClass(AItem: TObject;
  ARecordIndex: Integer; APastingFromClipboard: Boolean = False): TGraphicClass;
begin
  Result := FPictureGraphicClass;
  if Result = nil then
  begin
    if APastingFromClipboard then
      Result := TBitmap;
    if Assigned(FOnGetGraphicClass) then
      FOnGetGraphicClass(AItem, ARecordIndex,
        APastingFromClipboard, Result);
  end;
end;

procedure TcxCustomBlobEditProperties.Assign(Source: TPersistent);
begin
  if Source is TcxCustomBlobEditProperties then
  begin
    BeginUpdate;
    try
      inherited Assign(Source);
      with TcxCustomBlobEditProperties(Source) do
      begin
        // Common
        Self.AlwaysSaveData := AlwaysSaveData;
        Self.ApplyStyleToButtons := ApplyStyleToButtons;
        Self.BlobEditKind := BlobEditKind;
        Self.BlobPaintStyle := BlobPaintStyle;
        // Memo
        Self.MemoAutoReplace := MemoAutoReplace;
        Self.MemoCharCase := MemoCharCase;
        Self.MemoMaxLength := MemoMaxLength;
        Self.MemoOEMConvert := MemoOEMConvert;
        Self.MemoScrollBars := MemoScrollBars;
        Self.MemoWantReturns := MemoWantReturns;
        Self.MemoWantTabs := MemoWantTabs;
        Self.MemoWordWrap := MemoWordWrap;
        // Picture
        Self.PictureAutoSize := PictureAutoSize;
        Self.PictureFilter := PictureFilter;
        Self.PictureGraphicClass := PictureGraphicClass;
        Self.PictureTransparency := PictureTransparency;
        Self.ShowExPopupItems := ShowExPopupItems;
        Self.ShowPicturePopup := ShowPicturePopup;
        Self.OnAssignPicture := OnAssignPicture;
        Self.OnGetGraphicClass := OnGetGraphicClass;
      end;
    finally
      EndUpdate
    end
  end
  else
    inherited Assign(Source);
end;

function TcxCustomBlobEditProperties.CompareDisplayValues(
  const AEditValue1, AEditValue2: TcxEditValue): Boolean;
begin
  CorrectBlobEditKind;
  Result := (BlobPaintStyle = bpsText) and (BlobEditKind = bekMemo) and
    VarIsStr(AEditValue1) and VarIsStr(AEditValue2) and
    InternalCompareString(AEditValue1, AEditValue2, True);
end;

class function TcxCustomBlobEditProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxBlobEdit;
end;

function TcxCustomBlobEditProperties.GetDisplayText(
  const AEditValue: TcxEditValue; AFullText: Boolean = False;
  AIsInplace: Boolean = True): string;
begin
  Result := '';
  if not VarIsNull(AEditValue) then
    case BlobEditKind of
      bekMemo:
        Result := GetBlobText(AEditValue, Self, AFullText);
      bekPict:
        Result := cxGetResourceString(@cxSBlobPicture);
      bekOle:
        Result := 'OLE';
      bekBlob:
        Result := 'BLOB';
    end;
end;

function TcxCustomBlobEditProperties.GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource;
begin
  CorrectBlobEditKind;
  if BlobEditKind <> bekMemo then
    Result := evsValue
  else
    if (IDefaultValuesProvider <> nil) and IDefaultValuesProvider.IsBlob then
      Result := evsValue
    else
      Result := evsText;
end;

function TcxCustomBlobEditProperties.GetSpecialFeatures: TcxEditSpecialFeatures;
begin
  Result := inherited GetSpecialFeatures;
  if BlobEditKind = bekPict then
    Include(Result, esfBlobEditValue);
end;

function TcxCustomBlobEditProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := [esoEditing];
  if Buttons.Count > 0 then
    Include(Result, esoHotTrack);
end;

class function TcxCustomBlobEditProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxBlobEditViewInfo;
end;

function TcxCustomBlobEditProperties.IsEditValueValid(
  var AEditValue: TcxEditValue; AEditFocused: Boolean): Boolean;
begin
  Result := True;
end;

function KeyDownToModalResult(var Key: Word; Shift: TShiftState;
  AReturnClosed, AAlwaysSaveData: Boolean): TModalResult;
begin
  Result := mrNone;
  if ((Key = VK_F4) and not (ssAlt in Shift)) or (Key = VK_ESCAPE) or
    ((Key in [VK_UP, VK_DOWN]) and (ssAlt in Shift)) then
  begin
    if AAlwaysSaveData and not (Key = VK_ESCAPE) then
      Result := mrOk
    else
      Result := mrCancel
  end
  else
    if (TranslateKey(Key) = VK_RETURN) and (AReturnClosed or (ssCtrl in Shift)) then
      Result := mrOk;
  if Result <> mrNone then
    Key := 0;
end;

{ TcxPopupMemo }

procedure TcxPopupMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  TcxCustomEditPopupWindow(Parent).ModalResult :=
    KeyDownToModalResult(Key, Shift, not ActiveProperties.WantReturns,
      FBlobEdit.ActiveProperties.AlwaysSaveData);
  if Key = 0 then
    dxMessagesController.KillMessages(InnerControl.Handle, WM_CHAR) // to block Ctrl+Enter
  else
    inherited KeyDown(Key, Shift);
end;

procedure TcxPopupMemo.SpellCheckerSetValue(const AValue: Variant);
begin
  FBlobEdit.SpellCheckerSetValue(AValue);
end;

{ TcxPopupImage }

procedure TcxPopupImage.KeyDown(var Key: Word; Shift: TShiftState);
begin
  TcxCustomEditPopupWindow(Parent).ModalResult :=
    KeyDownToModalResult(Key, Shift, True,
      FBlobEdit.ActiveProperties.AlwaysSaveData);
  inherited KeyDown(Key, Shift);
end;

{ TcxCustomBlobEdit }

destructor TcxCustomBlobEdit.Destroy;
begin
  DestroyPopupControls;
  inherited Destroy;
end;

procedure TcxCustomBlobEdit.DoPopupImageGetGraphicClass(Sender: TObject;
  APastingFromClipboard: Boolean; var AGraphicClass: TGraphicClass);
begin
  AGraphicClass := GetPictureGraphicClass(APastingFromClipboard);
end;

procedure TcxCustomBlobEdit.DoPopupImagePropertiesGetGraphicClass(
  AItem: TObject; ARecordIndex: Integer; APastingFromClipboard: Boolean;
  var AGraphicClass: TGraphicClass);
begin
  DoPopupImageGetGraphicClass(nil, APastingFromClipboard, AGraphicClass);
end;

function TcxCustomBlobEdit.GetActiveProperties: TcxCustomBlobEditProperties;
begin
  Result := TcxCustomBlobEditProperties(InternalGetActiveProperties);
end;

function TcxCustomBlobEdit.GetProperties: TcxCustomBlobEditProperties;
begin
  Result := TcxCustomBlobEditProperties(inherited Properties);
end;

function TcxCustomBlobEdit.GetStorageMemo: TcxPopupMemo;
begin
  if FStorage is TcxPopupMemo then
    Result := TcxPopupMemo(FStorage)
  else
    Result := nil;
end;

function TcxCustomBlobEdit.GetStorageImage: TcxPopupImage;
begin
  if FStorage is TcxPopupImage then
    Result := TcxPopupImage(FStorage)
  else
    Result := nil;
end;

procedure TcxCustomBlobEdit.InternalChanged(Sender: TObject);
begin
  if FOkButton <> nil then
  begin
    FOkButton.Enabled := True(*FStorage.ModifiedAfterEnter*);
    FOkButton.Default := FOkButton.Enabled;
  end;
end;

procedure TcxCustomBlobEdit.PictureClosePopup(Sender: TcxControl; AReason: TcxEditCloseUpReason);
begin
  if not HasPopupWindow then
  begin
    if AReason = crEnter then
      SaveStorage;
    DoClosePopup(AReason);
  end;
end;

procedure TcxCustomBlobEdit.SaveStorage;

  function NeedSaveStorage: Boolean;
  begin
    Result := FStorage.EditModified and ((PopupWindow.ModalResult = mrOk) or
      ((PopupWindow.ModalResult <> mrCancel) and ActiveProperties.AlwaysSaveData)) and
      DoEditing;
  end;

begin
  if NeedSaveStorage then
  begin
    if StorageMemo <> nil then
      InternalEditValue := StorageMemo.Lines.Text
    else
      InternalEditValue := FStorage.EditValue;
    ModifiedAfterEnter := True;

    DoAssignPicture;
    if ActiveProperties.ImmediatePost and CanPostEditValue and InternalValidateEdit then
      InternalPostEditValue;
  end;
end;

procedure TcxCustomBlobEdit.SetProperties(Value: TcxCustomBlobEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxCustomBlobEdit.ColorChanged;
begin
  inherited ColorChanged;
  if not IsDestroying and HasPopupWindow and (StorageMemo <> nil) then
    StorageMemo.Style.Color := GetBackgroundColor;
end;

procedure TcxCustomBlobEdit.CreatePopupControls;

  procedure InitStorage(AStorageClass: TcxCustomEditClass);
  begin
    if (FStorage <> nil) and (AStorageClass <> FStorage.ClassType) then
      DestroyPopupControls;
    if FStorage = nil then
    begin
      FStorage := AStorageClass.Create(nil);
      FStorage.OnEditing := StorageEditingHandler;
      FStorage.Style.BorderStyle := ebsNone;
      FStorage.ActiveProperties.OnChange := InternalChanged;
    end;
    FStorage.ActiveProperties.ReadOnly := not CanModify;
    ActiveProperties.PopupControl := FStorage;
    ActiveProperties.PopupSysPanelStyle := True;
    FStorage.Parent := PopupWindow;
  end;

  procedure InitButton(var AButton: TcxButton; const ACaption: string; AHeight, AWidth: Integer);
  begin
    if AButton = nil then
    begin
      AButton := TcxButton.Create(nil);
      AButton.UseSystemPaint := False;
    end;
    AButton.Caption := ACaption;
    AButton.LookAndFeel.MasterLookAndFeel := PopupControlsLookAndFeel;
    if ViewInfo.UseSkins then
      AButton.LookAndFeel.SkinPainter := ViewInfo.Painter
    else
      AButton.LookAndFeel.SkinPainter := nil;

    dxAdjustToTouchableSize(AHeight, ScaleFactor);
    AButton.Parent := PopupWindow;
    AButton.Height := AHeight;
    AButton.Width := AWidth;
  end;

  procedure InitControls;
  var
    ACancelCaption, AOkCaption: string;
    ATempWidth, AButtonHeight: Integer;
    AFont: TFont;
  begin
    if ActiveProperties.ReadOnly or not DataBinding.IsDataAvailable then
      ACancelCaption := cxGetResourceString(@cxSBlobButtonClose)
    else
      ACancelCaption := cxGetResourceString(@cxSBlobButtonCancel);
    AOkCaption := cxGetResourceString(@cxSBlobButtonOK);

    if ActiveProperties.ApplyStyleToButtons then
      PopupWindow.Font.Size := VisibleFont.Size;
    AFont := PopupWindow.Font;
    FButtonWidth := Max(cxTextWidth(AFont, ACancelCaption + '00'), cxTextWidth(AFont, AOkCaption + '00'));
    AButtonHeight := MulDiv(cxTextHeight(AFont), 20, 13);

    InitButton(FCancelButton, ACancelCaption, AButtonHeight, FButtonWidth);
    FCancelButton.Cancel := True;
    FCancelButton.ModalResult := mrCancel;

    if not ActiveProperties.ReadOnly and DataBinding.IsDataAvailable then
    begin
      InitButton(FOkButton, AOkCaption, AButtonHeight, FButtonWidth);
      FOkButton.Enabled := FStorage.EditModified;
      FOkButton.ModalResult := mrOk;
    end
    else
      FreeAndNil(FOkButton);

    PopupWindow.MinSysPanelHeight := AButtonHeight + (AButtonHeight div 3);
    ATempWidth := (AButtonHeight div 3) + FButtonWidth;
    if FOkButton <> nil then
      ATempWidth := ATempWidth * 2;
    ActiveProperties.PopupMinWidth := ActiveProperties.ScaleFactor.Apply(
      ATempWidth + dxGetSystemMetrics(SM_CXVSCROLL, ScaleFactor) + AButtonHeight div 2, ScaleFactor);
  end;

var
  AMemoProperties: TcxMemoProperties;
  AImageProperties: TcxImageProperties;
begin
  case ActiveProperties.BlobEditKind of
    bekPict:
      begin
        InitStorage(TcxPopupImage);
        AImageProperties := StorageImage.ActiveProperties;
        StorageImage.FBlobEdit := Self;
        AImageProperties.ShowFocusRect := False;
        AImageProperties.Stretch := True;
        AImageProperties.CustomFilter := ActiveProperties.PictureFilter;
        AImageProperties.GraphicTransparency := ActiveProperties.PictureTransparency;
        AImageProperties.OnClosePopup := PictureClosePopup;
        if not ActiveProperties.ShowPicturePopup then
          AImageProperties.PopupMenuLayout.MenuItems := []
        else
          if not ActiveProperties.ShowExPopupItems then
            AImageProperties.PopupMenuLayout.MenuItems :=
              AImageProperties.PopupMenuLayout.MenuItems - [pmiSave, pmiLoad];

        AImageProperties.GraphicClass := ActiveProperties.PictureGraphicClass;
        //AImageProperties.OnAssignPicture := ActiveProperties.OnAssignPicture;
        //AImageProperties.OnGetGraphicClass := ActiveProperties.OnGetGraphicClass;
        StorageImage.OnGetGraphicClass := DoPopupImageGetGraphicClass;
        AImageProperties.OnGetGraphicClass := DoPopupImagePropertiesGetGraphicClass;
        LoadPicture(StorageImage.Picture, GetPictureGraphicClass, EditValue);
        StorageImage.EditModified := False;
        StorageImage.Style.LookAndFeel.MasterLookAndFeel := PopupControlsLookAndFeel;
        ActiveProperties.PopupAutoSize := ActiveProperties.PictureAutoSize;
        InitControls;
      end;
    bekMemo:
      begin
        InitStorage(TcxPopupMemo);
        AMemoProperties := StorageMemo.ActiveProperties;
        StorageMemo.FBlobEdit := Self;
        AMemoProperties.CharCase := ActiveProperties.MemoCharCase;
        AMemoProperties.ImeMode := ActiveProperties.ImeMode;
        AMemoProperties.ImeName := ActiveProperties.ImeName;
        AMemoProperties.MaxLength := ActiveProperties.MemoMaxLength;
        AMemoProperties.OEMConvert := ActiveProperties.MemoOEMConvert;
        AMemoProperties.ScrollBars := ActiveProperties.MemoScrollBars;
        AMemoProperties.WantReturns := ActiveProperties.MemoWantReturns;
        AMemoProperties.WantTabs := ActiveProperties.MemoWantTabs;
        AMemoProperties.WordWrap := ActiveProperties.MemoWordWrap;
        ActiveProperties.PopupAutoSize := False;
        StorageMemo.EditValue := EditValue;
        StorageMemo.Style.LookAndFeel.MasterLookAndFeel := PopupControlsLookAndFeel;
        StorageMemo.Style.Font.Assign(VisibleFont);
        StorageMemo.Style.Color := GetBackgroundColor;
        StorageMemo.Style.TextColor := VisibleFont.Color;
        InitControls;
      end;
  end;
end;

procedure TcxCustomBlobEdit.DoAssignPicture;
begin
  if StorageImage <> nil then
  begin
    LockEditValueChanging(True);
    try
      DoOnAssignPicture(StorageImage.Picture);
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

procedure TcxCustomBlobEdit.DoOnAssignPicture(APicture: TPicture);
begin
  with Properties do
    if Assigned(OnAssignPicture) then
      OnAssignPicture(Self, APicture);
  if RepositoryItem <> nil then
    with ActiveProperties do
      if Assigned(OnAssignPicture) then
        OnAssignPicture(Self, APicture);
end;

procedure TcxCustomBlobEdit.FontChanged;
begin
  inherited FontChanged;
  if not IsDestroying and HasPopupWindow then
  begin
    if StorageMemo <> nil then
      StorageMemo.Style.Font.Assign(VisibleFont);
    if FOkButton <> nil then
      FOkButton.Font.Assign(VisibleFont);
    if FCancelButton <> nil then
      FCancelButton.Font.Assign(VisibleFont);
  end;
end;

function TcxCustomBlobEdit.GetDisplayText: string;
begin
  if StorageMemo <> nil then
    Result := StorageMemo.Text
  else
    Result := inherited GetDisplayText;
end;

function TcxCustomBlobEdit.GetEditingValue: TcxEditValue;
begin
  if HasPopupWindow and (ActiveProperties.BlobEditKind in [bekMemo, bekPict]) then
    if StorageMemo <> nil then
      Result := StorageMemo.Text
    else
      Result := FStorage.EditValue
  else
    Result := EditValue;
end;

function TcxCustomBlobEdit.GetPopupFocusedControl: TWinControl;
begin
  if FStorage <> nil then
    Result := FStorage
  else
    Result := inherited GetPopupFocusedControl;
end;

function TcxCustomBlobEdit.GetPopupWindowClientPreferredSize: TSize;
begin
  if ActiveProperties.PictureAutoSize and (StorageImage <> nil) then
  begin
    Result := dxGetImageSize(StorageImage.Picture, ScaleFactor);
    Inc(Result.cx, ScaleFactor.Apply(4));
    Inc(Result.cy, ScaleFactor.Apply(4));
  end
  else
    Result := inherited GetPopupWindowClientPreferredSize;
end;

procedure TcxCustomBlobEdit.InternalValidateDisplayValue(const ADisplayValue: TcxEditValue);
begin
  if StorageMemo <> nil then
    StorageMemo.Text := ADisplayValue;
  inherited InternalValidateDisplayValue(ADisplayValue);
end;

procedure TcxCustomBlobEdit.DestroyPopupControls;
begin
  if not ((StorageImage <> nil) and StorageImage.IsDialogShowed) then
  begin
    ActiveProperties.PopupControl := nil;
    FreeAndNil(FOkButton);
    FreeAndNil(FCancelButton);
    FreeAndNil(FStorage);
  end;
end;

procedure TcxCustomBlobEdit.DoInitPopup;
begin
  inherited DoInitPopup;
  CreatePopupControls;
end;

procedure TcxCustomBlobEdit.DoFinalizePopup;
begin
  inherited;
end;

function TcxCustomBlobEdit.GetPictureGraphicClass(
  APastingFromClipboard: Boolean = False): TGraphicClass;
begin
  if IsInplace then
    with InplaceParams do
      Result := ActiveProperties.GetPictureGraphicClass(Position.Item,
        Position.RecordIndex, APastingFromClipboard)
  else
  begin
    Result := ActiveProperties.PictureGraphicClass;
    if Result = nil then
    begin
      if APastingFromClipboard then
        Result := TBitmap;
      if Assigned(FOnGetGraphicClass) then
        FOnGetGraphicClass(Self, APastingFromClipboard, Result);
    end;
  end;
end;

procedure TcxCustomBlobEdit.Initialize;
begin
  inherited Initialize;
  ControlStyle := ControlStyle - [csSetCaption];
  TabStop := True;
end;

function TcxCustomBlobEdit.InternalGetText: string;
begin
  if IsDesigning then
  begin
    Result := VarToStr(EditValue);
    Exit;
  end;
  Result := '';
  if ActiveProperties.BlobEditKind = bekMemo then
    if HasPopupWindow then
      Result := StorageMemo.Text
    else
      if VarIsStr(EditValue) then
        Result := VarToStr(EditValue);
end;

function TcxCustomBlobEdit.InternalSetText(const Value: string): Boolean;
begin
  if IsDesigning then
  begin
    EditValue := Value;
    Result := True;
    Exit;
  end;
  Result := ActiveProperties.BlobEditKind = bekMemo;
  if not Result then
    Exit;
  if HasPopupWindow then
    StorageMemo.EditingText := Value
  else
    EditValue := Value;
end;

procedure TcxCustomBlobEdit.PopupWindowClosed(Sender: TObject);
begin
  LockChangeEvents(True);
  try
    SaveStorage;
    inherited PopupWindowClosed(Sender);
    ShortRefreshContainer(False);
  finally
    LockChangeEvents(False);
  end;
end;

procedure TcxCustomBlobEdit.PropertiesChanged(Sender: TObject);
begin
  if IsDestroying then
    Exit;
  inherited PropertiesChanged(Sender);
  ShortRefreshContainer(False);
  if HasPopupWindow and (StorageImage <> nil) then
    StorageImage.ActiveProperties.GraphicClass := ActiveProperties.PictureGraphicClass;
  if HasPopupWindow and (StorageMemo <> nil) then
  begin
    StorageMemo.ActiveProperties.ImeMode := ActiveProperties.ImeMode;
    StorageMemo.ActiveProperties.ImeName := ActiveProperties.ImeName;
  end;
end;

procedure TcxCustomBlobEdit.SetEditingText(const Value: TCaption);
begin
  if ActiveProperties.BlobEditKind = bekMemo then
    if HasPopupWindow then
      Text := Value
    else
      inherited SetEditingText(Value);
end;

procedure TcxCustomBlobEdit.SetupPopupWindow;
var
  AHeight, AOffset: Integer;
  R: TRect;
  ACancelButtonBounds, AOKButtonBounds: TRect;
  AAnchors: TAnchors;
begin
  if FCancelButton = nil then
  begin
    inherited SetupPopupWindow;
    Exit;
  end;
  TcxEditStyleAccess(Style).PopupCloseButton := False;
  inherited SetupPopupWindow;
  AHeight := FCancelButton.Height;
  AOffset := AHeight div 6;

  R := PopupWindow.ViewInfo.SizeGripRect;

  if UseRightToLeftAlignment then
    R := TdxRightToLeftLayoutConverter.ConvertRect(R, PopupWindow.ClientRect);

  if (PopupWindow.ViewInfo.SizeGripCorner in [coTopRight, coBottomRight]) xor UseRightToLeftAlignment then
    ACancelButtonBounds.Left :=  R.Left - AOffset - FButtonWidth
  else
    ACancelButtonBounds.Left :=  PopupWindow.Width - 2 * AOffset - FButtonWidth;

  if PopupWindow.ViewInfo.SizeGripCorner in [coTopLeft, coTopRight] then
  begin
    ACancelButtonBounds.Top := R.Top + (PopupWindow.ViewInfo.SysPanelHeight - AHeight) div 2 - 2;
    AAnchors := [akTop];
  end
  else
  begin
    ACancelButtonBounds.Top := R.Bottom - (AHeight + (PopupWindow.ViewInfo.SysPanelHeight - AHeight) div 2 - 2);
    AAnchors := [akBottom];
  end;
  ACancelButtonBounds.Width := FButtonWidth;
  ACancelButtonBounds.Height := AHeight;

  AOKButtonBounds.InitSize(ACancelButtonBounds.Left - (FButtonWidth + (AHeight div 3)),
    ACancelButtonBounds.Top, FButtonWidth, AHeight);

  if UseRightToLeftAlignment then
  begin
    AOKButtonBounds := TdxRightToLeftLayoutConverter.ConvertRect(AOKButtonBounds, PopupWindow.ClientRect);
    ACancelButtonBounds := TdxRightToLeftLayoutConverter.ConvertRect(ACancelButtonBounds, PopupWindow.ClientRect);
    Include(AAnchors, akLeft);
  end
  else
    Include(AAnchors, akRight);

  FCancelButton.BoundsRect := ACancelButtonBounds;
  FCancelButton.Anchors := AAnchors;
  if FOkButton <> nil then
  begin
    FOkButton.BoundsRect := AOKButtonBounds;
    FOkButton.Anchors := AAnchors;
  end;

end;

procedure TcxCustomBlobEdit.StorageEditingHandler(Sender: TObject;
  var CanEdit: Boolean);
begin
  CanEdit := CanModify;
end;

procedure TcxCustomBlobEdit.SynchronizeDisplayValue;
begin
  ShortRefreshContainer(False);
end;

procedure TcxCustomBlobEdit.SynchronizeEditValue;
begin
  if StorageMemo <> nil then
    inherited SynchronizeEditValue;
end;

class function TcxCustomBlobEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomBlobEditProperties;
end;

function TcxCustomBlobEdit.IsEditClass: Boolean;
begin
  Result := False;
end;

{ TcxBlobEdit }

class function TcxBlobEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxBlobEditProperties;
end;

function TcxBlobEdit.GetActiveProperties: TcxBlobEditProperties;
begin
  Result := TcxBlobEditProperties(InternalGetActiveProperties);
end;

function TcxBlobEdit.GetProperties: TcxBlobEditProperties;
begin
  Result := TcxBlobEditProperties(inherited Properties);
end;

procedure TcxBlobEdit.SetProperties(Value: TcxBlobEditProperties);
begin
  Properties.Assign(Value);
end;

{ TcxFilterBlobEditHelper }

class function TcxFilterBlobEditHelper.GetSupportedFilterOperators(
  AProperties: TcxCustomEditProperties;
  AValueTypeClass: TcxValueTypeClass;
  AExtendedSet: Boolean = False): TcxFilterControlOperators;
begin
  Result := [fcoBlanks, fcoNonBlanks];
  if (AProperties is TcxBlobEditProperties) and
    (TcxBlobEditProperties(AProperties).BlobEditKind = bekMemo) then
  Result := Result + [fcoLike, fcoNotLike, fcoContains, fcoNotContains, fcoBeginsWith, fcoEndsWith];
end;

{ TcxBlobEditPopupWindow }

procedure TcxBlobEditPopupWindow.DoPopupControlKey(Key: Char);
var
  AStorage: TcxPopupMemo;
begin
  AStorage := TcxCustomBlobEdit(Edit).StorageMemo;
  if AStorage <> nil then
    if not AStorage.ActiveProperties.ReadOnly then
      if TcxCustomBlobEdit(Edit).ActiveProperties.MemoAutoReplace then
      begin
        AStorage.InnerEdit.EditValue := Key;
        AStorage.SelStart := 1;
        AStorage.ModifiedAfterEnter := True;
      end
      else
        inherited DoPopupControlKey(Key);
end;

procedure TcxBlobEditPopupWindow.ModalCloseUp;
begin
  dxMessagesController.LockMessages([CM_ENTER, CM_EXIT]);
  try
    inherited;
  finally
    dxMessagesController.UnlockMessages([CM_ENTER, CM_EXIT]);
  end;
end;

procedure LoadBlobImages;
const
  ImageNames: array[0..7] of string = (cxbmBlobNull, cxbmBlob,
    cxbmMemoNull, cxbmMemo, cxbmPictNull, cxbmPict, cxbmOleNull, cxbmOle);
var
  ABitmap: TBitmap;
  I: Integer;
begin
  ABitmap := TBitmap.Create;
  try
    ABitmap.LoadFromResourceName(HInstance, cxbmBlobNull);
    imgBlobImages := TcxImageList.CreateSize(ABitmap.Width, ABitmap.Height);
    for I := Low(ImageNames) to High(ImageNames) do
    begin
      ABitmap.LoadFromResourceName(HInstance, ImageNames[I]);
      GetBlobImages.AddMasked(ABitmap, clFuchsia);
    end;
  finally
    ABitmap.Free;
  end;
end;

function GetBlobImages: TcxImageList;
begin
  if imgBlobImages = nil then
    LoadBlobImages;
  Result := imgBlobImages;
end;

initialization
  GetRegisteredEditProperties.Register(TcxBlobEditProperties, scxSEditRepositoryBlobItem);

finalization
  FreeAndNil(imgBlobImages);

end.
