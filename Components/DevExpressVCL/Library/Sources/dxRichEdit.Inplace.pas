{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Inplace;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Windows, Forms, Classes, Controls, Graphics, StdCtrls, Messages,
  dxCore, dxCoreClasses, cxDrawTextUtils, cxGraphics, dxCoreGraphics,
  cxControls, cxContainer, cxEdit,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Keyboard,
  dxRichEdit.Types,
  dxRichEdit.NativeApi,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.View.Core,
  dxRichEdit.View.Inplace,
  dxRichEdit.InnerControl,
  dxRichEdit.Control.Core,
  dxRichEdit.Control.Keyboard;

type
  TdxCustomInplaceRichEditProperties = class;
  TcxInplaceRichEditHelper = class;

  { TdxInplaceRichEditViewInfo }

  TdxInplaceRichEditViewInfo = class(TcxCustomEditViewInfo)
  strict private
    FDisplayValue: Variant;
  protected
    FRenderBounds: TRect;
    procedure InternalPaint(ACanvas: TcxCanvas); override;
  public
    property DisplayValue: Variant read FDisplayValue write FDisplayValue;
  end;

  { TdxInplaceRichEditViewData }

  TdxInplaceRichEditViewData = class(TcxCustomEditViewData)
  private
    function GetProperties: TdxCustomInplaceRichEditProperties;
  protected
    function InternalGetEditContentSize(ACanvas: TcxCanvas;
      const AEditValue: TcxEditValue;
      const AEditSizeProperties: TcxEditSizeProperties): TSize; override;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
      AIsMouseEvent: Boolean); override;
    procedure EditValueToDrawValue(const AEditValue: TcxEditValue;
      AViewInfo: TcxCustomEditViewInfo); override;

    property Properties: TdxCustomInplaceRichEditProperties read GetProperties;
  end;

  { TdxCustomInplaceRichEditProperties }

  TdxCustomInplaceRichEditProperties = class(TcxCustomEditProperties)
  private
    FWantReturns: Boolean;
    FWantTabs: Boolean;
    procedure SetWantReturns(Value: Boolean);
    procedure SetWantTabs(Value: Boolean);
  protected
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    function HasDisplayValue: Boolean; override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
  public
    constructor Create(AOwner: TPersistent); override;
    class function GetContainerClass: TcxContainerClass; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function CanCompareEditValue: Boolean; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;

    property AutoSelect default False;
    property WantReturns: Boolean read FWantReturns write SetWantReturns default True;
    property WantTabs: Boolean read FWantTabs write SetWantTabs default True;
  end;

  { TdxInplaceRichEditProperties }

  TdxInplaceRichEditProperties = class(TdxCustomInplaceRichEditProperties)

  end;

  { TdxCustomInplaceRichEdit }

  TdxCustomInplaceRichEdit = class(TcxCustomEdit)
  strict private
    FInnerEditPositionAdjusting: Boolean;
  private
    function GetActiveProperties: TdxCustomInplaceRichEditProperties;
    function GetDocument: IdxRichEditDocument;
    function GetDocumentModel: TdxDocumentModel;
    function GetViewInfo: TdxInplaceRichEditViewInfo;
  protected
    procedure AdjustInnerEdit;
    procedure AdjustInnerEditPosition; override;
    function CanAutoSize: Boolean; override;
    function CanKeyDownModifyEdit(Key: Word; Shift: TShiftState): Boolean; override;
    function CanKeyPressModifyEdit(Key: Char): Boolean; override;
    procedure ChangeHandler(Sender: TObject); override;
    function DoRefreshContainer(const P: TPoint; Button: TcxMouseButton; Shift: TShiftState; AIsMouseEvent: Boolean): Boolean; override;
    procedure DoSetFocusWhenActivate; override;
    function GetEditValue: Variant; override;
    function GetEditingValue: Variant; override;
    function GetInnerEditClass: TControlClass; override;
    function GetDisplayValue: TcxEditValue; override;
    procedure PropertiesChanged(Sender: TObject); override;
    procedure Initialize; override;
    procedure InternalSetEditValue(const Value: Variant; AValidateEditValue: Boolean); override;
    procedure InternalValidateDisplayValue(const ADisplayValue: Variant); override;
    procedure SynchronizeDisplayValue; override;
    procedure SynchronizeEditValue; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function WantNavigationKeys: Boolean; override;
    procedure UpdateDrawValue; override;

    property Document: IdxRichEditDocument read GetDocument;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function IsEditClass: Boolean; override;
    procedure SelectAll; override;

    property ActiveProperties: TdxCustomInplaceRichEditProperties read GetActiveProperties;
    property ViewInfo: TdxInplaceRichEditViewInfo read GetViewInfo;
  end;

  { TdxInplaceRichEdit }

  TdxInplaceRichEdit = class(TdxCustomInplaceRichEdit)
  published
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
  end;

  { TdxInplaceRichEditOptions }

  TdxInplaceRichEditOptions = class(TdxRichEditControlCustomOptions);

  { TdxInnerInplaceRichEdit }

  TdxInnerInplaceRichEdit = class(TdxRichEditControlBase,
    IcxInnerEditHelper)
  strict private
    FAutoSelect: Boolean;
    FHelper: TcxInplaceRichEditHelper;
    FInternalUpdating: Boolean; // todo:
    FIsEditValueNull: Boolean;

    FOnChanged: TNotifyEvent;

    function GetContainer: TdxCustomInplaceRichEdit;

    // IcxInnerEditHelper
    function GetHelper: IcxCustomInnerEdit;

    procedure WMClear(var Message: TMessage); message WM_CLEAR;
    procedure WMSetFont(var Message: TWMSetFont); message WM_SETFONT;
    procedure WMSetText(var Message: TWMSetText); message WM_SETTEXT;
    procedure WMGetText(var Message: TMessage); message WM_GETTEXT;
    procedure WMGetTextLength(var Message: TWMGetTextLength); message WM_GETTEXTLENGTH;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    function CanKeyDownModifyEdit(Key: Word; Shift: TShiftState): Boolean;
    function CreateDocumentServer(ADocumentModel: TdxDocumentModel): IdxRichEditDocumentContainer; override;
    function CreateOptions(const ADocumentServer: TObject{TdxInnerRichEditDocumentServer}): TObject{TdxRichEditControlOptionsBase}; override;
    function CreateInnerControl: TdxInnerRichEditControl; override;
    function CreateHorizontalRuler: IdxRulerControl; override;
    function CreateVerticalRuler: IdxRulerControl; override;
    function CreateViewRepository: TdxRichEditCustomViewRepository; override;
    procedure SafelySetFocus;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure RaiseDeferredEvents(AChangeActions: TdxDocumentModelChangeActions); override;

    procedure DoChanged;
    function CalculateVerticalScrollbarVisibility: Boolean; override;

    procedure InitializeOptions;

    property AutoSelect: Boolean read FAutoSelect write FAutoSelect;
    property Helper: TcxInplaceRichEditHelper read FHelper;
    property Container: TdxCustomInplaceRichEdit read GetContainer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetEditValue: Variant;
    procedure SetEditValue(const Value: Variant);

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  { TcxInplaceRichEditDocumentHelper }

  TcxInplaceRichEditDocumentHelper = class
  public
    class procedure SetEditValue(ADocumentModel: TdxDocumentModel; const ADocument: IdxRichEditDocument; const Value: Variant); static;
    class function GetEditValue(const ADocument: IdxRichEditDocument): Variant; static;
    class procedure SetDefaultFont(const ADocument: IdxRichEditDocument; const AFont: TFont;
      AForeColor: TdxAlphaColor); overload; static;
    class procedure SetDefaultFont(const ADocument: IdxRichEditDocument; const AFont: TFont); overload; static;
    class procedure SynchronizePropertiesAndOptions(ASource: TdxCustomInplaceRichEditProperties;
      ADestination: TdxRichEditControlCustomOptions); static;
  end;

  { TcxInplaceRichEditHelper }

  TcxInplaceRichEditHelper = class(TcxInterfacedPersistent,
    IcxContainerInnerControl,
    IcxCustomInnerEdit)
  private
    FEdit: TdxInnerInplaceRichEdit;
  protected
    property Edit: TdxInnerInplaceRichEdit read FEdit;
  public
    constructor Create(AEdit: TdxInnerInplaceRichEdit); reintroduce; virtual;

    // IcxContainerInnerControl
    function GetControlContainer: TcxContainer;
    function GetControl: TWinControl;

    // IcxCustomInnerEdit
    function CallDefWndProc(AMsg: UINT; WParam: WPARAM;
      LParam: LPARAM): LRESULT;
    function CanProcessClipboardMessages: Boolean;
    function GetEditValue: TcxEditValue;
    function GetOnChange: TNotifyEvent;
    function GetReadOnly: Boolean;
    procedure LockBounds(ALock: Boolean);
    procedure SafelySetFocus;
    procedure SetEditValue(const Value: TcxEditValue);
    procedure SetParent(Value: TWinControl);
    procedure SetOnChange(Value: TNotifyEvent);
    procedure SetReadOnly(Value: Boolean);

    property EditValue: Variant read GetEditValue write SetEditValue;
  end;

  { TdxInplaceRichEditInnerControl }

  TdxInplaceRichEditInnerControl = class(TdxRichEditCustomInnerControl)
  protected
    function CreateDocumentModelCore: TdxDocumentModel; override;
    function CreateKeyboardController: TdxCustomKeyboardController; override;
  public
    procedure BeginInitialize; override;
  end;

  { TdxInplaceRichEditKeyboardDefaultHandler }

  TdxInplaceRichEditKeyboardDefaultHandler = class(TdxRichEditKeyboardDefaultHandler)
  protected
    procedure PopulateDialogsCommandTable; override;
    procedure PopulatePagesCommandTable; override;
  end;

  { TdxInplaceRichEditKeyboardController }

  TdxInplaceRichEditKeyboardController = class(TdxRichEditKeyboardController)
  protected
    function CreateDefaultHandler: IdxKeyboardHandlerService; override;
  end;

implementation

uses
  Variants, cxVariants, cxGeometry, dxTypeHelpers, cxTextEdit,
  cxLookAndFeelPainters,
  dxRichEdit.Utils.BackgroundThreadUIUpdater,
  dxRichEdit.Utils.Graphics,
  dxStringHelper,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.Api.MailMerge,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentLayout.Painters,
  dxRichEdit.DocumentModel.RichEditDocumentServer,
  dxRichEdit.DocumentModel.MailMerge,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.Cache,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Options.Core,
  dxRichEdit.Options,
  dxRichEdit.Inplace.Render,
  dxRichEdit.View.Simple;

type
  TdxRtfDocumentExporterCompatibilityOptionsAccess = class(TdxRtfDocumentExporterCompatibilityOptions);

  { TdxInplaceRichEditDocumentServer }

  TdxInplaceRichEditDocumentServer = class(TdxRichEditRenderDocumentServer)
  protected
    function GetMailMergeOptions(const AOptions: IdxRichEditMailMergeOptions): TdxMailMergeOptions; override;
  public
    function CreateMailMergeOptions: IdxRichEditMailMergeOptions; override;
  end;

  { TdxInplaceRichEditDocumentServer }

  TdxInplaceRichEditInternalDocumentServer = class(TdxRichEditRenderInternalDocumentServer)
  protected
    function CreateInnerServer(ADocumentModel: TdxDocumentModel): TdxInnerRichEditDocumentServer; override;
  end;

  { TdxInplaceRichEditParagraphFormattingInfoCache }

  TdxInplaceRichEditParagraphFormattingInfoCache = class(TdxParagraphFormattingInfoCache)
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxParagraphFormattingInfo; override;
  end;

  { TdxInplaceRichEditParagraphFormattingCache }

  TdxInplaceRichEditParagraphFormattingCache = class(TdxParagraphFormattingCache)
  protected
    function CreateDefaultParagraphFormattingInfo(ADocumentModel: TdxCustomDocumentModel): TdxParagraphFormattingInfo; override;
  end;

  { TdxInplaceRichEditDocumentCache }

  TdxInplaceRichEditDocumentCache = class(TdxDocumentCache)
  protected
    function CreateParagraphFormattingCache(ADocumentModel: TdxCustomDocumentModel): TdxParagraphFormattingCache; override;
    function CreateParagraphFormattingInfoCache(AUnitConverter: TdxDocumentModelUnitConverter): TdxParagraphFormattingInfoCache; override;
  end;

  { TdxInplaceRichEditDocumentModel }

  TdxInplaceRichEditDocumentModel = class(TdxDocumentModel)
  protected
    function CreateDocumentCache: TdxCustomDocumentCache; override;
    function GetDefaultLayoutUnit: TdxDocumentLayoutUnit; override;
  end;

{ TdxInplaceRichEditParagraphFormattingInfoCache }

function TdxInplaceRichEditParagraphFormattingInfoCache.CreateDefaultItem(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxParagraphFormattingInfo;
begin
  Result := CreateDefaultItemMSO2003(AUnitConverter);
end;

{ TdxInplaceRichEditParagraphFormattingCache }

function TdxInplaceRichEditParagraphFormattingCache.CreateDefaultParagraphFormattingInfo(
  ADocumentModel: TdxCustomDocumentModel): TdxParagraphFormattingInfo;
begin
  Result := TdxInplaceRichEditParagraphFormattingInfoCache.CreateDefaultItemMSO2003(ADocumentModel.UnitConverter);
end;

{ TdxInplaceRichEditDocumentCache }

function TdxInplaceRichEditDocumentCache.CreateParagraphFormattingCache(
  ADocumentModel: TdxCustomDocumentModel): TdxParagraphFormattingCache;
begin
  Result := TdxInplaceRichEditParagraphFormattingCache.Create(ADocumentModel);
end;

function TdxInplaceRichEditDocumentCache.CreateParagraphFormattingInfoCache(
  AUnitConverter: TdxDocumentModelUnitConverter): TdxParagraphFormattingInfoCache;
begin
  Result := TdxInplaceRichEditParagraphFormattingInfoCache.Create(AUnitConverter);
end;

{ TdxInplaceRichEditDocumentModel }

function TdxInplaceRichEditDocumentModel.CreateDocumentCache: TdxCustomDocumentCache;
begin
  Result := TdxInplaceRichEditDocumentCache.Create;
end;

function TdxInplaceRichEditDocumentModel.GetDefaultLayoutUnit: TdxDocumentLayoutUnit;
begin
  Result := TdxDocumentLayoutUnit.Pixel;
end;

{ TdxInplaceRichEditViewInfo }

procedure TdxInplaceRichEditViewInfo.InternalPaint(ACanvas: TcxCanvas);
begin
  inherited InternalPaint(ACanvas);
  TdxRichEditRender.Draw(ACanvas, DisplayValue, TdxCustomInplaceRichEditProperties(EditProperties),
    Font, TdxAlphaColors.FromColor(TextColor), FRenderBounds);
end;

{ TdxInplaceRichEditViewData }

function TdxInplaceRichEditViewData.InternalGetEditContentSize(ACanvas: TcxCanvas;
  const AEditValue: TcxEditValue;
  const AEditSizeProperties: TcxEditSizeProperties): TSize;
begin
  if VarIsSoftNull(AEditValue) then
    Result := GetTextEditContentSize(ACanvas, Self, '', 0, AEditSizeProperties, 1)
  else
    Result := TdxRichEditRender.CalculateSize(AEditValue, Properties, Style.Font, AEditSizeProperties.Width);
end;

procedure TdxInplaceRichEditViewData.Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
  Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
  AIsMouseEvent: Boolean);
begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo, AIsMouseEvent);
  TdxInplaceRichEditViewInfo(AViewInfo).FRenderBounds := AViewInfo.ClientRect;
end;

procedure TdxInplaceRichEditViewData.EditValueToDrawValue(
  const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
begin
  TdxInplaceRichEditViewInfo(AViewInfo).DisplayValue := AEditValue;
end;

function TdxInplaceRichEditViewData.GetProperties: TdxCustomInplaceRichEditProperties;
begin
  Result := TdxCustomInplaceRichEditProperties(inherited Properties);
end;

{ TdxCustomInplaceRichEditProperties }

constructor TdxCustomInplaceRichEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  AutoSelect := False;
  FWantReturns := True;
  FWantTabs := True;
end;

function TdxCustomInplaceRichEditProperties.CanCompareEditValue: Boolean;
begin
  Result := True;
end;

class function TdxCustomInplaceRichEditProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TdxCustomInplaceRichEdit;
end;

function TdxCustomInplaceRichEditProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := inherited GetSupportedOperations +
    [esoAutoHeight, esoEditingAutoHeight, esoEditing, esoHorzAlignment, esoIncSearch];
end;

class function TdxCustomInplaceRichEditProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TdxInplaceRichEditViewData;
end;

class function TdxCustomInplaceRichEditProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxInplaceRichEditViewInfo;
end;

procedure TdxCustomInplaceRichEditProperties.DoAssign(AProperties: TcxCustomEditProperties);
var
  ARichProperies: TdxCustomInplaceRichEditProperties;
begin
  inherited DoAssign(AProperties);
  if AProperties is TdxCustomInplaceRichEditProperties then
  begin
    ARichProperies := TdxCustomInplaceRichEditProperties(AProperties);
    WantReturns := ARichProperies.WantReturns;
    WantTabs := ARichProperies.WantTabs;
  end;
end;

function TdxCustomInplaceRichEditProperties.HasDisplayValue: Boolean;
begin
  Result := True;
end;

procedure TdxCustomInplaceRichEditProperties.SetWantReturns(Value: Boolean);
begin
  if WantReturns <> Value then
  begin
    FWantReturns := Value;
    Changed;
  end;
end;

procedure TdxCustomInplaceRichEditProperties.SetWantTabs(Value: Boolean);
begin
  if WantTabs <> Value then
  begin
    FWantTabs := Value;
    Changed;
  end;
end;

{ TdxCustomInplaceRichEdit }

constructor TdxCustomInplaceRichEdit.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TdxCustomInplaceRichEdit.AdjustInnerEdit;
var
  AFont: TFont;
begin
  InnerEdit.LockBounds(True);
  try
    TdxInnerInplaceRichEdit(InnerControl).ActiveView.BackColor := TdxAlphaColors.FromColor(ViewInfo.BackgroundColor);
    AFont := TFont.Create;
    try
      AFont.Assign(VisibleFont);
      AFont.Color := ViewInfo.TextColor;
      TdxInnerInplaceRichEdit(InnerControl).Font := AFont;
      TcxInplaceRichEditDocumentHelper.SetDefaultFont(TdxInnerInplaceRichEdit(InnerControl).Document, AFont);
    finally
      AFont.Free;
    end;
  finally
    InnerEdit.LockBounds(False);
  end;
end;

procedure TdxCustomInplaceRichEdit.AdjustInnerEditPosition;
var
  R: TRect;
begin
  if (InnerEdit = nil) or FInnerEditPositionAdjusting then
    Exit;
  FInnerEditPositionAdjusting := True;
  try
    R := ViewInfo.ClientRect;
    InnerEdit.Control.SetBounds(R.Left, R.Top, R.Width, R.Height);
    AlignControls(InnerEdit.Control, R);
  finally
    FInnerEditPositionAdjusting := False;
  end;
end;

function TdxCustomInplaceRichEdit.CanAutoSize: Boolean;
begin
  Result := False; // todo:
end;

function TdxCustomInplaceRichEdit.DoRefreshContainer(const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
  AIsMouseEvent: Boolean): Boolean;
begin
  Result := inherited DoRefreshContainer(P, Button, Shift, AIsMouseEvent);
  if Result then
    AdjustInnerEdit;
end;

procedure TdxCustomInplaceRichEdit.DoSetFocusWhenActivate;
begin
  Document.Selection := Document.CreateRange(0, 0);
  Document.CaretPosition := Document.CreatePosition(0);
  inherited DoSetFocusWhenActivate;
end;

function TdxCustomInplaceRichEdit.CanKeyDownModifyEdit(Key: Word;
  Shift: TShiftState): Boolean;
begin
  Result := inherited CanKeyDownModifyEdit(Key, Shift);
  Result := Result or TdxInnerInplaceRichEdit(InnerControl).CanKeyDownModifyEdit(Key, Shift);
end;

function TdxCustomInplaceRichEdit.CanKeyPressModifyEdit(Key: Char): Boolean;
begin
  Result := (Ord(Key) >= 32) or (Key = #8) or (Key = #22) or (Key = #24);
end;

procedure TdxCustomInplaceRichEdit.ChangeHandler(Sender: TObject);
begin
  inherited ChangeHandler(Sender);
end;

function TdxCustomInplaceRichEdit.GetDisplayValue: TcxEditValue;
begin
  Result := InnerEdit.EditValue;
end;

procedure TdxCustomInplaceRichEdit.PropertiesChanged(Sender: TObject);
var
  AInnerRichEdit: TdxInnerInplaceRichEdit;
begin
  if PropertiesChangeLocked then
    Exit;
  inherited PropertiesChanged(Sender);
  AInnerRichEdit := TdxInnerInplaceRichEdit(InnerControl);
  AInnerRichEdit.AutoSelect := ActiveProperties.AutoSelect;
  AInnerRichEdit.WantReturns := ActiveProperties.WantReturns;
  AInnerRichEdit.WantTabs := ActiveProperties.WantTabs;
end;

function TdxCustomInplaceRichEdit.GetEditingValue: Variant;
begin
  Result := EditValue;
end;

function TdxCustomInplaceRichEdit.GetEditValue: Variant;
begin
  Result := InnerEdit.EditValue;
end;

function TdxCustomInplaceRichEdit.GetInnerEditClass: TControlClass;
begin
  Result := TdxInnerInplaceRichEdit;
end;

class function TdxCustomInplaceRichEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxCustomInplaceRichEditProperties;
end;

function TdxCustomInplaceRichEdit.GetActiveProperties: TdxCustomInplaceRichEditProperties;
begin
  Result := TdxCustomInplaceRichEditProperties(inherited ActiveProperties);
end;

function TdxCustomInplaceRichEdit.GetDocument: IdxRichEditDocument;
begin
  if InnerControl = nil then
    Result := nil
  else
    Result := TdxInnerInplaceRichEdit(InnerControl).Document;
end;

function TdxCustomInplaceRichEdit.GetDocumentModel: TdxDocumentModel;
begin
  if InnerControl = nil then
    Result := nil
  else
    Result := TdxInnerInplaceRichEdit(InnerControl).DocumentModel;
end;

function TdxCustomInplaceRichEdit.GetViewInfo: TdxInplaceRichEditViewInfo;
begin
  Result := TdxInplaceRichEditViewInfo(inherited ViewInfo);
end;

procedure TdxCustomInplaceRichEdit.Initialize;
begin
  inherited Initialize;
  Width := 185;
  Height := 89;
end;

procedure TdxCustomInplaceRichEdit.InternalSetEditValue(const Value: Variant;
  AValidateEditValue: Boolean);
begin
  InnerEdit.EditValue := Value;
end;

procedure TdxCustomInplaceRichEdit.InternalValidateDisplayValue(
  const ADisplayValue: Variant);
begin
// do nothing
end;

function TdxCustomInplaceRichEdit.IsEditClass: Boolean;
begin
  Result := True;
end;

procedure TdxCustomInplaceRichEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if IsInplace and (Key = VK_RETURN) and ((ssCtrl in Shift) and not ActiveProperties.WantReturns or
      not (ssCtrl in Shift) and ActiveProperties.WantReturns) then
    DoEditKeyDown(Key, Shift)
  else
    inherited KeyDown(Key, Shift);
end;

procedure TdxCustomInplaceRichEdit.SelectAll;
begin
  inherited SelectAll;
  Document.SelectAll;
end;

procedure TdxCustomInplaceRichEdit.SynchronizeDisplayValue;
begin
// do nothing
end;

procedure TdxCustomInplaceRichEdit.SynchronizeEditValue;
begin
// do nothing
end;

procedure TdxCustomInplaceRichEdit.UpdateDrawValue;
begin
  ViewInfo.DisplayValue := DisplayValue;
end;

function TdxCustomInplaceRichEdit.WantNavigationKeys: Boolean;
begin
  Result := True;
end;

{ TdxInplaceRichEditDocumentServer }

function TdxInplaceRichEditDocumentServer.GetMailMergeOptions(const AOptions: IdxRichEditMailMergeOptions): TdxMailMergeOptions;
begin
  Result := TdxNativeMailMergeOptions(AOptions).GetInternalMailMergeOptions;
end;

function TdxInplaceRichEditDocumentServer.CreateMailMergeOptions: IdxRichEditMailMergeOptions;
begin
  Result := TdxNativeMailMergeOptions.Create;
end;

{ TdxInplaceRichEditInternalDocumentServer }

function TdxInplaceRichEditInternalDocumentServer.CreateInnerServer(ADocumentModel: TdxDocumentModel): TdxInnerRichEditDocumentServer;
begin
  if ADocumentModel = nil then
    Result := TdxInplaceRichEditDocumentServer.Create(Self)
  else
    Result := TdxInplaceRichEditDocumentServer.Create(Self, ADocumentModel);
end;

{ TdxInnerInplaceRichEdit }

constructor TdxInnerInplaceRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsEditValueNull := True;
  FHelper := TcxInplaceRichEditHelper.Create(Self);
  BorderStyle := cxcbsNone;
  InitializeOptions;
end;

destructor TdxInnerInplaceRichEdit.Destroy;
begin
  FreeAndNil(FHelper);
  inherited Destroy;
end;

function TdxInnerInplaceRichEdit.GetEditValue: Variant;
begin
  if FIsEditValueNull then
    Result := Null
  else
    Result := TcxInplaceRichEditDocumentHelper.GetEditValue(Document);
end;

procedure TdxInnerInplaceRichEdit.SetEditValue(const Value: Variant);
begin
  TcxInplaceRichEditDocumentHelper.SetEditValue(InnerControl.DocumentModel, Document, Value);
  FIsEditValueNull := VarIsNull(Value);
end;

function TdxInnerInplaceRichEdit.CalculateVerticalScrollbarVisibility: Boolean;
begin
  if Options.VerticalScrollbar.Visibility = TdxRichEditScrollbarVisibility.Auto then
    Result := TdxInplaceView(ActiveView).CalculateVerticalScrollbarAutoVisibility
  else
    Result := inherited CalculateVerticalScrollbarVisibility;
end;

function TdxInnerInplaceRichEdit.CreateInnerControl: TdxInnerRichEditControl;
begin
  Result := TdxInplaceRichEditInnerControl.Create(Self);
end;

function TdxInnerInplaceRichEdit.CreateViewRepository: TdxRichEditCustomViewRepository;
begin
  Result := TdxInplaceRichEditViewRepository.Create(Self);
end;

procedure TdxInnerInplaceRichEdit.DoChanged;
begin
  FIsEditValueNull := False;
  dxCallNotify(FOnChanged, Self);
end;

function TdxInnerInplaceRichEdit.GetContainer: TdxCustomInplaceRichEdit;
begin
  if Parent is TdxCustomInplaceRichEdit then
    Result := TdxCustomInplaceRichEdit(Parent)
  else
    Result := nil;
end;

function TdxInnerInplaceRichEdit.GetHelper: IcxCustomInnerEdit;
begin
  Result := Helper;
end;

procedure TdxInnerInplaceRichEdit.InitializeOptions;
begin
  Options.BeginUpdate;
  try
    Options.DocumentCapabilities.CharacterStyle := TdxDocumentCapability.Hidden;
    Options.DocumentCapabilities.ParagraphStyle := TdxDocumentCapability.Hidden;
    Options.DocumentCapabilities.TableStyle := TdxDocumentCapability.Hidden;
    Options.DocumentCapabilities.Bookmarks := TdxDocumentCapability.Hidden;
    Options.DocumentCapabilities.Hyperlinks := TdxDocumentCapability.Hidden;
    Options.Behavior.Zooming := TdxDocumentCapability.Disabled;
    Options.Export.Rtf.Compatibility.DuplicateObjectAsMetafile := True;
  finally
    Options.CancelUpdate;
  end;
end;

procedure TdxInnerInplaceRichEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FInternalUpdating := False;
  _TcxContainerAccess.KeyDown(Container, Key, Shift);
  if Key = 0 then
    FInternalUpdating := True
  else
    inherited KeyDown(Key, Shift);
end;

procedure TdxInnerInplaceRichEdit.KeyPress(var Key: Char);
begin
  _TcxContainerAccess.KeyPress(Container, Key);
  if Key = #0 then
    FInternalUpdating := True
  else
    inherited KeyPress(Key);
end;

procedure TdxInnerInplaceRichEdit.KeyUp(var Key: Word; Shift: TShiftState);
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

procedure TdxInnerInplaceRichEdit.RaiseDeferredEvents(
  AChangeActions: TdxDocumentModelChangeActions);
begin
  if TdxDocumentModelChangeAction.RaiseContentChanged in AChangeActions then
    DoChanged;
  inherited RaiseDeferredEvents(AChangeActions);
end;

procedure TdxInnerInplaceRichEdit.SafelySetFocus;
var
  APrevAutoSelect: Boolean;
begin
  APrevAutoSelect := AutoSelect;
  AutoSelect := False;
  try
    SetFocus;
  finally
    AutoSelect := APrevAutoSelect;
  end;
end;

procedure TdxInnerInplaceRichEdit.WMClear(var Message: TMessage);
begin
  inherited;
end;

procedure TdxInnerInplaceRichEdit.WMGetText(var Message: TMessage);
begin
  inherited;
end;

procedure TdxInnerInplaceRichEdit.WMGetTextLength(
  var Message: TWMGetTextLength);
begin
  inherited;
end;

procedure TdxInnerInplaceRichEdit.WMSetFont(var Message: TWMSetFont);
begin
  inherited;
  TcxInplaceRichEditDocumentHelper.SetDefaultFont(Document, Font);
end;

function TdxInnerInplaceRichEdit.CanKeyDownModifyEdit(Key: Word;
  Shift: TShiftState): Boolean;
begin
  Result := KeyboardController.CanKeyDownModifyEdit(Key, Shift);
end;

function TdxInnerInplaceRichEdit.CreateDocumentServer(ADocumentModel: TdxDocumentModel): IdxRichEditDocumentContainer;
begin
  Result := TdxInplaceRichEditInternalDocumentServer.Create(nil);
end;

function TdxInnerInplaceRichEdit.CreateOptions(const ADocumentServer: TObject{TdxInnerRichEditDocumentServer}): TObject{TdxRichEditControlOptionsBase};
begin
  Result := TdxInplaceRichEditOptions.Create(TdxInnerRichEditDocumentServer(ADocumentServer));
end;

function TdxInnerInplaceRichEdit.CreateHorizontalRuler: IdxRulerControl;
begin
  Result := nil;
end;

function TdxInnerInplaceRichEdit.CreateVerticalRuler: IdxRulerControl;
begin
  Result := nil;
end;

procedure TdxInnerInplaceRichEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  TcxInplaceRichEditDocumentHelper.SetDefaultFont(Document, Font);
end;

procedure TdxInnerInplaceRichEdit.WMSetText(var Message: TWMSetText);
begin
  inherited;
end;

{ TcxInplaceRichEditDocumentHelper }

class procedure TcxInplaceRichEditDocumentHelper.SetEditValue(ADocumentModel: TdxDocumentModel;
  const ADocument: IdxRichEditDocument; const Value: Variant);
var
  S: string;
begin
  if VarIsNull(Value) then
    ADocument.Text := ''
  else
  begin
    S := dxVariantToString(Value);
    if TdxStringHelper.StartsWith(S, '{\rtf') then
      ADocument.RtfText := S
    else
      ADocument.Text := S;
  end;
end;

class procedure TcxInplaceRichEditDocumentHelper.SynchronizePropertiesAndOptions(
  ASource: TdxCustomInplaceRichEditProperties;
  ADestination: TdxRichEditControlCustomOptions);
begin
// todo:
end;

class function TcxInplaceRichEditDocumentHelper.GetEditValue(const ADocument: IdxRichEditDocument): Variant;
begin
  Result := ADocument.RtfText;
end;

class procedure TcxInplaceRichEditDocumentHelper.SetDefaultFont(
  const ADocument: IdxRichEditDocument; const AFont: TFont);
begin
  TcxInplaceRichEditDocumentHelper.SetDefaultFont(ADocument, AFont, TdxAlphaColors.FromColor(AFont.Color));
end;

class procedure TcxInplaceRichEditDocumentHelper.SetDefaultFont(const ADocument: IdxRichEditDocument;
  const AFont: TFont; AForeColor: TdxAlphaColor);
begin
  ADocument.BeginUpdate;
  try
    ADocument.DefaultCharacterProperties.FontName := AFont.Name;
    ADocument.DefaultCharacterProperties.FontSize := Abs(AFont.Size);
    ADocument.DefaultCharacterProperties.ForeColor := AForeColor;
    ADocument.DefaultCharacterProperties.Bold := fsBold in AFont.Style;
    if fsUnderline in AFont.Style then
      ADocument.DefaultCharacterProperties.Underline := TdxRichEditUnderlineType.Single;
    ADocument.DefaultCharacterProperties.Italic := fsItalic in AFont.Style;
    if fsStrikeout in AFont.Style then
      ADocument.DefaultCharacterProperties.Strikeout := TdxRichEditStrikeoutType.Single;
  finally
    ADocument.EndUpdate;
  end;
end;

{ TcxInplaceRichEditHelper }

constructor TcxInplaceRichEditHelper.Create(AEdit: TdxInnerInplaceRichEdit);
begin
  inherited Create(nil);
  FEdit := AEdit;
end;

function TcxInplaceRichEditHelper.CallDefWndProc(AMsg: UINT; WParam: WPARAM;
  LParam: LPARAM): LRESULT;
begin
  Result := CallWindowProc(Edit.DefWndProc, Edit.Handle, AMsg, WParam, LParam);
end;

function TcxInplaceRichEditHelper.CanProcessClipboardMessages: Boolean;
begin
  Result := True;
end;

function TcxInplaceRichEditHelper.GetControl: TWinControl;
begin
  Result := FEdit;
end;

function TcxInplaceRichEditHelper.GetControlContainer: TcxContainer;
begin
  Result := Edit.Container;
end;

function TcxInplaceRichEditHelper.GetEditValue: TcxEditValue;
begin
  Result := Edit.GetEditValue;
end;

function TcxInplaceRichEditHelper.GetOnChange: TNotifyEvent;
begin
  Result := FEdit.OnChanged;
end;

function TcxInplaceRichEditHelper.GetReadOnly: Boolean;
begin
  Result := Edit.ReadOnly;
end;

procedure TcxInplaceRichEditHelper.LockBounds(ALock: Boolean);
begin

end;

procedure TcxInplaceRichEditHelper.SafelySetFocus;
begin
  Edit.SafelySetFocus;
end;

procedure TcxInplaceRichEditHelper.SetEditValue(const Value: TcxEditValue);
begin
  Edit.SetEditValue(Value);
end;

procedure TcxInplaceRichEditHelper.SetOnChange(Value: TNotifyEvent);
begin
  FEdit.OnChanged := Value;
end;

procedure TcxInplaceRichEditHelper.SetReadOnly(Value: Boolean);
begin
  FEdit.ReadOnly := Value;
end;

procedure TcxInplaceRichEditHelper.SetParent(Value: TWinControl);
begin
  FEdit.Parent := Value;
end;

{ TdxInplaceRichEditInnerControl }

procedure TdxInplaceRichEditInnerControl.BeginInitialize;
begin
  inherited BeginInitialize;
  TdxRtfDocumentExporterCompatibilityOptionsAccess(Options.Export.Rtf.Compatibility).Kerning := True;
end;

function TdxInplaceRichEditInnerControl.CreateDocumentModelCore: TdxDocumentModel;
begin
  Result := TdxInplaceRichEditDocumentModel.Create;
end;

function TdxInplaceRichEditInnerControl.CreateKeyboardController: TdxCustomKeyboardController;
begin
  Result := TdxInplaceRichEditKeyboardController.Create(Self);
end;

{ TdxInplaceRichEditKeyboardDefaultHandler }

procedure TdxInplaceRichEditKeyboardDefaultHandler.PopulateDialogsCommandTable;
begin
end;

procedure TdxInplaceRichEditKeyboardDefaultHandler.PopulatePagesCommandTable;
begin
end;

{ TdxInplaceRichEditKeyboardController }

function TdxInplaceRichEditKeyboardController.CreateDefaultHandler: IdxKeyboardHandlerService;
begin
  Result := TdxInplaceRichEditKeyboardDefaultHandler.Create(Self);
end;

procedure Initialize;
begin
end;

procedure Finalize;
begin
end;

initialization
   dxUnitsLoader.AddUnit(@Initialize, @Finalize);

finalization
  dxUnitsLoader.RemoveUnit(@Finalize);

end.
