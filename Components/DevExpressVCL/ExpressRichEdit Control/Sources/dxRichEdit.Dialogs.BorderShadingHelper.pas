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

unit dxRichEdit.Dialogs.BorderShadingHelper;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, Menus, Buttons,
  Generics.Defaults, Generics.Collections, cxGraphics, cxControls, cxButtons, cxImageComboBox, cxClasses, dxCoreGraphics,

  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.BorderShadingController,
  dxRichEdit.DocumentLayout,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.PatternLine,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Platform.Win.Painter,
  dxRichEdit.Control;

type
  TdxBorderLineState = (
    Known,
    Unknown,
    No
  );

  TdxTableCellBorderLine = (
    Up,
    Down,
    HorizontalIn,
    Left,
    Right,
    VerticalIn
  );

  TdxBorderShadingUserControlHelper = class;

  { TdxPreviewBorderViewInfo }

  TdxPreviewBorderViewInfo = class(TdxTableBorderViewInfoBase)
  private
    FEndCorner: TdxCornerViewInfoBase;
    FBorderType: TdxBorderTypes;
    FBorder: TdxBorderInfo;
    FStartCorner: TdxCornerViewInfoBase;
  protected
    function GetBorder: TdxBorderInfo; override;
    function GetBorderType: TdxBorderTypes; override;
    function GetEndCorner: TdxCornerViewInfoBase; override;
    function GetStartCorner: TdxCornerViewInfoBase; override;
    function GetConverter: TdxDocumentModelUnitToLayoutUnitConverter; override;
  public
    function GetBounds(ATableViewInfo: TdxTableViewInfo): TRect; override;
    property Border: TdxBorderInfo read GetBorder write FBorder;
    property BorderType: TdxBorderTypes read GetBorderType write FBorderType;
    property StartCorner: TdxCornerViewInfoBase read GetStartCorner write FStartCorner;
    property EndCorner: TdxCornerViewInfoBase read GetEndCorner write FEndCorner;
  end;

  { TdxPreviewBorderShadingControlHelper }

  TdxPreviewBorderShadingControlHelper = class
  private const
    BrushesLightGray = clWebLightgrey;
    ColorLightGray = clWebLightgrey;
  private
    FDocumentModel: TdxDocumentModel;
    FFillColor: TdxAlphaColor;
    FBorderLineStates: array[TdxTableCellBorderLine] of TdxBorderLineState;
    FDrawColumns: Boolean;
    FDrawParagraph: Boolean;
    FDrawPageBorderHorizontalDown: Boolean;
    FDrawPageBorderHorizontalUp: Boolean;
    FDrawPageBorderVerticalRight: Boolean;
    FDrawPageBorderVerticalLeft: Boolean;
    FOnChange: TNotifyEvent;
    FHeight: Integer;
    FWidth: Integer;
    FBorderInfoSource: TdxBorderShadingUserControlHelper;
    FControl: TdxCustomRichEditControl;
    procedure SetFillColor(const Value: TdxAlphaColor);
    function GetBorderLineState(const Index: TdxTableCellBorderLine): TdxBorderLineState; inline;
    procedure SetBorderLineState(const Index: TdxTableCellBorderLine; const Value: TdxBorderLineState);
    procedure SetDrawColumns(const Value: Boolean);
    procedure SetDrawParagraph(const Value: Boolean);
    procedure SetDrawPageBorderHorizontalDown(const Value: Boolean);
    procedure SetDrawPageBorderHorizontalUp(const Value: Boolean);
    procedure SetDrawPageBorderVerticalRight(const Value: Boolean);
    procedure SetDrawPageBorderVerticalLeft(const Value: Boolean);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetBounds(const Value: TRect);
    function GetBounds: TRect;
  protected
    procedure DoChange;
    procedure DrawRectangles(AGraphics: TdxGraphics; AWidth, AHeight, AX, AY: Integer);
  public
    procedure Draw(ACanvas: TcxCanvas);
    property DocumentModel: TdxDocumentModel read FDocumentModel write FDocumentModel;
    property DrawColumns: Boolean read FDrawColumns write SetDrawColumns;
    property DrawParagraph: Boolean read FDrawParagraph write SetDrawParagraph;
    property DrawPageBorderHorizontalDown: Boolean read FDrawPageBorderHorizontalDown write SetDrawPageBorderHorizontalDown;
    property DrawPageBorderHorizontalUp: Boolean read FDrawPageBorderHorizontalUp write SetDrawPageBorderHorizontalUp;
    property DrawPageBorderVerticalRight: Boolean read FDrawPageBorderVerticalRight write SetDrawPageBorderVerticalRight;
    property DrawPageBorderVerticalLeft: Boolean read FDrawPageBorderVerticalLeft write SetDrawPageBorderVerticalLeft;
    property FillColor: TdxAlphaColor read FFillColor write SetFillColor;
    property HorizontalLineUp: TdxBorderLineState index TdxTableCellBorderLine.Up read GetBorderLineState write SetBorderLineState;
    property HorizontalLineDown: TdxBorderLineState index TdxTableCellBorderLine.Down read GetBorderLineState write SetBorderLineState;
    property HorizontalLineIn: TdxBorderLineState index TdxTableCellBorderLine.HorizontalIn read GetBorderLineState write SetBorderLineState;
    property VerticalLineRight: TdxBorderLineState index TdxTableCellBorderLine.Right read GetBorderLineState write SetBorderLineState;
    property VerticalLineLeft: TdxBorderLineState index TdxTableCellBorderLine.Left read GetBorderLineState write SetBorderLineState;
    property VerticalLineIn: TdxBorderLineState index TdxTableCellBorderLine.VerticalIn read GetBorderLineState write SetBorderLineState;

    property Bounds: TRect read GetBounds write SetBounds;
    property BorderInfoSource: TdxBorderShadingUserControlHelper read FBorderInfoSource write FBorderInfoSource;
    property Control: TdxCustomRichEditControl read FControl write FControl;
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TdxBorderShadingUserControlHelper }

  TdxBorderShadingUserControlHelper = class
  private type
    TdxTableCellBorderLineInfos = array[TdxTableCellBorderLine] of TdxBorderInfo;
    TdxTableCellBorderLineBtns = array[TdxTableCellBorderLine] of TcxButton;
    TdxTableCellBorderLineBtnOnChanges = array[TdxTableCellBorderLine] of TNotifyEvent;
  private
    FBorderLines: TdxTableCellBorderLineInfos;
    FBorderLineBtns: TdxTableCellBorderLineBtns;
    FBorderLineBtnOnChanges: TdxTableCellBorderLineBtnOnChanges;
    FPreviewBSUserControl: TdxPreviewBorderShadingControlHelper;
    FUpdateCount: Integer;
    FSetMode: TdxSetMode;
    FButtonsVisible: Boolean;
    FCurrentBorderInfo: TdxBorderInfo;
    FInactiveBorderInfo: TdxBorderInfo;
    FBorderLineHorizontalInVisible: Boolean;
    FBorderLineVerticalInVisible: Boolean;
    procedure btnHorizontBorderUpChecked(Sender: TObject);
    procedure btnHorizontBorderInChecked(Sender: TObject);
    procedure btnHorizontBorderDownChecked(Sender: TObject);
    procedure btnVerticalBorderLeftChecked(Sender: TObject);
    procedure btnVerticalBorderInChecked(Sender: TObject);
    procedure btnVerticalBorderRightChecked(Sender: TObject);
    function GetBorderLineInfo(const Index: TdxTableCellBorderLine): TdxBorderInfo;
    procedure SetBorderLineInfo(const Index: TdxTableCellBorderLine; const Value: TdxBorderInfo);
    procedure SetButtonsVisible(const Value: Boolean);
    procedure SetCurrentBorderInfo(const Value: TdxBorderInfo);
    procedure SetBorderLineHorizontalInVisible(const Value: Boolean);
    procedure SetBorderLineVerticalInVisible(const Value: Boolean);
    function GetInsideSetProperties: Boolean; inline;
    function GetOnBorderLineChanged(const Index: TdxTableCellBorderLine): TNotifyEvent; inline;
    procedure SetOnBorderLineChanged(const Index: TdxTableCellBorderLine; const Value: TNotifyEvent); inline;
    function GetDocumentModel: TdxDocumentModel;
    procedure SetDocumentModel(const Value: TdxDocumentModel);
    function GetFillColor: TdxAlphaColor;
    procedure SetFillColor(const Value: TdxAlphaColor);
    function GetDrawColumns: Boolean;
    procedure SetDrawColumns(const Value: Boolean);
    function GetDrawParagraph: Boolean;
    procedure SetDrawParagraph(const Value: Boolean);
    procedure SetBtnBorderLineVisible(const Index: TdxTableCellBorderLine; const Value: Boolean);
  protected
    procedure BtnsVisibleChanged;
    procedure DoButtonBorderLineChecked(AIndex: TdxTableCellBorderLine);
    procedure SetAllModeCore;
    procedure SetCustomModeCore;
    procedure SetGridModeCore;
    procedure SetNoneModeCore;
    procedure SetBoxModeCore;
    procedure SubscribeEvents;
    property InsideSetProperties: Boolean read GetInsideSetProperties;
  public
    constructor Create(APreviewBorderShadingControl: TdxPreviewBorderShadingControlHelper);
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Initialize(ABtnLineUp, ABtnLineDown, ABtnLineHorizontalIn, ABtnLineLeft, ABtnLineRight, ABtnLineVerticalIn: TcxButton);
    procedure SetMode(const Value: TdxSetMode);
    class procedure SetButtonDown(AButton: TcxButton; ADown: Boolean);

    property BorderLineUp: TdxBorderInfo index TdxTableCellBorderLine.Up read GetBorderLineInfo write SetBorderLineInfo;
    property BorderLineDown: TdxBorderInfo index TdxTableCellBorderLine.Down read GetBorderLineInfo write SetBorderLineInfo;
    property BorderLineHorizontalIn: TdxBorderInfo index TdxTableCellBorderLine.HorizontalIn read GetBorderLineInfo write SetBorderLineInfo;
    property BorderLineLeft: TdxBorderInfo index TdxTableCellBorderLine.Left read GetBorderLineInfo write SetBorderLineInfo;
    property BorderLineRight: TdxBorderInfo index TdxTableCellBorderLine.Right read GetBorderLineInfo write SetBorderLineInfo;
    property BorderLineVerticalIn: TdxBorderInfo index TdxTableCellBorderLine.VerticalIn read GetBorderLineInfo write SetBorderLineInfo;
    property BorderLineHorizontalInVisible: Boolean read FBorderLineHorizontalInVisible write SetBorderLineHorizontalInVisible;
    property BorderLineVerticalInVisible: Boolean read FBorderLineVerticalInVisible write SetBorderLineVerticalInVisible;
    property ButtonsVisible: Boolean read FButtonsVisible write SetButtonsVisible;
    property CurrentBorderInfo: TdxBorderInfo read FCurrentBorderInfo write SetCurrentBorderInfo;
    property DocumentModel: TdxDocumentModel read GetDocumentModel write SetDocumentModel;
    property DrawColumns: Boolean read GetDrawColumns write SetDrawColumns;
    property DrawParagraph: Boolean read GetDrawParagraph write SetDrawParagraph;
    property FillColor: TdxAlphaColor read GetFillColor write SetFillColor;
    property Mode: TdxSetMode read FSetMode write FSetMode;

    property OnBorderLineUpChanged: TNotifyEvent index TdxTableCellBorderLine.Up read GetOnBorderLineChanged write SetOnBorderLineChanged;
    property OnBorderLineDownChanged: TNotifyEvent index TdxTableCellBorderLine.Down read GetOnBorderLineChanged write SetOnBorderLineChanged;
    property OnBorderLineHorizontalInChanged: TNotifyEvent index TdxTableCellBorderLine.HorizontalIn read GetOnBorderLineChanged write SetOnBorderLineChanged;
    property OnBorderLineVerticalInChanged: TNotifyEvent index TdxTableCellBorderLine.VerticalIn read GetOnBorderLineChanged write SetOnBorderLineChanged;
    property OnBorderLineLeftChanged: TNotifyEvent index TdxTableCellBorderLine.Left read GetOnBorderLineChanged write SetOnBorderLineChanged;
    property OnBorderLineRightChanged: TNotifyEvent index TdxTableCellBorderLine.Right read GetOnBorderLineChanged write SetOnBorderLineChanged;
  end;

  { TdxRichEditBorderShadingDialogHelper }

  TdxRichEditBorderShadingDialogHelper = class
  public const
    BordersLineWeights: array[0..8] of Single = (0.25, 0.5, 0.75, 1.0, 1.5, 2.25, 3.0, 4.5, 6.0);
  private
    FBorderInfos: TdxBorderInfoList;
    FBorder: TdxBorderInfo;
    FControl: TdxCustomRichEditControl;
    FColor: TdxAlphaColor;
    FWidth: Integer;
    function GetUnitConverter: TdxDocumentModelUnitConverter; inline;
    function GetDocumentModel: TdxDocumentModel; inline;
    procedure SetBorder(const Value: TdxBorderInfo);
    procedure SetColor(const Value: TdxAlphaColor);
    procedure SetWidth(const Value: Integer);
    procedure PopulateBorderInfos;
    procedure UpdateBorderInfos;
  protected
    function GetActualBorderWidth(ABorder: TdxBorderInfo; DocumentModel: TdxDocumentModel): Integer;
    class function GetBorderLineStyleTypeName(ABorderLineStyle: TdxBorderLineStyle): string;
    function ShouldSkipBorderStyle(AStyle: TdxBorderLineStyle; ASkipNone: Boolean): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function DrawVerticalLineRight(ABorderInfoSource: TdxBorderShadingUserControlHelper; ADocumentModel: TdxDocumentModel;
      AToLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter; AExporter: TdxGraphicsDocumentLayoutExporterTableBorder;
      AWidth, AHeight: Integer): Integer;
    function DrawVerticalLineLeft(ABorderInfoSource: TdxBorderShadingUserControlHelper; ADocumentModel: TdxDocumentModel;
      AToLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter; AExporter: TdxGraphicsDocumentLayoutExporterTableBorder;
      AWidth, AHeight: Integer): Integer;
    function DrawVerticalLineIn(ABorderInfoSource: TdxBorderShadingUserControlHelper; ADocumentModel: TdxDocumentModel;
      AToLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter; AExporter: TdxGraphicsDocumentLayoutExporterTableBorder;
      AWidth, AHeight: Integer): Integer;
    function DrawHorizontalLineIn(ABorderInfoSource: TdxBorderShadingUserControlHelper; ADocumentModel: TdxDocumentModel;
      AToLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter; AExporter: TdxGraphicsDocumentLayoutExporterTableBorder;
      AWidth, AHeight: Integer): Integer;
    function DrawHorizontalLineUp(ABorderInfoSource: TdxBorderShadingUserControlHelper; ADocumentModel: TdxDocumentModel;
      AToLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter; AExporter: TdxGraphicsDocumentLayoutExporterTableBorder;
      AWidth, AHeight: Integer): Integer;
    function DrawHorizontalLineDown(ABorderInfoSource: TdxBorderShadingUserControlHelper; ADocumentModel: TdxDocumentModel;
      AToLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter; AExporter: TdxGraphicsDocumentLayoutExporterTableBorder;
      AWidth, AHeight: Integer): Integer;

    procedure PopulateBorderLineStyle(AImageList: TcxImageList; AStrings: TStrings; ASkipNone: Boolean = True);

    property Border: TdxBorderInfo read FBorder write SetBorder;
    property Color: TdxAlphaColor read FColor write SetColor;
    property Width: Integer read FWidth write SetWidth;
    property Control: TdxCustomRichEditControl read FControl write FControl;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property UnitConverter: TdxDocumentModelUnitConverter read GetUnitConverter;
  end;

implementation

uses
  Contnrs, Math, TypInfo,
  dxCore,
  cxGeometry,
  dxTypeHelpers,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentLayout.UnitPixelsConverter,
  dxRichEdit.Control.Core,
  dxRichEdit.Platform.PatternLinePainter,
  dxRichEdit.Dialogs.UnderlinePainter,
  dxRichEditBorderLineWeightComboBox;

{ TdxRichEditBorderShadingDialogHelper }

constructor TdxRichEditBorderShadingDialogHelper.Create;
begin
  inherited Create;
  FBorder := TdxBorderInfo.Create;
  FBorderInfos := TdxBorderInfoList.Create(True);
end;

destructor TdxRichEditBorderShadingDialogHelper.Destroy;
begin
  FBorder.Free;
  FBorderInfos.Free;
  inherited Destroy;
end;

function TdxRichEditBorderShadingDialogHelper.DrawHorizontalLineDown(
  ABorderInfoSource: TdxBorderShadingUserControlHelper; ADocumentModel: TdxDocumentModel;
  AToLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  AExporter: TdxGraphicsDocumentLayoutExporterTableBorder; AWidth, AHeight: Integer): Integer;
var
  AViewInfo, AViewInfoRight: TdxPreviewBorderViewInfo;
  ABorderWidth: Integer;
  ARightCorner, ALeftCorner, AMiddleCorner: TdxCornerViewInfoBase;
begin
  AViewInfo := TdxPreviewBorderViewInfo.Create;
  try
    AViewInfo.Border := ABorderInfoSource.BorderLineDown;
    AViewInfo.BorderType := TdxBorderTypes.Bottom;
    ABorderWidth := GetActualBorderWidth(AViewInfo.Border, ADocumentModel);
    ARightCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.InnerBottomRight, AToLayoutUnitConverter,
      ABorderInfoSource.BorderLineDown, ABorderInfoSource.BorderLineRight, nil, nil, 0);
    try
      ALeftCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.InnerBottomLeft, AToLayoutUnitConverter, nil,
        ABorderInfoSource.BorderLineLeft, ABorderInfoSource.BorderLineDown, nil, 0);
      try
        AMiddleCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.InnerBottomMiddle, AToLayoutUnitConverter,
          ABorderInfoSource.BorderLineDown, ABorderInfoSource.BorderLineVerticalIn, ABorderInfoSource.BorderLineDown, nil, 0);
        try
          AViewInfo.StartCorner := ALeftCorner;
          AViewInfo.EndCorner := AMiddleCorner;
          AExporter.ExportTableBorderCorner(ALeftCorner, 0, AHeight - ABorderWidth);
          AExporter.ExportTableBorderCorner(AMiddleCorner, (AWidth - ABorderWidth) div 2, AHeight - ABorderWidth);
          AExporter.ExportTableBorder(AViewInfo.Border, TRect.Create(0, 0, (AWidth - ABorderWidth) div 2, AHeight - ABorderWidth),
            AToLayoutUnitConverter, AViewInfo);
          AViewInfoRight := TdxPreviewBorderViewInfo.Create;
          try
            AViewInfoRight.Border := ABorderInfoSource.BorderLineDown;
            AViewInfoRight.BorderType := TdxBorderTypes.Bottom;
            AViewInfoRight.StartCorner := AMiddleCorner;
            AViewInfoRight.EndCorner := ARightCorner;
            AExporter.ExportTableBorderCorner(AMiddleCorner, (AWidth - ABorderWidth) div 2, AHeight - ABorderWidth);
            AExporter.ExportTableBorderCorner(ARightCorner, AWidth - ABorderWidth, AHeight - ABorderWidth);
            AExporter.ExportTableBorder(AViewInfoRight.Border, TRect.CreateSize((AWidth - ABorderWidth) div 2, 0,
              (AWidth - ABorderWidth) div 2, AHeight - ABorderWidth), AToLayoutUnitConverter, AViewInfoRight);
            Result := ABorderWidth;
          finally
            AViewInfoRight.Free;
          end;
        finally
          AMiddleCorner.Free;
        end;
      finally
        ALeftCorner.Free;
      end;
    finally
      ARightCorner.Free;
    end;
  finally
    AViewInfo.Free;
  end;
end;

function TdxRichEditBorderShadingDialogHelper.DrawHorizontalLineIn(ABorderInfoSource: TdxBorderShadingUserControlHelper;
  ADocumentModel: TdxDocumentModel; AToLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  AExporter: TdxGraphicsDocumentLayoutExporterTableBorder; AWidth, AHeight: Integer): Integer;
var
  AViewInfoLeft, AViewInfoRight: TdxPreviewBorderViewInfo;
  ABorderWidth: Integer;
  ARightCorner, ALeftCorner, AMiddleCorner: TdxCornerViewInfoBase;
begin
  AViewInfoLeft := TdxPreviewBorderViewInfo.Create;
  try
    AViewInfoLeft.Border := ABorderInfoSource.BorderLineHorizontalIn;
    AViewInfoLeft.BorderType := TdxBorderTypes.Bottom;
    ABorderWidth := GetActualBorderWidth(AViewInfoLeft.Border, ADocumentModel);
    ALeftCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.InnerLeftMiddle, AToLayoutUnitConverter, nil,
      ABorderInfoSource.BorderLineLeft, ABorderInfoSource.BorderLineHorizontalIn, ABorderInfoSource.BorderLineLeft, 0);
    try
      ARightCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.InnerRightMiddle, AToLayoutUnitConverter,
        ABorderInfoSource.BorderLineHorizontalIn, ABorderInfoSource.BorderLineRight, nil, ABorderInfoSource.BorderLineRight, 0);
      try
        AMiddleCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.InnerNormal, AToLayoutUnitConverter,
          ABorderInfoSource.BorderLineHorizontalIn, ABorderInfoSource.BorderLineVerticalIn,
          ABorderInfoSource.BorderLineHorizontalIn, ABorderInfoSource.BorderLineVerticalIn, 0);
        try
          AViewInfoLeft.StartCorner := ALeftCorner;
          AViewInfoLeft.EndCorner := AMiddleCorner;
          AExporter.ExportTableBorderCorner(ALeftCorner, 0, (AHeight - ABorderWidth) div 2);
          AExporter.ExportTableBorderCorner(AMiddleCorner, (AWidth - ABorderWidth) div 2, (AHeight - ABorderWidth) div 2);
          AExporter.ExportTableBorder(AViewInfoLeft.Border, TRect.Create(0, 0, (AWidth - ABorderWidth) div 2,
            (AHeight - ABorderWidth) div 2), AToLayoutUnitConverter, AViewInfoLeft);

          AViewInfoRight := TdxPreviewBorderViewInfo.Create;
          try
            AViewInfoRight.Border := ABorderInfoSource.BorderLineHorizontalIn;
            AViewInfoRight.BorderType := TdxBorderTypes.Bottom;
            AViewInfoRight.StartCorner := AMiddleCorner;
            AViewInfoRight.EndCorner := ARightCorner;
            AExporter.ExportTableBorderCorner(AMiddleCorner, (AWidth - ABorderWidth) div 2, (AHeight - ABorderWidth) div 2);
            AExporter.ExportTableBorderCorner(ARightCorner, AWidth - ABorderWidth, (AHeight - ABorderWidth) div 2);
            AExporter.ExportTableBorder(AViewInfoRight.Border, TRect.CreateSize((AWidth - ABorderWidth) div 2, 0,
              (AWidth - ABorderWidth) div 2, (AHeight - ABorderWidth) div 2), AToLayoutUnitConverter, AViewInfoRight);
            Result := ABorderWidth;
          finally
            AViewInfoRight.Free;
          end;
        finally
          AMiddleCorner.Free;
        end;
      finally
        ARightCorner.Free;
      end;
    finally
      ALeftCorner.Free;
    end;
  finally
    AViewInfoLeft.Free;
  end;
end;

function TdxRichEditBorderShadingDialogHelper.DrawHorizontalLineUp(ABorderInfoSource: TdxBorderShadingUserControlHelper;
  ADocumentModel: TdxDocumentModel; AToLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  AExporter: TdxGraphicsDocumentLayoutExporterTableBorder; AWidth, AHeight: Integer): Integer;
var
  AViewInfo, AViewInfoRight: TdxPreviewBorderViewInfo;
  ABorderWidth: Integer;
  ARightCorner, ALeftCorner, AMiddleCorner: TdxCornerViewInfoBase;
begin
  AViewInfo := TdxPreviewBorderViewInfo.Create;
  try
    AViewInfo.Border := ABorderInfoSource.BorderLineUp;
    AViewInfo.BorderType := TdxBorderTypes.Top;
    ABorderWidth := GetActualBorderWidth(AViewInfo.Border, ADocumentModel);
    ARightCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.InnerTopRight, AToLayoutUnitConverter,
      ABorderInfoSource.BorderLineUp, nil, nil, ABorderInfoSource.BorderLineRight, 0);
    try
      ALeftCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.InnerTopLeft, AToLayoutUnitConverter, nil,
        nil, ABorderInfoSource.BorderLineUp, ABorderInfoSource.BorderLineLeft, 0);
      try
        AMiddleCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.InnerTopMiddle, AToLayoutUnitConverter,
          ABorderInfoSource.BorderLineUp, nil, ABorderInfoSource.BorderLineUp, ABorderInfoSource.BorderLineVerticalIn, 0);
        try
          AViewInfo.StartCorner := ALeftCorner;
          AViewInfo.EndCorner := AMiddleCorner;
          AExporter.ExportTableBorderCorner(ALeftCorner, 0, 0);
          AExporter.ExportTableBorderCorner(AMiddleCorner, (AWidth - ABorderWidth) div 2, 0);
          AExporter.ExportTableBorder(AViewInfo.Border, TRect.Create(0, 0, (AWidth - ABorderWidth) div 2, AHeight - ABorderWidth),
            AToLayoutUnitConverter, AViewInfo);
          AViewInfoRight := TdxPreviewBorderViewInfo.Create;
          try
            AViewInfoRight.Border := ABorderInfoSource.BorderLineUp;
            AViewInfoRight.BorderType := TdxBorderTypes.Top;
            AViewInfoRight.StartCorner := AMiddleCorner;
            AViewInfoRight.EndCorner := ARightCorner;
            AExporter.ExportTableBorderCorner(AMiddleCorner, (AWidth - ABorderWidth) div 2, 0);
            AExporter.ExportTableBorderCorner(ARightCorner, AWidth - ABorderWidth, 0);
            AExporter.ExportTableBorder(AViewInfoRight.Border, TRect.CreateSize((AWidth - ABorderWidth) div 2, 0,
              (AWidth - ABorderWidth) div 2, AHeight - ABorderWidth), AToLayoutUnitConverter, AViewInfoRight);
            Result := ABorderWidth;
          finally
            AViewInfoRight.Free;
          end;
        finally
          AMiddleCorner.Free;
        end;
      finally
        ALeftCorner.Free;
      end;
    finally
      ARightCorner.Free;
    end;
  finally
    AViewInfo.Free;
  end;
end;

function TdxRichEditBorderShadingDialogHelper.DrawVerticalLineIn(ABorderInfoSource: TdxBorderShadingUserControlHelper;
  ADocumentModel: TdxDocumentModel; AToLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  AExporter: TdxGraphicsDocumentLayoutExporterTableBorder; AWidth, AHeight: Integer): Integer;
var
  AViewInfoTop, AViewInfoBottom: TdxPreviewBorderViewInfo;
  ABorderWidth: Integer;
  ATopCorner, AMiddleCorner, ABottomCorner: TdxCornerViewInfoBase;
begin
  AViewInfoTop := TdxPreviewBorderViewInfo.Create;
  try
    AViewInfoTop.Border := ABorderInfoSource.BorderLineVerticalIn;
    AViewInfoTop.BorderType := TdxBorderTypes.Right;
    ABorderWidth := GetActualBorderWidth(AViewInfoTop.Border, ADocumentModel);
    ATopCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.InnerTopMiddle, AToLayoutUnitConverter,
      ABorderInfoSource.BorderLineUp, nil, ABorderInfoSource.BorderLineUp, ABorderInfoSource.BorderLineVerticalIn, 0);
    try
      AMiddleCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.InnerNormal, AToLayoutUnitConverter,
        ABorderInfoSource.BorderLineHorizontalIn, ABorderInfoSource.BorderLineVerticalIn, ABorderInfoSource.BorderLineHorizontalIn,
        ABorderInfoSource.BorderLineVerticalIn, 0);
      try
        AViewInfoTop.StartCorner := ATopCorner;
        AViewInfoTop.EndCorner := AMiddleCorner;
        AExporter.ExportTableBorderCorner(ATopCorner, (AWidth - ABorderWidth) div 2, 0);
        AExporter.ExportTableBorderCorner(AMiddleCorner, (AWidth - ABorderWidth) div 2, (AHeight - ABorderWidth) div 2);
        AExporter.ExportTableBorder(AViewInfoTop.Border, TRect.Create(0, 0, (AWidth - ABorderWidth) div 2,
          (AHeight - ABorderWidth) div 2), AToLayoutUnitConverter, AViewInfoTop);

        AViewInfoBottom := TdxPreviewBorderViewInfo.Create;
        try
          AViewInfoBottom.Border := ABorderInfoSource.BorderLineVerticalIn;
          AViewInfoBottom.BorderType := TdxBorderTypes.Right;
          ABottomCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.InnerBottomMiddle, AToLayoutUnitConverter,
            ABorderInfoSource.BorderLineDown, ABorderInfoSource.BorderLineVerticalIn, ABorderInfoSource.BorderLineDown, nil, 0);
          try
            AViewInfoBottom.StartCorner := AMiddleCorner;
            AViewInfoBottom.EndCorner := ABottomCorner;
            AExporter.ExportTableBorderCorner(AMiddleCorner, (AWidth - ABorderWidth) div 2, (AHeight - ABorderWidth) div 2);
            AExporter.ExportTableBorderCorner(ABottomCorner, (AWidth - ABorderWidth) div 2, AHeight - ABorderWidth);
            AExporter.ExportTableBorder(AViewInfoBottom.Border, TRect.CreateSize(0, (AHeight - ABorderWidth) div 2,
              (AWidth - ABorderWidth) div 2, (AHeight - ABorderWidth) div 2), AToLayoutUnitConverter, AViewInfoBottom);
            Result := ABorderWidth;
          finally
            ABottomCorner.Free;
          end;
        finally
          AViewInfoBottom.Free;
        end;
      finally
        AMiddleCorner.Free;
      end;
    finally
      ATopCorner.Free;
    end;
  finally
    AViewInfoTop.Free;
  end;
end;

function TdxRichEditBorderShadingDialogHelper.DrawVerticalLineLeft(ABorderInfoSource: TdxBorderShadingUserControlHelper;
  ADocumentModel: TdxDocumentModel; AToLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  AExporter: TdxGraphicsDocumentLayoutExporterTableBorder; AWidth, AHeight: Integer): Integer;
var
  AViewInfo, AViewInfoBottom: TdxPreviewBorderViewInfo;
  ABorderWidth: Integer;
  ATopCorner, AMiddleCorner, ABottomCorner: TdxCornerViewInfoBase;
begin
  AViewInfo := TdxPreviewBorderViewInfo.Create;
  try
    AViewInfo.Border := ABorderInfoSource.BorderLineLeft;
    AViewInfo.BorderType := TdxBorderTypes.Left;
    ABorderWidth := GetActualBorderWidth(AViewInfo.Border, ADocumentModel);
    ATopCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.InnerTopLeft, AToLayoutUnitConverter, nil, nil,
      ABorderInfoSource.BorderLineUp, ABorderInfoSource.BorderLineLeft, 0);
    try
      ABottomCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.InnerBottomLeft, AToLayoutUnitConverter,
        nil, ABorderInfoSource.BorderLineLeft, ABorderInfoSource.BorderLineDown, nil, 0);
      try
        AMiddleCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.InnerLeftMiddle, AToLayoutUnitConverter,
          nil, ABorderInfoSource.BorderLineLeft, ABorderInfoSource.BorderLineHorizontalIn, ABorderInfoSource.BorderLineLeft, 0);
        try
          AViewInfo.StartCorner := ATopCorner;
          AViewInfo.EndCorner := AMiddleCorner;
          AExporter.ExportTableBorderCorner(ATopCorner, 0, 0);
          AExporter.ExportTableBorderCorner(AMiddleCorner, 0, (AHeight - ABorderWidth) div 2);
          AExporter.ExportTableBorder(AViewInfo.Border, TRect.Create(0, 0, (AWidth - ABorderWidth) div 2,
            (AHeight - ABorderWidth) div 2), AToLayoutUnitConverter, AViewInfo);
          AViewInfoBottom := TdxPreviewBorderViewInfo.Create;
          try
            AViewInfoBottom.Border := ABorderInfoSource.BorderLineLeft;
            AViewInfoBottom.BorderType := TdxBorderTypes.Left;
            AViewInfoBottom.StartCorner := AMiddleCorner;
            AViewInfoBottom.EndCorner := ABottomCorner;
            AExporter.ExportTableBorderCorner(AMiddleCorner, 0, (AHeight - ABorderWidth) div 2);
            AExporter.ExportTableBorderCorner(ABottomCorner, 0, AHeight - ABorderWidth);
            AExporter.ExportTableBorder(AViewInfoBottom.Border, TRect.CreateSize(0, (AHeight - ABorderWidth) div 2,
              (AWidth - ABorderWidth) div 2, (AHeight - ABorderWidth) div 2), AToLayoutUnitConverter, AViewInfoBottom);
            Result := ABorderWidth;
          finally
            AViewInfoBottom.Free;
          end;
        finally
          AMiddleCorner.Free;
        end;
      finally
        ABottomCorner.Free;
      end;
    finally
      ATopCorner.Free;
    end;
  finally
    AViewInfo.Free;
  end;
end;

function TdxRichEditBorderShadingDialogHelper.DrawVerticalLineRight(
  ABorderInfoSource: TdxBorderShadingUserControlHelper; ADocumentModel: TdxDocumentModel;
  AToLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  AExporter: TdxGraphicsDocumentLayoutExporterTableBorder; AWidth, AHeight: Integer): Integer;
var
  AViewInfo, AViewInfoBottom: TdxPreviewBorderViewInfo;
  ABorderWidth: Integer;
  ATopCorner, AMiddleCorner, ABottomCorner: TdxCornerViewInfoBase;
begin
  AViewInfo := TdxPreviewBorderViewInfo.Create;
  try
    AViewInfo.Border := ABorderInfoSource.BorderLineRight;
    AViewInfo.BorderType := TdxBorderTypes.Right;
    ABorderWidth := GetActualBorderWidth(AViewInfo.Border, ADocumentModel);
    ATopCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.InnerTopRight, AToLayoutUnitConverter, ABorderInfoSource.BorderLineUp, nil, nil, ABorderInfoSource.BorderLineRight, 0);
    try
      ABottomCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.InnerBottomRight, AToLayoutUnitConverter, ABorderInfoSource.BorderLineDown, ABorderInfoSource.BorderLineRight, nil, nil, 0);
      try
        AMiddleCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.InnerRightMiddle, AToLayoutUnitConverter, ABorderInfoSource.BorderLineHorizontalIn, ABorderInfoSource.BorderLineRight, nil, ABorderInfoSource.BorderLineRight, 0);
        try
          AViewInfo.StartCorner := ATopCorner;
          AViewInfo.EndCorner := AMiddleCorner;
          AExporter.ExportTableBorderCorner(ATopCorner, AWidth - ABorderWidth, 0);
          AExporter.ExportTableBorderCorner(AMiddleCorner, AWidth - ABorderWidth, (AHeight - ABorderWidth) div 2);
          AExporter.ExportTableBorder(AViewInfo.Border, TRect.Create(0, 0, AWidth - ABorderWidth, (AHeight - ABorderWidth) div 2), AToLayoutUnitConverter, AViewInfo);
          AViewInfoBottom := TdxPreviewBorderViewInfo.Create;
          try
            AViewInfoBottom.Border := ABorderInfoSource.BorderLineRight;
            AViewInfoBottom.BorderType := TdxBorderTypes.Right;
            AViewInfoBottom.StartCorner := AMiddleCorner;
            AViewInfoBottom.EndCorner := ABottomCorner;
            AExporter.ExportTableBorderCorner(AMiddleCorner, AWidth - ABorderWidth, (AHeight - ABorderWidth) div 2);
            AExporter.ExportTableBorderCorner(ABottomCorner, AWidth - ABorderWidth, AHeight - ABorderWidth);
            AExporter.ExportTableBorder(AViewInfoBottom.Border, TRect.CreateSize(0, (AHeight - ABorderWidth) div 2, AWidth - ABorderWidth, (AHeight - ABorderWidth) div 2), AToLayoutUnitConverter, AViewInfoBottom);
            Result := ABorderWidth;
          finally
            AViewInfoBottom.Free;
          end;
        finally
          AMiddleCorner.Free;
        end;
      finally
        ABottomCorner.Free;
      end;
    finally
      ATopCorner.Free;
    end;
  finally
    AViewInfo.Free;
  end;
end;

function TdxRichEditBorderShadingDialogHelper.GetActualBorderWidth(ABorder: TdxBorderInfo;
  DocumentModel: TdxDocumentModel): Integer;
var
  ABorderCalculator: TdxTableBorderCalculator;
  AScaleFactor, AThickness: Integer;
begin
  ABorderCalculator := TdxTableBorderCalculator.Create;
  try
    AScaleFactor := 1;
    AThickness := Math.Max(1, DocumentModel.UnitConverter.ModelUnitsToPixels(ABorder.Width * AScaleFactor, DocumentModel.Dpi));
    Result := ABorderCalculator.GetActualWidth(ABorder.Style, AThickness);
  finally
    ABorderCalculator.Free;
  end;
end;

class function TdxRichEditBorderShadingDialogHelper.GetBorderLineStyleTypeName(
  ABorderLineStyle: TdxBorderLineStyle): string;
begin
  Result := IntToStr(Ord(ABorderLineStyle));
end;

function TdxRichEditBorderShadingDialogHelper.GetDocumentModel: TdxDocumentModel;
begin
  Result := Control.DocumentModel;
end;

function TdxRichEditBorderShadingDialogHelper.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := Control.DocumentModel.UnitConverter;
end;

procedure TdxRichEditBorderShadingDialogHelper.PopulateBorderInfos;
var
  ABorderInfos: TdxBorderInfoList;
  I: Integer;
begin
  if FBorderInfos.Count > 0 then
    Exit;
  ABorderInfos := DocumentModel.TableBorderInfoRepository.Items;
  for I := 0 to ABorderInfos.Count - 1 do
    FBorderInfos.Add(ABorderInfos[I].Clone);
end;

procedure TdxRichEditBorderShadingDialogHelper.PopulateBorderLineStyle(AImageList: TcxImageList; AStrings: TStrings;
  ASkipNone: Boolean);
var
  ABitmap: TcxBitmap;
  ABorderInfo: TdxBorderInfo;
  I: Integer;

  procedure AddImage(ABorderInfo: TdxBorderInfo);
  begin
    cxPaintCanvas.BeginPaint(ABitmap.Canvas);
    TdxBorderLineWeightPainter.DrawBorderLineItem(ABorderInfo, ABitmap.cxCanvas, ABitmap.ClientRect,
      TdxAlphaColors.White, TdxAlphaColors.Black, UnitConverter);
    cxPaintCanvas.EndPaint;
    AImageList.Add(ABitmap, nil);
  end;

  procedure AddItem(ABorderInfo: TdxBorderInfo);
  var
    ADescription: string;
  begin
    if ABorderInfo.Style = TdxBorderLineStyle.None then
      ADescription := cxGetResourceString(@sdxRichEditBorderLineStyleNone)
    else
      ADescription := GetBorderLineStyleTypeName(ABorderInfo.Style);
    AStrings.AddObject(ADescription, ABorderInfo);
  end;

begin
  PopulateBorderInfos;
  AStrings.BeginUpdate;
  ABitmap := TcxBitmap.CreateSize(AImageList.Width, AImageList.Height);
  try
    AStrings.Clear;
    AImageList.Clear;
    for I := 0 to FBorderInfos.Count - 1 do
    begin
      ABorderInfo := FBorderInfos[I];
      if not ShouldSkipBorderStyle(ABorderInfo.Style, ASkipNone) then
      begin
        AddImage(ABorderInfo);
        AddItem(ABorderInfo);
      end;
    end;
  finally
    ABitmap.Free;
    AStrings.EndUpdate;
  end;
end;

procedure TdxRichEditBorderShadingDialogHelper.SetBorder(const Value: TdxBorderInfo);
begin
  FBorder.CopyFrom(Value);
  FColor := FBorder.Color;
  FWidth := FBorder.Width;
end;

procedure TdxRichEditBorderShadingDialogHelper.SetColor(const Value: TdxAlphaColor);
begin
  FColor := Value;
  FBorder.Color := FColor;
  UpdateBorderInfos;
end;

procedure TdxRichEditBorderShadingDialogHelper.SetWidth(const Value: Integer);
begin
  FWidth := Value;
  FBorder.Width := FWidth;
  UpdateBorderInfos;
end;

function TdxRichEditBorderShadingDialogHelper.ShouldSkipBorderStyle(AStyle: TdxBorderLineStyle;
  ASkipNone: Boolean): Boolean;
begin
  if ASkipNone and ((AStyle = TdxBorderLineStyle.None) or (AStyle = TdxBorderLineStyle.&Nil)) then
    Result := True
  else
    Result := (AStyle = TdxBorderLineStyle.Disabled) or (AStyle = TdxBorderLineStyle.DashDotStroked) or
      (AStyle = TdxBorderLineStyle.ThreeDEmboss) or (AStyle = TdxBorderLineStyle.ThreeDEngrave) or
      (AStyle = TdxBorderLineStyle.Inset) or (AStyle = TdxBorderLineStyle.Outset);
end;

procedure TdxRichEditBorderShadingDialogHelper.UpdateBorderInfos;
var
  ABorderInfo: TdxBorderInfo;
  I: Integer;
begin
  for I := 0 to FBorderInfos.Count - 1 do
  begin
    ABorderInfo := FBorderInfos[I];
    ABorderInfo.Color := Color;
    ABorderInfo.Width := Width;
  end;
end;

{ TdxBorderShadingUserControlHelper }

procedure TdxBorderShadingUserControlHelper.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TdxBorderShadingUserControlHelper.btnHorizontBorderDownChecked(Sender: TObject);
begin
  if InsideSetProperties then
    Exit;
  if FBorderLineBtns[TdxTableCellBorderLine.Down].SpeedButtonOptions.Down then
  begin
    BorderLineDown := FCurrentBorderInfo;
    FPreviewBSUserControl.HorizontalLineDown := TdxBorderLineState.Known;
  end
  else
  begin
    BorderLineDown := FInactiveBorderInfo;
    FPreviewBSUserControl.HorizontalLineDown := TdxBorderLineState.No;
  end;
  DoButtonBorderLineChecked(TdxTableCellBorderLine.Down);
end;

procedure TdxBorderShadingUserControlHelper.btnHorizontBorderInChecked(Sender: TObject);
begin
  if InsideSetProperties then
    Exit;
  if FBorderLineBtns[TdxTableCellBorderLine.HorizontalIn].SpeedButtonOptions.Down and FBorderLineHorizontalInVisible then
  begin
    BorderLineHorizontalIn := FCurrentBorderInfo;
    if Mode = TdxSetMode.Grid then
    begin
      BorderLineHorizontalIn.Style := TdxBorderLineStyle.Single;
      BorderLineHorizontalIn.Width := DocumentModel.UnitConverter.TwipsToModelUnits(15);
    end;
    FPreviewBSUserControl.HorizontalLineIn := TdxBorderLineState.Known;
  end
  else
  begin
    BorderLineHorizontalIn := FInactiveBorderInfo;
    FPreviewBSUserControl.HorizontalLineIn := TdxBorderLineState.No;
  end;
  DoButtonBorderLineChecked(TdxTableCellBorderLine.HorizontalIn);
end;

procedure TdxBorderShadingUserControlHelper.btnHorizontBorderUpChecked(Sender: TObject);
begin
  if InsideSetProperties then
    Exit;
  if FBorderLineBtns[TdxTableCellBorderLine.Up].SpeedButtonOptions.Down then
  begin
    BorderLineUp := FCurrentBorderInfo;
    FPreviewBSUserControl.HorizontalLineUp := TdxBorderLineState.Known;
  end
  else
  begin
    BorderLineUp := FInactiveBorderInfo;
    FPreviewBSUserControl.HorizontalLineUp := TdxBorderLineState.No;
  end;
  DoButtonBorderLineChecked(TdxTableCellBorderLine.Up);
end;

procedure TdxBorderShadingUserControlHelper.BtnsVisibleChanged;
var
  I: TdxTableCellBorderLine;
begin
  for I := Low(TdxTableCellBorderLine) to High(TdxTableCellBorderLine) do
  begin
    case I of
      TdxTableCellBorderLine.HorizontalIn:
        if not FBorderLineHorizontalInVisible then
          Continue;
      TdxTableCellBorderLine.VerticalIn:
        if not FBorderLineVerticalInVisible then
          Continue;
    end;
    SetBtnBorderLineVisible(I, FButtonsVisible);
  end;
end;

procedure TdxBorderShadingUserControlHelper.btnVerticalBorderInChecked(Sender: TObject);
begin
  if InsideSetProperties then
    Exit;
  if FBorderLineBtns[TdxTableCellBorderLine.VerticalIn].SpeedButtonOptions.Down and BorderLineVerticalInVisible then
  begin
    BorderLineVerticalIn := FCurrentBorderInfo;
    if Mode = TdxSetMode.Grid then
    begin
      BorderLineVerticalIn.Style := TdxBorderLineStyle.Single;
      BorderLineVerticalIn.Width := DocumentModel.UnitConverter.TwipsToModelUnits(15);
    end;
    FPreviewBSUserControl.VerticalLineIn := TdxBorderLineState.Known;
  end
  else
  begin
    BorderLineVerticalIn := FInactiveBorderInfo;
    FPreviewBSUserControl.VerticalLineIn := TdxBorderLineState.No;
  end;
  DoButtonBorderLineChecked(TdxTableCellBorderLine.VerticalIn);
end;

procedure TdxBorderShadingUserControlHelper.btnVerticalBorderLeftChecked(Sender: TObject);
begin
  if InsideSetProperties then
    Exit;
  if FBorderLineBtns[TdxTableCellBorderLine.Left].SpeedButtonOptions.Down then
  begin
    BorderLineLeft := FCurrentBorderInfo;
    FPreviewBSUserControl.VerticalLineLeft := TdxBorderLineState.Known;
  end
  else
  begin
    BorderLineLeft := FInactiveBorderInfo;
    FPreviewBSUserControl.VerticalLineLeft := TdxBorderLineState.No;
  end;
  DoButtonBorderLineChecked(TdxTableCellBorderLine.Left);
end;

procedure TdxBorderShadingUserControlHelper.btnVerticalBorderRightChecked(Sender: TObject);
begin
  if InsideSetProperties then
    Exit;
  if FBorderLineBtns[TdxTableCellBorderLine.Right].SpeedButtonOptions.Down then
  begin
    BorderLineRight := FCurrentBorderInfo;
    FPreviewBSUserControl.VerticalLineRight := TdxBorderLineState.Known;
  end
  else
  begin
    BorderLineRight := FInactiveBorderInfo;
    FPreviewBSUserControl.VerticalLineRight := TdxBorderLineState.No;
  end;
  DoButtonBorderLineChecked(TdxTableCellBorderLine.Right);
end;

constructor TdxBorderShadingUserControlHelper.Create(APreviewBorderShadingControl: TdxPreviewBorderShadingControlHelper);
begin
  inherited Create;
  FCurrentBorderInfo := TdxBorderInfo.Create;
  FInactiveBorderInfo := TdxBorderInfo.Create;
  FInactiveBorderInfo.Style := TdxBorderLineStyle.None;
  FPreviewBSUserControl := APreviewBorderShadingControl;
  APreviewBorderShadingControl.FBorderInfoSource := Self;
  FSetMode := TdxSetMode.Custom;
end;

destructor TdxBorderShadingUserControlHelper.Destroy;
var
  I: TdxTableCellBorderLine;
begin
  for I := Low(FBorderLines) to High(FBorderLines) do
    FBorderLines[I].Free;
  FCurrentBorderInfo.Free;
  FInactiveBorderInfo.Free;
  inherited Destroy;
end;

procedure TdxBorderShadingUserControlHelper.DoButtonBorderLineChecked(AIndex: TdxTableCellBorderLine);
begin
  if Assigned(FBorderLineBtnOnChanges[AIndex]) then
    FBorderLineBtnOnChanges[AIndex](Self);
end;

procedure TdxBorderShadingUserControlHelper.EndUpdate;
begin
  Dec(FUpdateCount);
end;

function TdxBorderShadingUserControlHelper.GetBorderLineInfo(const Index: TdxTableCellBorderLine): TdxBorderInfo;
begin
  Result := FBorderLines[Index];
end;

function TdxBorderShadingUserControlHelper.GetDocumentModel: TdxDocumentModel;
begin
  Result := FPreviewBSUserControl.DocumentModel;
end;

function TdxBorderShadingUserControlHelper.GetDrawColumns: Boolean;
begin
  Result := FPreviewBSUserControl.DrawColumns;
end;

function TdxBorderShadingUserControlHelper.GetDrawParagraph: Boolean;
begin
  Result := FPreviewBSUserControl.DrawParagraph;
end;

function TdxBorderShadingUserControlHelper.GetFillColor: TdxAlphaColor;
begin
  Result := FPreviewBSUserControl.FillColor;
end;

function TdxBorderShadingUserControlHelper.GetInsideSetProperties: Boolean;
begin
  Result := FUpdateCount > 0;
end;

function TdxBorderShadingUserControlHelper.GetOnBorderLineChanged(const Index: TdxTableCellBorderLine): TNotifyEvent;
begin
  Result := FBorderLineBtnOnChanges[Index];
end;

procedure TdxBorderShadingUserControlHelper.Initialize(ABtnLineUp, ABtnLineDown, ABtnLineHorizontalIn, ABtnLineLeft,
  ABtnLineRight, ABtnLineVerticalIn: TcxButton);
begin
  FBorderLineBtns[TdxTableCellBorderLine.Up] := ABtnLineUp;
  FBorderLineBtns[TdxTableCellBorderLine.Down] := ABtnLineDown;
  FBorderLineBtns[TdxTableCellBorderLine.HorizontalIn] := ABtnLineHorizontalIn;
  FBorderLineBtns[TdxTableCellBorderLine.Left] := ABtnLineLeft;
  FBorderLineBtns[TdxTableCellBorderLine.Right] := ABtnLineRight;
  FBorderLineBtns[TdxTableCellBorderLine.VerticalIn] := ABtnLineVerticalIn;
  SubscribeEvents;
end;

procedure TdxBorderShadingUserControlHelper.SetAllModeCore;
var
  AButton: TcxButton;
begin
  for AButton in FBorderLineBtns do
    SetButtonDown(AButton, True);
end;

procedure TdxBorderShadingUserControlHelper.SetBorderLineInfo(const Index: TdxTableCellBorderLine; const Value: TdxBorderInfo);
begin
  if Value = nil then
    FBorderLines[Index] := nil
  else
    if FBorderLines[Index] = nil then
      FBorderLines[Index] := Value.Clone
    else
      FBorderLines[Index].CopyFrom(Value);
  if Assigned(Value) then
  begin
    if Value.Style <> TdxBorderLineStyle.None then
    begin
      SetButtonDown(FBorderLineBtns[Index], True);
      FPreviewBSUserControl.SetBorderLineState(Index, TdxBorderLineState.Known);
    end
    else
    begin
      SetButtonDown(FBorderLineBtns[Index], False);
      FPreviewBSUserControl.SetBorderLineState(Index, TdxBorderLineState.No);
    end;
  end
  else
  begin
    SetButtonDown(FBorderLineBtns[Index], False);
    FPreviewBSUserControl.SetBorderLineState(Index, TdxBorderLineState.Unknown);
  end;
end;

procedure TdxBorderShadingUserControlHelper.SetBorderLineHorizontalInVisible(const Value: Boolean);
begin
  FBorderLineHorizontalInVisible := Value;
  if FBorderLineHorizontalInVisible then
    SetBtnBorderLineVisible(TdxTableCellBorderLine.HorizontalIn, True)
  else
  begin
    SetBtnBorderLineVisible(TdxTableCellBorderLine.HorizontalIn, False);
    FPreviewBSUserControl.HorizontalLineIn := TdxBorderLineState.No;
  end;
end;

procedure TdxBorderShadingUserControlHelper.SetBorderLineVerticalInVisible(const Value: Boolean);
begin
  FBorderLineVerticalInVisible := Value;
  if FBorderLineVerticalInVisible then
    SetBtnBorderLineVisible(TdxTableCellBorderLine.VerticalIn, True)
  else
  begin
    SetBtnBorderLineVisible(TdxTableCellBorderLine.VerticalIn, False);
    FPreviewBSUserControl.VerticalLineIn := TdxBorderLineState.No;
  end;
end;

procedure TdxBorderShadingUserControlHelper.SetBtnBorderLineVisible(const Index: TdxTableCellBorderLine;
  const Value: Boolean);
begin
  if Assigned(FBorderLineBtns[Index]) then
    FBorderLineBtns[Index].Visible := Value;
end;

procedure TdxBorderShadingUserControlHelper.SetBoxModeCore;
begin
  SetButtonDown(FBorderLineBtns[TdxTableCellBorderLine.Down], True);
  SetButtonDown(FBorderLineBtns[TdxTableCellBorderLine.Up], True);
  SetButtonDown(FBorderLineBtns[TdxTableCellBorderLine.Right], True);
  SetButtonDown(FBorderLineBtns[TdxTableCellBorderLine.Left], True);
  SetButtonDown(FBorderLineBtns[TdxTableCellBorderLine.HorizontalIn], False);
  SetButtonDown(FBorderLineBtns[TdxTableCellBorderLine.VerticalIn], False);
end;

class procedure TdxBorderShadingUserControlHelper.SetButtonDown(AButton: TcxButton; ADown: Boolean);
begin
  if AButton.SpeedButtonOptions.Down = ADown then
    Exit;
  AButton.SpeedButtonOptions.Down := ADown;
  if Assigned(AButton.OnClick) then
    AButton.OnClick(AButton);
end;

procedure TdxBorderShadingUserControlHelper.SetButtonsVisible(const Value: Boolean);
begin
  FButtonsVisible := Value;
  BtnsVisibleChanged;
end;

procedure TdxBorderShadingUserControlHelper.SetCurrentBorderInfo(const Value: TdxBorderInfo);
begin
  if Assigned(FCurrentBorderInfo) then
    FCurrentBorderInfo.CopyFrom(Value)
  else
    FCurrentBorderInfo := Value.Clone;
  if FSetMode <> TdxSetMode.Custom then
    SetNoneModeCore;
  case FSetMode of
    TdxSetMode.Box:
      SetBoxModeCore;
    TdxSetMode.All:
      SetAllModeCore;
    TdxSetMode.Grid:
      SetGridModeCore;
  end;
end;

procedure TdxBorderShadingUserControlHelper.SetCustomModeCore;
begin
  SetAllModeCore;
end;

procedure TdxBorderShadingUserControlHelper.SetDocumentModel(const Value: TdxDocumentModel);
begin
  FPreviewBSUserControl.DocumentModel := Value;
end;

procedure TdxBorderShadingUserControlHelper.SetDrawColumns(const Value: Boolean);
begin
  FPreviewBSUserControl.DrawColumns := Value;
end;

procedure TdxBorderShadingUserControlHelper.SetDrawParagraph(const Value: Boolean);
begin
  FPreviewBSUserControl.DrawParagraph := Value;
end;

procedure TdxBorderShadingUserControlHelper.SetFillColor(const Value: TdxAlphaColor);
begin
  FPreviewBSUserControl.FillColor := Value;
end;

procedure TdxBorderShadingUserControlHelper.SetGridModeCore;
begin
  SetAllModeCore;
end;

procedure TdxBorderShadingUserControlHelper.SetNoneModeCore;
var
  AButton: TcxButton;
begin
  for AButton in FBorderLineBtns do
    SetButtonDown(AButton, False);
end;

procedure TdxBorderShadingUserControlHelper.SetOnBorderLineChanged(const Index: TdxTableCellBorderLine; const Value: TNotifyEvent);
begin
  FBorderLineBtnOnChanges[Index] := Value;
end;

procedure TdxBorderShadingUserControlHelper.SetMode(const Value: TdxSetMode);
begin
  FSetMode := Value;
  case Value of
    TdxSetMode.None:
      SetNoneModeCore;
    TdxSetMode.Box:
      SetBoxModeCore;
    TdxSetMode.All:
      SetAllModeCore;
    TdxSetMode.Grid:
      SetGridModeCore;
    TdxSetMode.Custom:
      SetCustomModeCore;
  end;
end;

procedure TdxBorderShadingUserControlHelper.SubscribeEvents;
begin
  FBorderLineBtns[TdxTableCellBorderLine.Down].OnClick := btnHorizontBorderDownChecked;
  FBorderLineBtns[TdxTableCellBorderLine.Up].OnClick := btnHorizontBorderUpChecked;
  FBorderLineBtns[TdxTableCellBorderLine.Right].OnClick := btnVerticalBorderRightChecked;
  FBorderLineBtns[TdxTableCellBorderLine.Left].OnClick := btnVerticalBorderLeftChecked;
  FBorderLineBtns[TdxTableCellBorderLine.HorizontalIn].OnClick := btnHorizontBorderInChecked;
  FBorderLineBtns[TdxTableCellBorderLine.VerticalIn].OnClick := btnVerticalBorderInChecked;
end;

{ TdxPreviewBorderShadingControlHelper }

procedure TdxPreviewBorderShadingControlHelper.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

type
  TdxCustomRichEditControlAccess = class(TdxCustomRichEditControl);

procedure TdxPreviewBorderShadingControlHelper.Draw(ACanvas: TcxCanvas);

  function DrawLine(ALeft, ATop, ARight, ABottom: Integer; AColor: TdxAlphaColor; AWidth: Integer = 10): Integer;
  begin
    ACanvas.Pen.Color := TdxAlphaColors.ToColor(AColor);
    ACanvas.Pen.Width := AWidth;
    ACanvas.MoveTo(ALeft, ATop);
    ACanvas.LineTo(ARight, ABottom);
    Result := 10;
  end;

var
  AGraphics: TdxGraphics;
  APainter: TdxPainter;
  AConverter: TdxDocumentLayoutUnitConverter;
  ADpiX: Single;
  AHorizontalLinePainter, AVerticalLinePainter: TdxRichEditPatternLinePainter;
  AExporter: TdxGraphicsDocumentLayoutExporterTableBorder;
  AToLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  AHelper: TdxRichEditBorderShadingDialogHelper;
  AVerticalLineRightWidth, AVerticalLineLeftWidth, AVerticalLineInWidth: Integer;
  AHorizontalLineInWidth, AHorizontalLineUpWidth, AHorizontalLineDownWidth: Integer;
begin
  if not Assigned(DocumentModel) then
    Exit;
  ADpiX := DocumentModel.DpiX;
  AGraphics := TdxGraphics.CreateFromHdc(ACanvas.Handle);
  APainter := TdxPainter(TdxCustomRichEditControlAccess(FControl).MeasurementAndDrawingStrategy.CreateDocumentPainter(AGraphics));

  AConverter := TdxDocumentLayoutUnitPixelsConverter.Create(ADpiX);
  AHorizontalLinePainter := TdxRichEditHorizontalPatternLinePainter.Create(APainter, AConverter);
  AVerticalLinePainter := TdxRichEditVerticalPatternLinePainter.Create(APainter, AConverter);
  AExporter := TdxGraphicsDocumentLayoutExporterTableBorder.Create(DocumentModel, APainter, AHorizontalLinePainter, AVerticalLinePainter);
  AToLayoutUnitConverter := DocumentModel.UnitConverter.CreateConverterToLayoutUnits(TdxDocumentLayoutUnit.Pixel, ADpiX);
  AHelper := TdxRichEditBorderShadingDialogHelper.Create;
  try
    AVerticalLineRightWidth := 0;
    AVerticalLineLeftWidth := 0;
    AVerticalLineInWidth := 0;
    AHorizontalLineInWidth := 0;
    AHorizontalLineUpWidth := 0;
    AHorizontalLineDownWidth := 0;
    if not TdxAlphaColors.IsEmpty(FillColor) then
      AGraphics.FillRectangle(TRect.Create(0, 0, Width - 1, Height - 1), FillColor);

    if VerticalLineRight = TdxBorderLineState.Known then
       AVerticalLineRightWidth := AHelper.DrawVerticalLineRight(BorderInfoSource, DocumentModel, AToLayoutUnitConverter,
         AExporter, Width, Height);

    if VerticalLineRight = TdxBorderLineState.Unknown then
      AVerticalLineRightWidth := DrawLine(Width - 1, 0, Width - 1, Height, TdxAlphaColors.Silver);
    if VerticalLineLeft = TdxBorderLineState.Known then
      AVerticalLineLeftWidth := AHelper.DrawVerticalLineLeft(BorderInfoSource, DocumentModel, AToLayoutUnitConverter, AExporter, Width, Height)
    else
      if VerticalLineLeft = TdxBorderLineState.Unknown then
        AVerticalLineLeftWidth := DrawLine(0, 0, 0, Height, TdxAlphaColors.Silver);

    if VerticalLineIn = TdxBorderLineState.Known then
      AVerticalLineInWidth := AHelper.DrawVerticalLineIn(BorderInfoSource, DocumentModel, AToLayoutUnitConverter, AExporter, Width, Height)
    else
      if VerticalLineIn = TdxBorderLineState.Unknown then
        AVerticalLineInWidth := DrawLine(Width div 2, 0, Width div 2, Height, TdxAlphaColors.Silver);

    if HorizontalLineIn = TdxBorderLineState.Known then
      AHorizontalLineInWidth := AHelper.DrawHorizontalLineIn(BorderInfoSource, documentModel, AToLayoutUnitConverter, AExporter, Width, Height)
    else
      if HorizontalLineIn = TdxBorderLineState.Unknown then
        AHorizontalLineInWidth := DrawLine(0, Height div 2, Width, Height div 2, TdxAlphaColors.Silver);

   if HorizontalLineUp = TdxBorderLineState.Known then
      AHorizontalLineUpWidth := AHelper.DrawHorizontalLineUp(BorderInfoSource, DocumentModel, AToLayoutUnitConverter, AExporter, Width, Height)
    else
      if HorizontalLineUp = TdxBorderLineState.Unknown then
        AHorizontalLineUpWidth := DrawLine(0, 0, Width, 0, TdxAlphaColors.Silver);

    if HorizontalLineDown = TdxBorderLineState.Known then
      AHorizontalLineDownWidth := AHelper.DrawHorizontalLineDown(BorderInfoSource, DocumentModel, AToLayoutUnitConverter, AExporter, Width, Height)
    else
      if HorizontalLineDown = TdxBorderLineState.Unknown then
        AHorizontalLineDownWidth := DrawLine(0, Height - 1, Width, Height - 1, TdxAlphaColors.Silver);

    if DrawColumns then
    begin
      DrawRectangles(AGraphics, (Width - AVerticalLineRightWidth) div 2 - AVerticalLineLeftWidth - (AVerticalLineInWidth div 2),
        (Height) div 2 - AHorizontalLineUpWidth - (AHorizontalLineInWidth div 2), AVerticalLineLeftWidth, AHorizontalLineUpWidth);
      DrawRectangles(AGraphics, (Width - AVerticalLineRightWidth) div 2 - AVerticalLineLeftWidth - (AVerticalLineInWidth div 2),
        (Height) div 2 - AHorizontalLineDownWidth - (AHorizontalLineInWidth div 2), AVerticalLineLeftWidth,
        (Height{ - horizontalLineDownWidth}) div 2 + (AHorizontalLineInWidth div 2));
      DrawRectangles(AGraphics, (Width - AVerticalLineRightWidth) div 2 - (AVerticalLineInWidth div 2) - AVerticalLineRightWidth,
        (Height) div 2 - AHorizontalLineUpWidth - (AHorizontalLineInWidth div 2), (Width{ - verticalLineRightWidth}) div 2 + (AVerticalLineInWidth div 2),
        AHorizontalLineUpWidth);
      DrawRectangles(AGraphics, (Width - AVerticalLineRightWidth) div 2 - (AVerticalLineInWidth div 2) - AVerticalLineRightWidth,
        (Height) div 2 - AHorizontalLineDownWidth - (AHorizontalLineInWidth div 2), (Width {- verticalLineRightWidth}) div 2 + (AVerticalLineInWidth div 2),
        (Height{ - horizontalLineDownWidth}) div 2 + (AHorizontalLineInWidth div 2));
    end
    else
      if DrawParagraph then
        begin
          DrawRectangles(AGraphics, Width - AVerticalLineLeftWidth - AVerticalLineRightWidth,
            Height div 2 - AHorizontalLineUpWidth - (AHorizontalLineInWidth div 2), AVerticalLineLeftWidth, AHorizontalLineUpWidth);
          DrawRectangles(AGraphics, Width - AVerticalLineLeftWidth - AVerticalLineRightWidth,
            Height div 2 - AHorizontalLineDownWidth - (AHorizontalLineInWidth div 2), AVerticalLineLeftWidth,
            Height div 2 + (AHorizontalLineInWidth div 2));
        end
      else
        DrawRectangles(AGraphics, Width - AVerticalLineLeftWidth - AVerticalLineRightWidth,
          Height - AHorizontalLineUpWidth - AHorizontalLineDownWidth, AVerticalLineLeftWidth, AHorizontalLineUpWidth);
  finally
    AHelper.Free;
    AToLayoutUnitConverter.Free;
    AExporter.Free;
    AVerticalLinePainter.Free;
    AHorizontalLinePainter.Free;
    AConverter.Free;
    APainter.Free;
    AGraphics.Free;
  end;
end;

procedure TdxPreviewBorderShadingControlHelper.DrawRectangles(AGraphics: TdxGraphics; AWidth, AHeight, AX, AY: Integer);
const
  HeightRectangle = 3;
  PageBorder: array[0..3] of Integer = (1, 1, 1, 1);
var
  I: Integer;
  ACountRectangle: Integer;
begin
  ACountRectangle := (AHeight - PageBorder[0] - PageBorder[1]) div (2 * HeightRectangle);
  for I := 0 to ACountRectangle - 1 do
    AGraphics.FillRectangle(TRect.CreateSize(AX + PageBorder[2], AY + I * 2 * HeightRectangle + PageBorder[0],
      AWidth - PageBorder[3] - PageBorder[2], HeightRectangle), TdxAlphaColors.FromColor(BrushesLightGray));
end;

function TdxPreviewBorderShadingControlHelper.GetBorderLineState(const Index: TdxTableCellBorderLine): TdxBorderLineState;
begin
  Result := FBorderLineStates[Index];
end;

function TdxPreviewBorderShadingControlHelper.GetBounds: TRect;
begin
  Result := Rect(0, 0, FWidth, FHeight);
end;

procedure TdxPreviewBorderShadingControlHelper.SetBorderLineState(const Index: TdxTableCellBorderLine;
  const Value: TdxBorderLineState);
begin
  FBorderLineStates[Index] := Value;
  DoChange;
end;

procedure TdxPreviewBorderShadingControlHelper.SetBounds(const Value: TRect);
begin
  FHeight := Value.Bottom;
  FWidth := Value.Right;
end;

procedure TdxPreviewBorderShadingControlHelper.SetDrawColumns(const Value: Boolean);
begin
  FDrawColumns := Value;
  DoChange;
end;

procedure TdxPreviewBorderShadingControlHelper.SetDrawPageBorderHorizontalDown(const Value: Boolean);
begin
  FDrawPageBorderHorizontalDown := Value;
  DoChange;
end;

procedure TdxPreviewBorderShadingControlHelper.SetDrawPageBorderHorizontalUp(const Value: Boolean);
begin
  FDrawPageBorderHorizontalUp := Value;
  DoChange;
end;

procedure TdxPreviewBorderShadingControlHelper.SetDrawPageBorderVerticalLeft(const Value: Boolean);
begin
  FDrawPageBorderVerticalLeft := Value;
  DoChange;
end;

procedure TdxPreviewBorderShadingControlHelper.SetDrawPageBorderVerticalRight(const Value: Boolean);
begin
  FDrawPageBorderVerticalRight := Value;
  DoChange;
end;

procedure TdxPreviewBorderShadingControlHelper.SetDrawParagraph(const Value: Boolean);
begin
  FDrawParagraph := Value;
  DoChange;
end;

procedure TdxPreviewBorderShadingControlHelper.SetFillColor(const Value: TdxAlphaColor);
begin
  FFillColor := Value;
  DoChange;
end;

procedure TdxPreviewBorderShadingControlHelper.SetHeight(const Value: Integer);
begin
  FHeight := Value;
end;

procedure TdxPreviewBorderShadingControlHelper.SetWidth(const Value: Integer);
begin
  FWidth := Value;
end;

{ TdxPreviewBorderViewInfo }

function TdxPreviewBorderViewInfo.GetBorder: TdxBorderInfo;
begin
  Result := FBorder;
end;

function TdxPreviewBorderViewInfo.GetBorderType: TdxBorderTypes;
begin
  Result := FBorderType;
end;

function TdxPreviewBorderViewInfo.GetBounds(ATableViewInfo: TdxTableViewInfo): TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

function TdxPreviewBorderViewInfo.GetConverter: TdxDocumentModelUnitToLayoutUnitConverter;
begin
  Result := nil;
end;

function TdxPreviewBorderViewInfo.GetEndCorner: TdxCornerViewInfoBase;
begin
  Result := FEndCorner;
end;

function TdxPreviewBorderViewInfo.GetStartCorner: TdxCornerViewInfoBase;
begin
  Result := FStartCorner;
end;

end.
