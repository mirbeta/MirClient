{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY                                          }
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

unit dxPScxCommon;

{$I cxVer.inc}

interface

uses
  Types, Classes, Windows, Messages, Graphics, Controls, StdCtrls, ExtCtrls, ImgList, Menus,
  Dialogs, ExtDlgs, cxClasses, cxControls, cxGraphics, cxLookAndFeels, cxStyles, cxLookAndFeelPainters, cxEdit,
  cxTextEdit, cxMemo, cxCheckBox, cxRadioGroup, cxBlobEdit, cxGroupBox, cxImage, cxImageComboBox, cxHyperLinkEdit,
  cxCurrencyEdit, dxPSGlbl, dxBase, dxPSCore, dxPSRes, cxDrawTextUtils, cxListBox, dxCore, dxPSReportRenderCanvas,
  cxGeometry, dxToggleSwitch, dxSparkline, dxPSReportLinkDesignWindow, dxLayoutControlAdapters, cxButtons,
  dxLayoutLookAndFeels, dxLayoutContainer, dxLayoutControl, dxTokenEdit, dxGDIPlusClasses, dxInputDialogs;

type
  TdxCustomcxControlReportLink = class;
  TdxCustomTableControlReportLink = class;
  TdxfmCustomcxControlReportLinkDesignWindow = class;

  TdxReportItemViewParams = record
    AdvancedViewParams: IUnknown;
    CellSides: TdxCellSides;
    FontSize: Integer;
    FontStyle: TFontStyles;
    Transparent: Boolean;
    NativeParams: TcxViewParams;
  end;


  IdxPSCellParams = interface
  ['{F0A495A1-1F0F-4245-A437-16E21ACF9FED}']
    function GetAutoHeight: Boolean;
    function GetCanvas: TdxPSReportRenderCustomCanvas;
    function GetDisplayGraphicsAsText: Boolean;
    function GetDisplayTrackBarsAsText: Boolean;
    function GetEndEllipsis: Boolean;
    function GetFlatCheckMarks: Boolean;
    function GetGraphicsText: string;
    function GetMultiline: Boolean;
    function GetTransparentGraphics: Boolean;

    property AutoHeight: Boolean read GetAutoHeight;
    property Canvas: TdxPSReportRenderCustomCanvas read GetCanvas;
    property DisplayGraphicsAsText: Boolean read GetDisplayGraphicsAsText;
    property DisplayTrackBarsAsText: Boolean read GetDisplayTrackBarsAsText;
    property EndEllipsis: Boolean read GetEndEllipsis;
    property FlatCheckMarks: Boolean read GetFlatCheckMarks;
    property GraphicsText: string read GetGraphicsText;
    property Multiline: Boolean read GetMultiline;
    property TransparentGraphics: Boolean read GetTransparentGraphics;
  end;

  IdxPSCellParams2 = interface
  ['{09EAB051-1AC2-46FB-A7F4-D6BFA883D015}']
    function GetPreviewMarginLeft: Integer;
    function GetPreviewMarginRight: Integer;
    function GetPreviewMaxHeight: Integer;
    function GetPreviewMaxLineCount: Integer;
    function GetRichEditGraphicClass: TGraphicClass;
    function GetRichEditTransparent: Boolean;

    property PreviewMarginLeft: Integer read GetPreviewMarginLeft;
    property PreviewMarginRight: Integer read GetPreviewMarginRight;
    property PreviewMaxHeight: Integer read GetPreviewMaxHeight;
    property PreviewMaxLineCount: Integer read GetPreviewMaxLineCount;
    property RichEditGraphicClass: TGraphicClass read GetRichEditGraphicClass;
    property RichEditTransparent: Boolean read GetRichEditTransparent;
  end;

  { Data Maps }

  TdxPSDataMapClass = class of TdxPSCustomDataMap;

  TdxPSDataMaps = class(TdxCustomClassMaps)
  private
    function GetMapClass(Properties: TcxCustomEditProperties): TdxPSDataMapClass;
  public
    class function Instance: TdxPSDataMaps; reintroduce; overload;

    function DoesItemParticipateInAutoHeightCalculation(AProperties: TcxCustomEditProperties): Boolean;
    function DoesItemParticipateInAutoWidthCalculation(AProperties: TcxCustomEditProperties): Boolean;
    function DoesItemParticipateInBestFitCalculation(AProperties: TcxCustomEditProperties): Boolean;
    procedure GetImageLists(AProperties: TcxCustomEditProperties; AProc: TdxPSGetImageListProc);
    procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil);
    function ItemClass(AProperties: TcxCustomEditProperties; AnIsPreview: Boolean = False): TdxReportCellDataClass; overload;
    function ItemClass(AProperties: TcxCustomEditProperties; const AViewParams: TdxReportItemViewParams; AIsPreview: Boolean = False): TdxReportCellDataClass; overload;
    function MeasureWidth(AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
      const ACellParams: IdxPSCellParams; AFont: TFont; ARecordIndex: Integer = 0): Integer;
    property MapClasses[Properties: TcxCustomEditProperties]: TdxPSDataMapClass read GetMapClass; default;
  end;

  { TdxPSCustomDataMap }

  TdxPSCustomDataMap = class(TdxCustomClassMapItem)
  protected
    class function DoesItemParticipateInAutoHeightCalculation(AProperties: TcxCustomEditProperties): Boolean; virtual;
    class function DoesItemParticipateInAutoWidthCalculation(AProperties: TcxCustomEditProperties): Boolean; virtual;
    class function DoesItemParticipateInBestFitCalculation(AProperties: TcxCustomEditProperties): Boolean; virtual;
    class procedure GetImageLists(AProperties: TcxCustomEditProperties; AProc: TdxPSGetImageListProc); virtual;
    class function GetText(AProperties: TcxCustomEditProperties; const AValue: TcxEditValue): string; virtual;
    class function HasText(AProperties: TcxCustomEditProperties; const AValue: TcxEditValue): Boolean; virtual;
    class procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); virtual;
    class function ItemClass(AProperties: TcxCustomEditProperties; AIsPreview: Boolean = False): TdxReportCellDataClass; overload; virtual;
    class function ItemClass(AProperties: TcxCustomEditProperties; const AViewParams: TdxReportItemViewParams; AIsPreview: Boolean = False): TdxReportCellDataClass; overload; virtual;
    class function MeasureWidth(AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
      const ACellParams: IdxPSCellParams; AFont: TFont; ARecordIndex: Integer = 0): Integer; virtual;
    class function PropertiesClass: TcxCustomEditPropertiesClass; virtual;
  public
    class function PairClass: TClass; override;

    class procedure Register;
    class procedure Unregister;
  end;

  { TdxPSTextDataMap }

  TdxPSTextDataMap = class(TdxPSCustomDataMap)
  protected
    class procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); override;
    class function ItemClass(AProperties: TcxCustomEditProperties; AnIsPreview: Boolean = False): TdxReportCellDataClass; override;
    class function PropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

  { TdxPSMemoDataMap }

  TdxPSMemoDataMap = class(TdxPSTextDataMap)
  protected
    class function DoesItemParticipateInAutoWidthCalculation(AProperties: TcxCustomEditProperties): Boolean; override;
    class function DoesItemParticipateInBestFitCalculation(AProperties: TcxCustomEditProperties): Boolean; override;
    class procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); override;
    class function PropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

  { TdxPSHyperLinkDataMap }

  TdxPSHyperLinkDataMap = class(TdxPSTextDataMap)
  protected
    class procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); override;
    class function PropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

  { TdxPSCheckDataMap }

  TdxPSCheckDataMap = class(TdxPSCustomDataMap)
  protected
    class procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); override;
    class function ItemClass(AProperties: TcxCustomEditProperties; AnIsPreview: Boolean = False): TdxReportCellDataClass; override;
    class function MeasureWidth(AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
      const ACellParams: IdxPSCellParams; AFont: TFont; ARecordIndex: Integer = 0): Integer; override;
    class function PropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

  { TdxPSToggleSwitchDataMap }

  TdxPSToggleSwitchDataMap = class(TdxPSCheckDataMap)
  protected
    class procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); override;
    class function PropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

  { TdxSparklineDataMap }

  TdxSparklineDataMap = class(TdxPSCustomDataMap)
  protected
    class procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); override;
    class function ItemClass(AProperties: TcxCustomEditProperties; AnIsPreview: Boolean = False): TdxReportCellDataClass; override;
    class function PropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

  { TdxTokenEditDataMap }

  TdxTokenEditDataMap = class(TdxPSCustomDataMap)
  strict private
    class function CalculateViewInfo(AProperties: TcxCustomEditProperties; ABounds: TRect; const AValue: TcxEditValue): TdxTokenEditViewInfo;
  protected
    class procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); override;
    class function ItemClass(AProperties: TcxCustomEditProperties; AnIsPreview: Boolean = False): TdxReportCellDataClass; override;
    class function PropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

  { TdxPSCustomButtonGroupDataMap }

  TdxPSCustomButtonGroupDataMap = class(TdxPSCustomDataMap)
  protected
    class function ButtonGroupClass(AProperties: TcxCustomEditProperties): TdxCustomReportButtonGroupClass; virtual;
    class procedure InitializeGroupButton(AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; AButton: TdxCustomReportCellCheck; AnIndex: Integer); virtual;

    class procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); override;
    class function ItemClass(AProperties: TcxCustomEditProperties; AnIsPreview: Boolean = False): TdxReportCellDataClass; override;
    class function MeasureWidth(AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
      const ACellParams: IdxPSCellParams; AFont: TFont; ARecordIndex: Integer = 0): Integer; override;
    class function PropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

  { TdxPSRadioButtonGroupDataMap }

  TdxPSRadioButtonGroupDataMap = class(TdxPSCustomButtonGroupDataMap)
  protected
    class function ButtonGroupClass(AProperties: TcxCustomEditProperties): TdxCustomReportButtonGroupClass; override;
    class procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); override;

    class function PropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

  { TdxPSGraphicDataMap }

  TdxPSGraphicDataMap = class(TdxPSCustomDataMap)
  protected
    class function DoesItemParticipateInAutoHeightCalculation(AProperties: TcxCustomEditProperties): Boolean; override;
    class function DefaultGraphicClass: TGraphicClass; virtual;
    class function GetGraphic(AProperties: TcxCustomEditProperties; AGraphicClass: TGraphicClass;
      const AValue: TcxEditValue): TGraphic; virtual;
    class function GetGraphicClass(AProperties: TcxCustomEditProperties; ARecordIndex: Integer; AOwner: TObject = nil): TGraphicClass; virtual;
    class function HasGraphic(const AValue: TcxEditValue): Boolean; virtual;
    class procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); override;
    class function ItemClass(AProperties: TcxCustomEditProperties; AnIsPreview: Boolean = False): TdxReportCellDataClass; override;
    class function MeasureWidth(AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
      const ACellParams: IdxPSCellParams; AFont: TFont; ARecordIndex: Integer = 0): Integer; override;
    class function PropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

  { TdxPSGraphicAsTextDataMap }

  TdxPSGraphicAsTextDataMap = class(TdxPSCustomDataMap)
  protected
    class procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); override;
    class function ItemClass(AProperties: TcxCustomEditProperties; AnIsPreview: Boolean = False): TdxReportCellDataClass; override;
    class function MeasureWidth(AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
      const ACellParams: IdxPSCellParams; AFont: TFont; ARecordIndex: Integer = 0): Integer; override;
    class function PropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

  { TdxPSImageDataMap }

  TdxPSImageDataMap = class(TdxPSTextDataMap)
  protected
    class function DoesItemParticipateInBestFitCalculation(AProperties: TcxCustomEditProperties): Boolean; override;
    class function GetComboBoxItem(AProperties: TcxCustomEditProperties; const AValue: TcxEditValue): TcxImageComboBoxItem; virtual;
    class function GetImageIndex(AProperties: TcxCustomEditProperties; const AValue: TcxEditValue): Integer; virtual;
    class procedure GetImageLists(AProperties: TcxCustomEditProperties; AProc: TdxPSGetImageListProc); override;
    class function GetImages(AProperties: TcxCustomEditProperties): TCustomImageList; virtual;
    class function GetText(AProperties: TcxCustomEditProperties; const AValue: TcxEditValue): string; override;
    class function HasImages(AProperties: TcxCustomEditProperties): Boolean; virtual;
    class procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); override;
    class function IsMultilinedText(AProperties: TcxCustomEditProperties): Boolean; virtual;
    class function ItemClass(AProperties: TcxCustomEditProperties; AnIsPreview: Boolean = False): TdxReportCellDataClass; override;
    class function MeasureWidth(AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
      const ACellParams: IdxPSCellParams; AFont: TFont; ARecordIndex: Integer = 0): Integer; override;
    class function PropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

  { TdxPSDelegateDataMap }

  TdxPSDelegateDataMap = class(TdxPSCustomDataMap)
  protected
    class function DataMapClass(AProperties: TcxCustomEditProperties): TdxPSDataMapClass; virtual;
    class function DoesItemParticipateInAutoHeightCalculation(AProperties: TcxCustomEditProperties): Boolean; override;
    class function DoesItemParticipateInAutoWidthCalculation(AProperties: TcxCustomEditProperties): Boolean; override;
    class function DoesItemParticipateInBestFitCalculation(AProperties: TcxCustomEditProperties): Boolean; override;
    class procedure GetImageLists(AProperties: TcxCustomEditProperties; AProc: TdxPSGetImageListProc); override;
    class procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); override;
    class function ItemClass(AProperties: TcxCustomEditProperties; AnIsPreview: Boolean = False): TdxReportCellDataClass; override;
    class function MeasureWidth(AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
      const ACellParams: IdxPSCellParams; AFont: TFont; ARecordIndex: Integer = 0): Integer; override;
  end;

  { TdxPSBlobDataMap }

  TdxPSBlobDataMap = class(TdxPSDelegateDataMap)
  protected
    class function DataMapClass(AProperties: TcxCustomEditProperties): TdxPSDataMapClass; override;
    class function PropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

  { TdxPSBlobDefaultDataMap }

  TdxPSBlobDefaultDataMap = class(TdxPSTextDataMap)
  protected
    class procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); override;
  end;

  { TdxPSBlobTextDataMap }

  TdxPSBlobTextDataMap = class(TdxPSTextDataMap)
  protected
    class function DoesItemParticipateInAutoWidthCalculation(AProperties: TcxCustomEditProperties): Boolean; override;
    class function DoesItemParticipateInBestFitCalculation(AProperties: TcxCustomEditProperties): Boolean; override;
    class procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); override;
  end;

  { TdxPSBlobPictureDataMap }

  TdxPSBlobPictureDataMap = class(TdxPSCustomDataMap)
  protected
    class procedure GetImageLists(AProperties: TcxCustomEditProperties; AProc: TdxPSGetImageListProc); override;
    class procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); override;
    class function MeasureWidth(AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
      const ACellParams: IdxPSCellParams; AFont: TFont; ARecordIndex: Integer = 0): Integer; override;
    class function ItemClass(AProperties: TcxCustomEditProperties; AnIsPreview: Boolean = False): TdxReportCellDataClass; override;
  end;

  { TdxReportCellPreviewText }

  TdxReportCellPreviewText = class(TdxReportCellString)
  strict private
    FMaxLineCount: Integer;
    FRightIndent: Integer;

    function GetLeftIndent: Integer;
    procedure SetLeftIndent(Value: Integer);
    procedure SetMaxLineCount(Value: Integer);
    procedure SetRightIndent(Value: Integer);
  protected
    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;
  public
    procedure Assign(Source: TPersistent); override;

    procedure DrawText(ACanvas: TdxPSReportRenderCustomCanvas); override;

    property LeftIndent: Integer read GetLeftIndent write SetLeftIndent;
    property MaxLineCount: Integer read FMaxLineCount write SetMaxLineCount;
    property PreventLeftTextExceed;
    property PreventTopTextExceed;
    property RightIndent: Integer read FRightIndent write SetRightIndent;
    property TextAlignX;
    property TextAlignY;
  end;

  { TdxReportCellSparklineImage }

  TdxReportCellSparklineImage = class(TdxReportCellImage)
  protected
    ViewData: TcxCustomEditViewData;
    ViewInfo: TcxCustomEditViewInfo;
    procedure BoundsChanged;  override;
  public
    destructor Destroy; override;
  end;

  { TdxReportCellToken }

  TdxReportCellToken = class
  strict private
    FOwner: TAbstractdxReportCellData;
  protected
    FBounds: TRect;
    FGlyph: TdxGPImage;
    FGlyphBounds: TRect;
    FText: string;
    FTextBounds: TRect;

    procedure AssignFromTokenCell(ASource: TdxReportCellToken);
    procedure AssignFromTokenItem(ASource: TdxTokenEditTokenViewInfo);
    procedure SetGlyph(AImage: TdxGPImage);
    procedure ReadData(AReader: TdxPSDataReader);
    procedure WriteData(AWriter: TdxPSDataWriter);
  public
    constructor Create(AOwner: TAbstractdxReportCellData);
    destructor Destroy; override;
    procedure Assign(ASource: TObject);
    procedure ConvertCoords(ANumerator, ADenominator: Integer);
    procedure Draw(ACanvas: TdxPSReportRenderCustomCanvas);
  end;

  { TdxReportCellTokens }

  TdxReportCellTokens = class(TAbstractdxReportCellData)
  strict private
    FTokens: TcxObjectList;

    function GetToken(Index: Integer): TdxReportCellToken;
    function GetTokenCount: Integer;
  protected
    procedure ConvertCoords(APixelsNumerator, APixelsDenominator: Integer); override;
    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;
  public
    constructor Create(AParent: TdxReportCell); override;
    destructor Destroy; override;
    function AddToken: TdxReportCellToken;
    procedure Assign(Source: TPersistent); override;
    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages); override;
    function MeasureContentHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    function MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    //
    property Tokens[Index: Integer]: TdxReportCellToken read GetToken;
    property TokenCount: Integer read GetTokenCount;
  end;

  { IdxReportLinkOptionsDesignerSupport }

  IdxReportLinkOptionsDesignerSupport = interface
  ['{98EBA10A-01A5-4011-ABA0-C52CE12F5553}']
    procedure DesignerFinalize;
    procedure DesignerInitialize;
    function DesignerTabIndex: Integer;
  end;

  { ReportLink Styles and StyleSheets }

  TdxCustomReportLinkStylesClass = class of TdxCustomReportLinkStyles;

  TdxCustomReportLinkStyles = class(TcxStyles, IdxReportLinkOptionsDesignerSupport)
  strict private
    function GetReportLink: TdxCustomcxControlReportLink;
  protected
    {IdxReportLinkOptionsDesignerSupport}
    procedure DesignerFinalize; virtual;
    procedure DesignerInitialize; virtual;
    function DesignerTabIndex: Integer; virtual;

    procedure Changed(AIndex: Integer); override;

    procedure GetDefaultViewParamsByCaption(const ACaption: string; AData: Pointer; out AParams: TcxViewParams); virtual;
    class function GetStyleCaption(AnIndex: Integer): string; virtual;
    function GetStyleByCaption(const Caption: string): TcxStyle; virtual;
    function GetStyleIndexByCaption(const Caption: string): Integer; virtual; abstract;
    procedure SetStyleByCaption(const Caption: string; Value: TcxStyle); virtual;

    property StyleIndexesByCaption[const Caption: string]: Integer read GetStyleIndexByCaption;
  public
    property ReportLink: TdxCustomcxControlReportLink read GetReportLink;
    property StylesByCaption[const Caption: string]: TcxStyle read GetStyleByCaption write SetStyleByCaption;
  end;

  TdxCustomReportLinkStyleSheetClass = class of TdxCustomReportLinkStyleSheet;

  TdxCustomReportLinkStyleSheet = class(TcxCustomStyleSheet)
  public
    class procedure Register;
    class procedure Unregister;
  end;

  { ReportLink Options }

  TdxCustomReportLinkOptionsClass = class of TdxCustomReportLinkOptions;

  TdxCustomReportLinkOptions = class(TPersistent, IUnknown,  IdxReportLinkOptionsDesignerSupport)
  private
    FReportLink: TdxCustomcxControlReportLink;
  protected
    { IUnknown }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; virtual; stdcall;
    { IdxReportLinkOptionsDesignerSupport }
    procedure DesignerFinalize; virtual;
    procedure DesignerInitialize; virtual;
    function DesignerTabIndex: Integer; virtual;

    procedure Changed; virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); virtual;

    procedure CreateObjects; virtual;
    procedure DestroyObjects; virtual;

    procedure DesignerModified;
  public
    constructor Create(AReportLink: TdxCustomcxControlReportLink); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; virtual;
    property ReportLink: TdxCustomcxControlReportLink read FReportLink;
  end;

  TdxCustomReportLinkOptionsExpandingClass = class of TdxCustomReportLinkOptionsExpanding;

  TdxCustomReportLinkOptionsExpanding = class(TdxCustomReportLinkOptions)
  end;

  TdxCustomReportLinkOptionsFormattingClass = class of TdxCustomReportLinkOptionsFormatting;

  TdxCustomReportLinkOptionsFormatting = class(TdxCustomReportLinkOptions)
  private
    FGridLineColor: TColor;
    FLookAndFeelKind: TcxLookAndFeelKind;
    FSuppressBackgroundBitmaps: Boolean;
    FUseLookAndFeelColors: Boolean;
    FUseNativeStyles: Boolean;
    procedure SetGridLineColor(Value: TColor);
    procedure SetLookAndFeelKind(Value: TcxLookAndFeelKind);
    procedure SetSuppressBackgroundBitmaps(Value: Boolean);
    procedure SetUseLookAndFeelColors(Value: Boolean);
    procedure SetUseNativeStyles(Value: Boolean);
  protected
    property UseLookAndFeelColors: Boolean read FUseLookAndFeelColors write SetUseLookAndFeelColors default False; // affects only in WinXP themed environment
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor default clDefault;
    property LookAndFeelKind: TcxLookAndFeelKind read FLookAndFeelKind write SetLookAndFeelKind default lfUltraFlat;
    property SuppressBackgroundBitmaps: Boolean read FSuppressBackgroundBitmaps write SetSuppressBackgroundBitmaps default False;
    property UseNativeStyles: Boolean read FUseNativeStyles write SetUseNativeStyles default False;
  end;

  TdxCustomReportLinkOptionsPaginationClass = class of TdxCustomReportLinkOptionsPagination;

  TdxCustomReportLinkOptionsPagination = class(TdxCustomReportLinkOptions)
  end;

  TdxCustomReportLinkOptionsRefinementsClass = class of TdxCustomReportLinkOptionsRefinements;

  TdxCustomReportLinkOptionsRefinements = class(TdxCustomReportLinkOptions)
  private
    FDisplayGraphicsAsText: Boolean;
    FDisplayTrackBarsAsText: Boolean;
    FFlatCheckMarks: Boolean;
    FGraphicsText: string;
    FIsGraphicsTextAssigned: Boolean;
    FTransparentGraphics: Boolean;
    FTransparentRichEdits: Boolean;
    function GetGraphicsText: string;
    function IsGraphicsTextStored: Boolean;
    procedure SetDisplayGraphicsAsText(Value: Boolean);
    procedure SetDisplayTrackBarsAsText(Value: Boolean);
    procedure SetFlatCheckMarks(Value: Boolean);
    procedure SetGraphicsText(const Value: string);
    procedure SetTransparentGraphics(Value: Boolean);
    procedure SetTransparentRichEdits(Value: Boolean);
    procedure ReadIsGraphicsTextAssigned(Reader: TReader);
    procedure WriteIsGraphicsTextAssigned(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AReportLink: TdxCustomcxControlReportLink); override;
    procedure Assign(Source: TPersistent); override;
    function DefaultGraphicsText: string; virtual;
    procedure RestoreDefaults; override;
  published
    property DisplayGraphicsAsText: Boolean read FDisplayGraphicsAsText write SetDisplayGraphicsAsText default False;
    property DisplayTrackBarsAsText: Boolean read FDisplayTrackBarsAsText write SetDisplayTrackBarsAsText default True;
    property FlatCheckMarks: Boolean read FFlatCheckMarks write SetFlatCheckMarks default True;
    property GraphicsText: string read GetGraphicsText write SetGraphicsText stored IsGraphicsTextStored;
    property TransparentGraphics: Boolean read FTransparentGraphics write SetTransparentGraphics default False;
    property TransparentRichEdits: Boolean read FTransparentRichEdits write SetTransparentRichEdits default False; // you can expect slow painting when True
  end;

  TdxCustomReportLinkOptionsSizeClass = class of TdxCustomReportLinkOptionsSize;

  TdxCustomReportLinkOptionsSize = class(TdxCustomReportLinkOptions)
  private
    FAutoWidth: Boolean;
    procedure SetAutoWidth(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth default False;
  end;

  TdxCustomReportLinkOptionsViewClass = class of TdxCustomReportLinkOptionsView;

  TdxCustomReportLinkOptionsView = class(TdxCustomReportLinkOptions)
  end;

  { ReportLink }

  TdxStyleSheetHasCaptionProc = function(const ACaption: string): Boolean of object;

  TdxCustomcxControlReportLink = class(TBasedxReportLink)
  private
    FDelimitersHorz: TList;
    FDelimitersVert: TList;
    FDesignerTabIndex: Integer;
    FImageLists: TList;
    FOptions: TList;
    FOptionsExpanding: TdxCustomReportLinkOptionsExpanding;
    FOptionsFormatting: TdxCustomReportLinkOptionsFormatting;
    FOptionsPagination: TdxCustomReportLinkOptionsPagination;
    FOptionsRefinements: TdxCustomReportLinkOptionsRefinements;
    FOptionsSize: TdxCustomReportLinkOptionsSize;
    FOptionsView: TdxCustomReportLinkOptionsView;
    FScreenCanvas: TdxPSReportRenderCustomCanvas;
    FStyleRepository: TcxStyleRepository;
    FStyles: TdxCustomReportLinkStyles;
    FSupportedCustomDraw: Boolean;
    function GetActiveStyles: TdxCustomReportLinkStyles;
    function GetDesignWindow: TdxfmCustomcxControlReportLinkDesignWindow;
    function GetEffects3D: Boolean;
    function GetImageList(Index: Integer): TCustomImageList;
    function GetImageListCount: Integer;
    function GetOptions(Index: Integer): TdxCustomReportLinkOptions;
    function GetOptionsCount: Integer;
    function GetSoft3D: Boolean;
    procedure SetOptionsExpanding(Value: TdxCustomReportLinkOptionsExpanding);
    procedure SetOptionsFormatting(Value: TdxCustomReportLinkOptionsFormatting);
    procedure SetOptionsPagination(Value: TdxCustomReportLinkOptionsPagination);
    procedure SetOptionsRefinements(Value: TdxCustomReportLinkOptionsRefinements);
    procedure SetOptionsSize(Value: TdxCustomReportLinkOptionsSize);
    procedure SetOptionsView(Value: TdxCustomReportLinkOptionsView);
    procedure SetStyleRepository(Value: TcxStyleRepository);
    procedure SetStyles(Value: TdxCustomReportLinkStyles);
    procedure SetSupportedCustomDraw(Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure BeforeDesignReport; override;
    procedure ConvertCoords; override;
    procedure GetImageLists(AProc: TdxPSGetImageListProc); override;
    procedure InternalRestoreDefaults; override;
    function IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean; override;
    procedure MakeDelimiters(AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList); override;

    procedure AddHorizontalDelimiter(ADelimiter: TdxReportCell); overload;
    procedure AddHorizontalDelimiter(ADelimiter: Integer); overload;
    procedure AddVerticalDelimiter(ADelimiter: TdxReportCell); overload;
    procedure AddVerticalDelimiter(ADelimiter: Integer); overload;

    procedure CreateOptions; virtual;
    procedure DestroyOptions; virtual;
    function GetOptionsExpandingClass: TdxCustomReportLinkOptionsExpandingClass; virtual;
    function GetOptionsFormattingClass: TdxCustomReportLinkOptionsFormattingClass; virtual;
    function GetOptionsPaginationClass: TdxCustomReportLinkOptionsPaginationClass; virtual;
    function GetOptionsRefinementsClass: TdxCustomReportLinkOptionsRefinementsClass; virtual;
    function GetOptionsSizeClass: TdxCustomReportLinkOptionsSizeClass; virtual;
    function GetOptionsViewClass: TdxCustomReportLinkOptionsViewClass; virtual;
    procedure OptionsChanged(AnOptions: TdxCustomReportLinkOptions); virtual;

    procedure AddOptions(AnOptions: TdxCustomReportLinkOptions);
    procedure NotifyOptions(AComponent: TComponent; AOperation: TOperation);
    procedure RemoveOptions(AnOptions: TdxCustomReportLinkOptions);

    procedure AppendImageList(AnImageList: TCustomImageList);
    function CanCreateComponent: Boolean;
    procedure ConvertDelimiters(ADelimiters: TList);
    function CreateStyle(const ACaption: string): TcxStyle;
    function CreateStyleRepository: TcxStyleRepository;
    function CreateStyleSheet(APrototype: TcxCustomStyleSheet;
      const ACaption: string; AUseStyles: Boolean): TdxCustomReportLinkStyleSheet;
    function FindStyleRepositoryInStyles(AStyles: TcxStyles): TcxStyleRepository;
    function GetChildComponentOwner: TComponent;
    function GetStyleConsumerCount(AStyle: TcxStyle): Integer;
    function GetStyleRepository: TcxStyleRepository; virtual;
    function GetStyleSheetCaption(ACheckProc: TdxStyleSheetHasCaptionProc; var ACaption: string): Boolean;
    function InitiateStyle(const ACaption: string; var AStyle: TcxStyle; AForceCreation: Boolean): Boolean;
    procedure StyleRestoreDefaults(const ACaption: string; AStyle: TcxStyle);
    procedure StyleChanged(const ACaption: string; AStyle: TcxStyle); virtual;

    function GetAreNativeStylesAvailable: Boolean; virtual;
    function GetStylesClass: TdxCustomReportLinkStylesClass; virtual;
    function GetStyleSheetClass: TdxCustomReportLinkStyleSheetClass; virtual;
    function GetStyleSheetPrototype: TdxCustomReportLinkStyleSheet; virtual;

    procedure PrepareConstruct; virtual;
    procedure UnprepareConstruct; virtual;

    property ActiveStyles: TdxCustomReportLinkStyles read GetActiveStyles;
    property AreNativeStylesAvailable: Boolean read GetAreNativeStylesAvailable;
    property DelimitersHorz: TList read FDelimitersHorz;
    property DelimitersVert: TList read FDelimitersVert;
    property DesignerTabIndex: Integer read FDesignerTabIndex write FDesignerTabIndex;
    property Effects3D: Boolean read GetEffects3D;
    property ImageListCount: Integer read GetImageListCount;
    property ImageLists[Index: Integer]: TCustomImageList read GetImageList;
    property Options[Index: Integer]: TdxCustomReportLinkOptions read GetOptions;
    property OptionsCount: Integer read GetOptionsCount;
    property ScreenCanvas: TdxPSReportRenderCustomCanvas read FScreenCanvas;
    property Soft3D: Boolean read GetSoft3D;
    property StyleRepository: TcxStyleRepository read FStyleRepository write SetStyleRepository;
    property Styles: TdxCustomReportLinkStyles read FStyles write SetStyles;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property DesignWindow: TdxfmCustomcxControlReportLinkDesignWindow read GetDesignWindow;
    property OptionsExpanding: TdxCustomReportLinkOptionsExpanding read FOptionsExpanding write SetOptionsExpanding;
    property OptionsFormatting: TdxCustomReportLinkOptionsFormatting read FOptionsFormatting write SetOptionsFormatting;
    property OptionsPagination: TdxCustomReportLinkOptionsPagination read FOptionsPagination write SetOptionsPagination;
    property OptionsRefinements: TdxCustomReportLinkOptionsRefinements read FOptionsRefinements write SetOptionsRefinements;
    property OptionsSize: TdxCustomReportLinkOptionsSize read FOptionsSize write SetOptionsSize;
    property OptionsView: TdxCustomReportLinkOptionsView read FOptionsView write SetOptionsView;
    property SupportedCustomDraw: Boolean read FSupportedCustomDraw write SetSupportedCustomDraw default False;
  end;

  { TableReportLink Options }

  TdxCustomTableControlReportLinkOptionsClass = class of TdxCustomTableControlReportLinkOptions;

  TdxCustomTableControlReportLinkOptions = class(TdxCustomReportLinkOptions)
  private
    function GetReportLink: TdxCustomTableControlReportLink;
  public
    property ReportLink: TdxCustomTableControlReportLink read GetReportLink;
  end;

  TdxCustomTableControlReportLinkOptionsOnEveryPageClass = class of TdxCustomTableControlReportLinkOptionsOnEveryPage;

  TdxCustomTableControlReportLinkOptionsOnEveryPage = class(TdxCustomTableControlReportLinkOptions)
  private
    FBandHeaders: Boolean;
    FFooters: Boolean;
    FHeaders: Boolean;
    procedure SetBandHeaders(Value: Boolean);
    procedure SetFooters(Value: Boolean);
    procedure SetHeaders(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property BandHeaders: Boolean read FBandHeaders write SetBandHeaders default True;
    property Footers: Boolean read FFooters write SetFooters default True;
    property Headers: Boolean read FHeaders write SetHeaders default True;
  end;

  TdxCustomTableControlReportLinkOptionsPaginationClass = class of TdxCustomTableControlReportLinkOptionsPagination;

  TdxCustomTableControlReportLinkOptionsPagination = class(TdxCustomReportLinkOptionsPagination)
  private
    FBand: Boolean;
    FCustom: Boolean;
    function GetColumn: Boolean;
    function GetRow: Boolean;
    procedure SetBand(Value: Boolean);
    procedure SetColumn(Value: Boolean);
    procedure SetCustom(Value: Boolean);
    procedure SetRow(Value: Boolean);
  protected
    property Band: Boolean read FBand write SetBand default False;
    property Row: Boolean read GetRow write SetRow default True;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
    property Column: Boolean read GetColumn write SetColumn default True;
    property Custom: Boolean read FCustom write SetCustom default False;
  end;

  TdxCustomTableControlReportLinkOptionsPreviewClass = class of TdxCustomTableControlReportLinkOptionsPreview;

  TdxCustomTableControlReportLinkOptionsPreview = class(TdxCustomTableControlReportLinkOptions)
  private
    FAutoHeight: Boolean;
    FMaxLineCount: Integer;
    FVisible: Boolean;
    procedure SetAutoHeight(Value: Boolean);
    procedure SetMaxLineCount(Value: Integer);
    procedure SetVisible(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default True;
    property MaxLineCount: Integer read FMaxLineCount write SetMaxLineCount default 0;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TdxCustomTableControlReportLinkOptionsSelectionClass = class of TdxCustomTableControlReportLinkOptionsSelection;

  TdxCustomTableControlReportLinkOptionsSelection = class(TdxCustomTableControlReportLinkOptions)
  private
    FProcessExactSelection: Boolean;
    FProcessSelection: Boolean;
    procedure SetProcessExactSelection(Value: Boolean);
    procedure SetProcessSelection(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property ProcessExactSelection: Boolean read FProcessExactSelection write SetProcessExactSelection default False;
    property ProcessSelection: Boolean read FProcessSelection write SetProcessSelection default False;
  end;

  TdxCustomTableControlReportLinkOptionsViewClass = class of TdxCustomTableControlReportLinkOptionsView;

  TdxCustomTableControlReportLinkOptionsView = class(TdxCustomReportLinkOptionsView)
  private
    FBandHeaders: Boolean;
    FExpandButtons: Boolean;
    FFooters: Boolean;
    FHeaders: Boolean;
    procedure SetBandHeaders(Value: Boolean);
    procedure SetExpandButtons(Value: Boolean);
    procedure SetFooters(Value: Boolean);
    procedure SetHeaders(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
    property BandHeaders: Boolean read FBandHeaders write SetBandHeaders default True;
    property ExpandButtons: Boolean read FExpandButtons write SetExpandButtons default True;
  published
    property Footers: Boolean read FFooters write SetFooters default True;
    property Headers: Boolean read FHeaders write SetHeaders default True;
  end;

  { ReportLink }

  TdxCustomTableControlReportLink = class(TdxCustomcxControlReportLink)
  private
    FOptionsOnEveryPage: TdxCustomTableControlReportLinkOptionsOnEveryPage;
    FOptionsPreview: TdxCustomTableControlReportLinkOptionsPreview;
    FOptionsSelection: TdxCustomTableControlReportLinkOptionsSelection;
    function GetOptionsPagination: TdxCustomTableControlReportLinkOptionsPagination;
    function GetOptionsView: TdxCustomTableControlReportLinkOptionsView;
    procedure SetOptionsOnEveryPage(Value: TdxCustomTableControlReportLinkOptionsOnEveryPage);
    procedure SetOptionsPagination(Value: TdxCustomTableControlReportLinkOptionsPagination);
    procedure SetOptionsPreview(Value: TdxCustomTableControlReportLinkOptionsPreview);
    procedure SetOptionsSelection(Value: TdxCustomTableControlReportLinkOptionsSelection);
    procedure SetOptionsView(Value: TdxCustomTableControlReportLinkOptionsView);
  protected
    procedure InternalRestoreDefaults; override;

    procedure CreateOptions; override;
    procedure DestroyOptions; override;
    function GetOptionsOnEveryPageClass: TdxCustomTableControlReportLinkOptionsOnEveryPageClass; virtual;
    function GetOptionsPaginationClass: TdxCustomReportLinkOptionsPaginationClass; override;
    function GetOptionsPreviewClass: TdxCustomTableControlReportLinkOptionsPreviewClass; virtual;
    function GetOptionsSelectionClass: TdxCustomTableControlReportLinkOptionsSelectionClass; virtual;
    function GetOptionsViewClass: TdxCustomReportLinkOptionsViewClass; override;
  public
    procedure Assign(Source: TPersistent); override;
    property OptionsOnEveryPage: TdxCustomTableControlReportLinkOptionsOnEveryPage read FOptionsOnEveryPage write SetOptionsOnEveryPage;
    property OptionsPagination: TdxCustomTableControlReportLinkOptionsPagination read GetOptionsPagination write SetOptionsPagination;
    property OptionsPreview: TdxCustomTableControlReportLinkOptionsPreview read FOptionsPreview write SetOptionsPreview;
    property OptionsSelection: TdxCustomTableControlReportLinkOptionsSelection read FOptionsSelection write SetOptionsSelection;
    property OptionsView: TdxCustomTableControlReportLinkOptionsView read GetOptionsView write SetOptionsView;
  end;

  { Design Window }

  TdxfmCustomcxControlReportLinkDesignWindow = class(TStandarddxReportLinkDesignWindow)
  private
    function GetActiveStyleSheet: TcxCustomStyleSheet;
    function GetAreNativeStylesAvailable: Boolean;
    function GetReportLink: TdxCustomcxControlReportLink;
    function GetStyleRepository: TcxStyleRepository;
    procedure SetActiveStyleSheet(Value: TcxCustomStyleSheet);
    procedure WMActivate(var message: TWMActivate); message WM_ACTIVATE;
  protected
    procedure DoInitialize; override;

    function CanCopyStyleSheet: Boolean; virtual;
    function CanCreateStyleSheet: Boolean; virtual;
    function CanDeleteStyleSheet: Boolean; virtual;
    function CanRenameStyleSheet: Boolean; virtual;
    function CanSaveStyles: Boolean; virtual;

    function PerformStyleSheetCopy: Boolean;
    function PerformStyleSheetDelete: Boolean;
    procedure PerformStyleSheetDrawItem(ACanvas: TCanvas; AnIndex: Integer; R: TRect; AState: TOwnerDrawState; AEnabled: Boolean);
    function PerformStyleSheetKeyDown(Sender: TObject; var AKey: Word; AShift: TShiftState): Boolean;
    function PerformStyleSheetNew: Boolean;
    function PerformStyleSheetRename: Boolean;

    function PerformStylesChangeBitmap: Boolean;
    function PerformStylesChangeColor: Boolean;
    function PerformStylesChangeFont: Boolean;
    function PerformStylesClearBitmap: Boolean;
    function PerformStylesRestoreDefaults: Boolean;
    function PerformStylesSaveAsStyleSheet: Boolean;
    procedure RefreshStyleSheetList;
    procedure RefreshStylesList;

    // next methods can be overriden
    procedure DoActiveStyleSheetChanged; dynamic;
    procedure DoFormActivated(AnActive: Boolean); dynamic;
    procedure DoRefreshStylesList; dynamic;
    procedure DoStyleChanged(const ACaption: string; AStyle: TcxStyle); dynamic;
    procedure DoStylesChanged(AStrings: TStrings; ARecreate: Boolean); dynamic;

    function GetDesignerTabIndex: Integer; virtual;
    procedure SetDesignerTabIndex(Value: Integer); virtual;

    // next methods must be overriden
    procedure GetSelectedStyleNames(AStrings: TStrings); virtual;
    procedure GetStyleNames(out AStrings: TStrings); virtual;
    procedure GetStyleSheetNames(out AStrings: TStrings); virtual;

    function GetStyleConsumerCount(AStyle: TcxStyle): Integer;
    function GetStyleSheetCaption(var ACaption: string): Boolean;
    function HasStyleSheetWithCaption(const ACaption: string): Boolean;
    procedure InitiateStyle(const ACaption: string; var AStyle: TcxStyle; AForceCreation: Boolean);

    property ActiveStyleSheet: TcxCustomStyleSheet read GetActiveStyleSheet write SetActiveStyleSheet;
    property AreNativeStylesAvailable: Boolean read GetAreNativeStylesAvailable;
    property DesignerTabIndex: Integer read GetDesignerTabIndex write SetDesignerTabIndex;
    property ReportLink: TdxCustomcxControlReportLink read GetReportLink;
    property StyleRepository: TcxStyleRepository read GetStyleRepository;
  end;

  { Styles ListBox with ToolTips Support }

  TdxStyleBarViewInfo = record
    BarColor: TColor;
    BarFont: TFont;
    BarStyleColorBoxFrameColor: TColor;
    ScaleFactor: TdxScaleFactor;
    StyleBitmap: TBitmap;
    StyleBitmapOrg: TPoint;
    StyleCaption: string;
    StyleCaptionBoxBounds: TRect;
    StyleColor: TColor;
    StyleColorBoxBounds: TRect;
    StyleColorBoxContentBounds: TRect;
    StyleFont: TFont;
    StyleFontScaleFactor: TdxScaleFactor;
    StyleFontInfoBoxBounds: TRect;
    StyleTextColor: TColor;
    MaxWidth: Integer;
    RestSpaceBounds: TRect;
  end;

  TdxStylesListBox = class;

  TdxStylesListBoxToolTipsWindow = class(TCustomControl)
  private
    FStyleIndex: Integer;
    function GetListBox: TdxStylesListBox;
    procedure WMEraseBkgnd(var message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCHitTest(var message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;

    procedure CalculateStyleBarViewInfo(const R: TRect; var AViewInfo: TdxStyleBarViewInfo); virtual;
    procedure DrawBorder(DC: HDC; const R: TRect); virtual;
    procedure DrawStyle(R: TRect); virtual;
  public
    procedure Activate(const R: TRect; AnIndex: Integer);
    procedure Deactivate;

    property ListBox: TdxStylesListBox read GetListBox;
    property StyleIndex: Integer read FStyleIndex;
  end;

  { TdxStylesListBox }

  TdxStylesListBox = class(TcxListBox)
  strict private
    FHotTrackStyleIndex: Integer;
    FReportLinkStyles: TdxCustomReportLinkStyles;
    FToolTips: Boolean;
    FToolTipsLongHideTimer: TTimer;
    FToolTipsShortHideTimer: TTimer;
    FToolTipsWindow: TdxStylesListBoxToolTipsWindow;

    function GetHotTrackStyleBounds: TRect;
    function GetHotTrackStyleName: string;
    function GetSelectedStyle: TcxStyle;
    function GetStyle(Index: Integer): TcxStyle;
    function GetStyleLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    function GetToolTipsBounds: TRect;
    procedure SetReportLinkStyles(Value: TdxCustomReportLinkStyles);
    procedure SetToolTips(Value: Boolean);

    procedure ToolTipsLongHideTimerHandler(Sender: TObject);
    procedure ToolTipsShortHideTimerHandler(Sender: TObject);
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  protected
    function CreateTimer(AInterval: Integer; ATimerEvent: TNotifyEvent): TTimer;
    procedure DoMeasureItem(AControl: TcxListBox; AIndex: Integer; var Height: Integer); virtual;
    function DrawItem(ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState): Boolean; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure CalculateStyleBarViewInfoSizes(AnIndex: Integer; const R: TRect;
      AScaleFactor: TdxScaleFactor; var AViewInfo: TdxStyleBarViewInfo); virtual;
    procedure CalculateStyleBarViewInfoViewParams(const ACaption: string; AStyle: TcxStyle;
      const R: TRect; ASelected: Boolean; var AViewInfo: TdxStyleBarViewInfo); virtual;
    procedure CalculateStyleBarViewInfo(const ACaption: string; AnIndex: Integer; AStyle: TcxStyle;
      const R: TRect; ASelected: Boolean; AScaleFactor: TdxScaleFactor; var AViewInfo: TdxStyleBarViewInfo);
    function GetStyleBarItemColor(ASelected: Boolean): TColor; virtual;
    function GetStyleBarItemTextColor(ASelected: Boolean; AColor, ABkColor: TColor): TColor; virtual;
    function GetStyleFontByText(const AText: string; out AFont: TFont; out AScaleFactor: TdxScaleFactor): Boolean;

    function AreToolTipsNeeded: Boolean;
    function FindHotTrackStyle(const Pt: TPoint): Integer;
    function IsHotTrackStyleViolateControlBounds: Boolean;
    function IsMouseOver: Boolean;
    procedure ShowToolTips;
    procedure UpdateToolTips(const Pt: TPoint);
    //
    property HotTrackStyleBounds: TRect read GetHotTrackStyleBounds;
    property HotTrackStyleIndex: Integer read FHotTrackStyleIndex write FHotTrackStyleIndex;
    property HotTrackStyleName: string read GetHotTrackStyleName;
    property ToolTipsBounds: TRect read GetToolTipsBounds;
    property ToolTipsWindow: TdxStylesListBoxToolTipsWindow read FToolTipsWindow;
  public
    constructor Create(AOwner: TComponent); override;
    function IndexOfStyle(AStyle: TcxStyle): Integer;
    procedure HideToolTips;
    //
    property ReportLinkStyles: TdxCustomReportLinkStyles read FReportLinkStyles write SetReportLinkStyles;
    property StyleLookAndFeelPainter: TcxCustomLookAndFeelPainter read GetStyleLookAndFeelPainter;
    property Styles[Index: Integer]: TcxStyle read GetStyle;
    property SelectedStyle: TcxStyle read GetSelectedStyle;
  published
    property ToolTips: Boolean read FToolTips write SetToolTips default True;
  end;

{ Helpers }

function cxButtonGroupItem_GetCaption(AnItem: TcxButtonGroupItem): string;

function cxBlobEditProperties_GetEditKind(AProperties: TcxCustomEditProperties): TcxBlobEditKind;
function cxBlobEditProperties_GetPaintStyle(AProperties: TcxCustomEditProperties): TcxBlobPaintStyle;

function cxButtonGroupProperties_GetColumnCount(AProperties: TcxCustomEditProperties): Integer;
function cxButtonGroupProperties_GetItems(AProperties: TcxCustomEditProperties): TcxButtonGroupItems;

function cxCheckBoxProperties_GetAlignment(AProperties: TcxCustomEditProperties): TAlignment;
function cxCheckBoxProperties_GetGlyph(AProperties: TcxCustomEditProperties): TdxSmartGlyph;
function cxCheckBoxProperties_GetGlyphCount(AProperties: TcxCustomEditProperties): Integer;
function cxCheckBoxProperties_GetIsMultilined(AProperties: TcxCustomEditProperties): Boolean;
function cxCheckBoxProperties_GetNullStyle(AProperties: TcxCustomEditProperties): TcxCheckBoxNullValueShowingStyle;

function cxCurrencyProperties_GetNullString(AProperties: TcxCustomEditProperties): string;

function cxEditProperties_GetAlignment(AProperties: TcxCustomEditProperties): TcxEditAlignment;

function cxHyperLinkEditProperties_GetLinkColor(AProperties: TcxCustomEditProperties): TColor;

function cxMemoProperties_GetAlignment(AProperties: TcxCustomEditProperties): TAlignment;
function cxMemoProperties_GetWordWrap(AProperties: TcxCustomEditProperties): Boolean;

function cxImageComboBoxProperties_GetDefaultDescription(AProperties: TcxCustomEditProperties): string;
function cxImageComboBoxProperties_GetDefaultImageIndex(AProperties: TcxCustomEditProperties): Integer;
function cxImageComboBoxProperties_FindItemByValue(AProperties: TcxCustomEditProperties; const AValue: TcxEditValue): TcxImageComboBoxItem;
function cxImageComboBoxProperties_GetImageAlignment(AProperties: TcxCustomEditProperties): TcxImageAlign;
function cxImageComboBoxProperties_GetImages(AProperties: TcxCustomEditProperties): TCustomImageList;
function cxImageComboBoxProperties_GetIsMultilined(AProperties: TcxCustomEditProperties): Boolean;
function cxImageComboBoxProperties_GetItems(AProperties: TcxCustomEditProperties): TcxImageComboBoxItems;
function cxImageComboBoxProperties_GetShowDescription(AProperties: TcxCustomEditProperties): Boolean;

function cxImageProperties_GetCaption(AProperties: TcxCustomEditProperties): string;
function cxImageProperties_GetCenter(AProperties: TcxCustomEditProperties): Boolean;
function cxImageProperties_GetProportional(AProperties: TcxCustomEditProperties): Boolean;
function cxImageProperties_GetStretch(AProperties: TcxCustomEditProperties): Boolean;
function cxImageProperties_GetGraphicClass(AProperties: TcxCustomEditProperties; ARecordIndex: Integer; AOwner: TObject = nil): TGraphicClass;

function cxRadioGroupProperties_GetDisplayText(AProperties: TcxCustomEditProperties; const AValue: TcxEditValue): string;

{ Utilities }

function dxPSDataMaps: TdxPSDataMaps;
procedure dxPSDrawStyleBar(ACanvas: TCanvas; R: TRect; const AViewInfo: TdxStyleBarViewInfo);
function dxPSMakecxGridLines(AHorizontal, AVertical: Boolean): TcxGridLines;
function dxPSMeasureTextWidth(ACanvas: TdxPSReportRenderCustomCanvas; const AText: string; AFont: TFont): Integer;
function dxPSPreviewCarLogos(Index: Integer): TPicture;
function dxPSPreviewCarLogosAsString(Index: Integer): AnsiString;
function dxPSPictureDialog: TOpenPictureDialog;
procedure dxPSResetStyles(AStyles: TcxStyles);

const
  dxPSPreviewCarLogoCount = 5;
  dxPSPreviewCarLogoWidth = 60;
  dxPSPreviewCarLogoHeight = 30;

  CellSidesMap: array[TcxGridLines] of TdxCellSides = (csAll, [], csLeftRight, csTopBottom);
  EdgeStyleMap: array[Boolean] of TdxCheckButtonEdgeStyle = (cbes3D, cbesUltraFlat);
  EditTextAlignXMap: array[TcxEditHorzAlignment] of TcxTextAlignX = (taLeft, taRight, taCenterX);
  EditTextAlignYMap: array[TcxEditVertAlignment] of TcxTextAlignY = (taTop, taBottom, taCenterY);
  HeaderImageLayoutMap: array[TAlignment, TcxAlignmentVert] of TdxImageLayout =
    ((ilImageTopLeft, ilImageBottomLeft, ilImageCenterLeft),
     (ilImageTopRight, ilImageBottomRight, ilImageCenterRight),
     (ilImageTopCenter, ilImageBottomCenter, ilImageCenterCenter));
  ImageLayoutMap: array[TcxImageAlign] of TdxImageLayout = (ilImageCenterLeft, ilImageCenterRight);
  TextAlignXMap: array[TAlignment] of TcxTextAlignX = (taLeft, taRight, taCenterX);
  TextAlignYMap: array[TcxAlignmentVert] of TcxTextAlignY = (taTop, taBottom, taCenterY);

implementation

uses
  Themes, Variants, SysUtils, Forms, Math, dxThemeManager, cxContainer, cxVariants,
  cxDataUtils, dxPSUtl, dxPSEngn, dxPSImgs, dxExtCtrls, dxDPIAwareUtils;

{$R *.dfm}

const
  ColorBarWidth: Integer = 60;

var
  FPicture: TPicture;
  FPictureDialog: TOpenPictureDialog;
  FPreviewPictures: array[0..dxPSPreviewCarLogoCount - 1] of TPicture;

type
  TcxCustomStylesAccess = class(TcxCustomStyles);
  TcxButtonGroupItemAccess = class(TcxButtonGroupItem);
  TcxCustomBlobEditPropertiesAccess = class(TcxCustomBlobEditProperties);
  TcxCustomButtonGroupPropertiesAccess = class(TcxCustomButtonGroupProperties);
  TcxCustomCheckBoxPropertiesAccess = class(TcxCustomCheckBoxProperties);
  TcxCustomCurrencyEditPropertiesAccess = class(TcxCustomCurrencyEditProperties);
  TcxCustomEditPropertiesAccess = class(TcxCustomEditProperties);
  TcxCustomHyperLinkEditPropertiesAccess = class(TcxCustomHyperLinkEditProperties);
  TcxCustomMemoPropertiesAccess = class(TcxCustomMemoProperties);
  TcxCustomImageComboBoxPropertiesAccess = class(TcxCustomImageComboBoxProperties);
  TcxCustomImagePropertiesAccess = class(TcxCustomImageProperties);
  TcxCustomRadioGroupPropertiesAccess = class(TcxCustomRadioGroupProperties);
  TcxOffice11LookAndFeelPainterAccess = class(TcxOffice11LookAndFeelPainter);
  TdxToggleSwitchViewInfoAccess = class(TdxCustomToggleSwitchViewInfo);
  TdxTokenEditTokenViewInfoAccess = class(TdxTokenEditTokenViewInfo);
  TdxTokenEditViewInfoAccess = class(TdxTokenEditViewInfo);

{ Helpers }

{ TcxStyles Helpers }

function cxStyles_GetCount(AInstance: TcxCustomStyles): Integer;
begin
  Result := TcxCustomStylesAccess(AInstance).Count;
end;

procedure cxStyles_ResetStyles(AInstance: TcxCustomStyles);
begin
  TcxCustomStylesAccess(AInstance).ResetStyles;
end;

function cxStyles_GetStyleSheet(AInstance: TcxCustomStyles): TcxCustomStyleSheet;
begin
  Result := TcxCustomStylesAccess(AInstance).StyleSheet;
end;

procedure cxStyles_SetStyleSheet(AInstance: TcxCustomStyles; Value: TcxCustomStyleSheet);
begin
  TcxCustomStylesAccess(AInstance).StyleSheet := Value;
end;

{ ButtonGroupItem Helpers }

function cxButtonGroupItem_GetCaption(AnItem: TcxButtonGroupItem): string;
begin
  Result := TcxButtonGroupItemAccess(AnItem).Caption;
end;

{ BlobEditProperties Helpers }

function cxBlobEditProperties_GetEditKind(AProperties: TcxCustomEditProperties): TcxBlobEditKind;
begin
  Result := TcxCustomBlobEditPropertiesAccess(AProperties).BlobEditKind;
end;

function cxBlobEditProperties_GetPaintStyle(AProperties: TcxCustomEditProperties): TcxBlobPaintStyle;
begin
  Result := TcxCustomBlobEditPropertiesAccess(AProperties).BlobPaintStyle;
end;

{ ButtonGroupProperties Helpers }

function cxButtonGroupProperties_GetColumnCount(AProperties: TcxCustomEditProperties): Integer;
begin
  Result := TcxCustomButtonGroupPropertiesAccess(AProperties).Columns;
end;

function cxButtonGroupProperties_GetItems(AProperties: TcxCustomEditProperties): TcxButtonGroupItems;
begin
  Result := TcxCustomButtonGroupPropertiesAccess(AProperties).Items;
end;

{ CheckBoxProperties Helpers }

function cxCheckBoxProperties_GetAlignment(AProperties: TcxCustomEditProperties): TAlignment;
begin
  Result := TcxCustomCheckBoxPropertiesAccess(AProperties).Alignment;
end;

function cxCheckBoxProperties_GetGlyph(AProperties: TcxCustomEditProperties): TdxSmartGlyph;
begin
  Result := TcxCustomCheckBoxPropertiesAccess(AProperties).Glyph;
end;

function cxCheckBoxProperties_GetGlyphCount(AProperties: TcxCustomEditProperties): Integer;
begin
  Result := TcxCustomCheckBoxPropertiesAccess(AProperties).GlyphCount;
end;

function cxCheckBoxProperties_GetIsMultilined(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := TcxCustomCheckBoxPropertiesAccess(AProperties).Multiline;
end;

function cxCheckBoxProperties_GetNullStyle(AProperties: TcxCustomEditProperties): TcxCheckBoxNullValueShowingStyle;
begin
  Result := TcxCustomCheckBoxPropertiesAccess(AProperties).NullStyle;
end;

{ CurrencyProperties Helpers }

function cxCurrencyProperties_GetNullString(AProperties: TcxCustomEditProperties): string;
begin
  Result := TcxCustomCurrencyEditPropertiesAccess(AProperties).Nullstring;
end;

{ EditProperties Helpers}

function cxEditProperties_GetAlignment(AProperties: TcxCustomEditProperties): TcxEditAlignment;
begin
  Result := TcxCustomEditPropertiesAccess(AProperties).Alignment;
end;

{ HyperLinkEditProperties Helpers }

function cxHyperLinkEditProperties_GetLinkColor(AProperties: TcxCustomEditProperties): TColor;
begin
  Result := TcxCustomHyperLinkEditPropertiesAccess(AProperties).LinkColor;
end;

{ MemoProperties Helpers }

function cxMemoProperties_GetAlignment(AProperties: TcxCustomEditProperties): TAlignment;
begin
  Result := TcxCustomMemoPropertiesAccess(AProperties).Alignment;
end;

function cxMemoProperties_GetWordWrap(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := TcxCustomMemoPropertiesAccess(AProperties).WordWrap;
end;

{ ImageComboBoxProperties Helpers }

function cxImageComboBoxProperties_GetDefaultDescription(AProperties: TcxCustomEditProperties): string;
begin
  Result := TcxCustomImageComboBoxPropertiesAccess(AProperties).DefaultDescription;
end;

function cxImageComboBoxProperties_GetDefaultImageIndex(AProperties: TcxCustomEditProperties): Integer;
begin
  Result := TcxCustomImageComboBoxPropertiesAccess(AProperties).DefaultImageIndex;
end;

function cxImageComboBoxProperties_FindItemByValue(AProperties: TcxCustomEditProperties;
  const AValue: TcxEditValue): TcxImageComboBoxItem;
begin
  Result := TcxCustomImageComboBoxPropertiesAccess(AProperties).FindItemByValue(AValue);
end;

function cxImageComboBoxProperties_GetImageAlignment(AProperties: TcxCustomEditProperties): TcxImageAlign;
begin
  Result := TcxCustomImageComboBoxPropertiesAccess(AProperties).ImageAlign;
end;

function cxImageComboBoxProperties_GetImages(AProperties: TcxCustomEditProperties): TCustomImageList;
begin
  Result := TcxCustomImageComboBoxPropertiesAccess(AProperties).Images;
end;

function cxImageComboBoxProperties_GetIsMultilined(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := TcxCustomImageComboBoxPropertiesAccess(AProperties).MultiLineText;
end;

function cxImageComboBoxProperties_GetItems(AProperties: TcxCustomEditProperties): TcxImageComboBoxItems;
begin
  Result := TcxCustomImageComboBoxPropertiesAccess(AProperties).Items;
end;

function cxImageComboBoxProperties_GetShowDescription(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := TcxCustomImageComboBoxPropertiesAccess(AProperties).ShowDescriptions;
end;

{ ImageProperties Helpers }

function cxImageProperties_GetCaption(AProperties: TcxCustomEditProperties): string;
begin
  Result := TcxCustomImagePropertiesAccess(AProperties).Caption;
end;

function cxImageProperties_GetCenter(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := TcxCustomImagePropertiesAccess(AProperties).Center;
end;

function cxImageProperties_GetProportional(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := TcxCustomImagePropertiesAccess(AProperties).Proportional;
end;

function cxImageProperties_GetTransparency(AProperties: TcxCustomEditProperties): TcxImageTransparency;
begin
  Result := TcxCustomImagePropertiesAccess(AProperties).GraphicTransparency;
end;

function cxImageProperties_GetStretch(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := TcxCustomImagePropertiesAccess(AProperties).Stretch;
end;

function cxImageProperties_GetGraphicClass(AProperties: TcxCustomEditProperties; ARecordIndex: Integer; AOwner: TObject = nil): TGraphicClass;
begin
  if AOwner = nil then
    AOwner := AProperties.Owner;
  Result := TcxCustomImagePropertiesAccess(AProperties).GetGraphicClass(AOwner, ARecordIndex);
end;

{ RadioGroupProperties Helpers }

function cxRadioGroupProperties_GetDisplayText(AProperties: TcxCustomEditProperties; const AValue: TcxEditValue): string;
begin
  Result := TcxCustomRadioGroupPropertiesAccess(AProperties).GetDisplayText(AValue, True);
end;

{ Office11 LookAndFeelPainter }

function cxOffice11LookAndFeelPainter_GetHeaderTopColor(APainter: TcxCustomLookAndFeelPainter): TColor;
begin
  if (APainter = nil) or (APainter.LookAndFeelStyle <> lfsOffice11) then
    APainter := cxLookAndFeelPaintersManager.GetPainter(lfsStandard);
  Result := APainter.DefaultHeaderColor;
end;

{ Utilities }

function Picture: TPicture;
begin
  if FPicture = nil then FPicture := TPicture.Create;
  Result := FPicture;
end;

function dxPSMakecxGridLines(AHorizontal, AVertical: Boolean): TcxGridLines;
const
  Results: array[Boolean] of TcxGridLines = (glVertical, glHorizontal);
begin
  Result := glNone;
  case Ord(AHorizontal) + Ord(AVertical) of
    1: Result := Results[AHorizontal];
    2: Result := glBoth;
  end;
end;

function GetUniqueName(AOwner: TComponent; const APrefix: string): string;
var
  I: Integer;
begin
  I := 0;
  repeat
    Inc(I);
    Result := APrefix + IntToStr(I);
  until (I = MaxInt) or (AOwner = nil) or (AOwner.FindComponent(Result) = nil);

  if I = MaxInt then Result := '';
end;

procedure InitializeStyleBarViewInfo(var AViewInfo: TdxStyleBarViewInfo);
begin
  FillChar(AViewInfo, SizeOf(AViewInfo), 0);
  AViewInfo.BarFont := TFont.Create;
  AViewInfo.StyleBitmap := TBitmap.Create;
  AViewInfo.StyleFont := TFont.Create;
  AViewInfo.StyleFontScaleFactor := TdxScaleFactor.Create;
end;

procedure DeinitializeStyleBarViewInfo(var AViewInfo: TdxStyleBarViewInfo);
begin
  AViewInfo.BarFont.Free;
  AViewInfo.StyleCaption := '';
  AViewInfo.StyleBitmap.Free;
  AViewInfo.StyleFontScaleFactor.Free;
  AViewInfo.StyleFont.Free;
  FillChar(AViewInfo, SizeOf(AViewInfo), 0);
end;

procedure dxPSDrawStyleBar(ACanvas: TCanvas; R: TRect; const AViewInfo: TdxStyleBarViewInfo);

  procedure DrawStyleBarCaption;
  var
    X, Y: Integer;
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := AViewInfo.BarColor;
    ACanvas.Font := AViewInfo.BarFont;

    X := AViewInfo.StyleCaptionBoxBounds.Left + AViewInfo.ScaleFactor.Apply(2);
    Y := AViewInfo.StyleCaptionBoxBounds.Top + (cxRectHeight(AViewInfo.StyleCaptionBoxBounds) - cxTextHeight(ACanvas.Handle)) div 2;

    ACanvas.TextRect(AViewInfo.StyleCaptionBoxBounds, X, Y, AViewInfo.StyleCaption);
  end;

  procedure DrawStyleBarColorBar;
  var
    APrevBrushOrg: TPoint;
    ARegion: TcxRegionHandle;
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := AViewInfo.StyleColor;

    if not AViewInfo.StyleBitmap.Empty then
    begin
      ACanvas.Brush.Bitmap := AViewInfo.StyleBitmap;
      SetBrushOrgEx(ACanvas.Handle, AViewInfo.StyleBitmapOrg.X, AViewInfo.StyleBitmapOrg.Y, @APrevBrushOrg);
    end;

    ACanvas.Pen.Color := AViewInfo.BarStyleColorBoxFrameColor;
    ACanvas.Rectangle(AViewInfo.StyleColorBoxContentBounds);

    if not AViewInfo.StyleBitmap.Empty then
    begin
      ACanvas.Brush.Bitmap := nil;
      SetBrushOrgEx(ACanvas.Handle, APrevBrushOrg.X, APrevBrushOrg.Y, nil);
    end;

    ARegion := dxPSUtl.ExcludeClipRect(ACanvas.Handle, AViewInfo.StyleColorBoxContentBounds);
    try
      ACanvas.Brush.Color := AViewInfo.BarColor;
      ACanvas.FillRect(AViewInfo.StyleColorBoxBounds);
    finally
      dxPSUtl.RestoreClipRgn(ACanvas.Handle, ARegion);
    end;
  end;

  procedure DrawStyleBarFontInfo;
  var
    X, Y: Integer;
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := AViewInfo.BarColor;
    ACanvas.Font := AViewInfo.StyleFont;
    ACanvas.Font.Height := AViewInfo.ScaleFactor.Apply(ACanvas.Font.Height, AViewInfo.StyleFontScaleFactor);
    ACanvas.Font.Color := AViewInfo.StyleTextColor;

    X := AViewInfo.StyleFontInfoBoxBounds.Left + AViewInfo.ScaleFactor.Apply(1);
    Y := AViewInfo.StyleFontInfoBoxBounds.Top + (cxRectHeight(AViewInfo.StyleFontInfoBoxBounds) - cxTextHeight(ACanvas.Handle)) div 2;

    ACanvas.TextRect(AViewInfo.StyleFontInfoBoxBounds, X, Y, dxPSUtl.FormatFontInfo(AViewInfo.StyleFont));
  end;

  procedure DrawRestSpace;
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := AViewInfo.BarColor;
    ACanvas.FillRect(AViewInfo.RestSpaceBounds);
  end;

begin
  DrawStyleBarCaption;
  DrawStyleBarColorBar;
  DrawStyleBarFontInfo;
  DrawRestSpace;
end;

function dxPSMeasureTextWidth(ACanvas: TdxPSReportRenderCustomCanvas; const AText: string; AFont: TFont): Integer;
begin
  ACanvas.SaveState;
  try
    ACanvas.Font := AFont;
    Result := 2 + ACanvas.TextSize(AText).cx + 2;
  finally
    ACanvas.RestoreState;
  end;
end;

function dxPSPreviewCarLogos(Index: Integer): TPicture;

  function ExtractImage(Source: TPicture; Index: Integer): TPicture;
  begin
    Result := TPicture.Create;
    with Result.Bitmap do
    begin
      Width := dxPSPreviewCarLogoWidth;
      Height := dxPSPreviewCarLogoHeight;
      Canvas.Draw(-Index * dxPSPreviewCarLogoWidth, 0, Source.Bitmap);
      Transparent := True;
    end;
  end;

var
  Picture: TPicture;
  I: Integer;
begin
  if FPreviewPictures[0] = nil then
  begin
    Picture := TPicture.Create;
    try
      dxLoadBitmapFromResource(Picture.Bitmap, IDB_DXPSCARLOGOS);
      for I := 0 to dxPSPreviewCarLogoCount - 1 do
        FPreviewPictures[I] := ExtractImage(Picture, I);
    finally
      Picture.Free;
    end;
  end;

  if (Index > -1) and (Index < dxPSPreviewCarLogoCount) then
    Result := FPreviewPictures[Index]
  else
    Result := nil;
end;

function dxPSPreviewCarLogosAsString(Index: Integer): AnsiString;
begin
  cxImage.SavePicture(dxPSPreviewCarLogos(Index), Result);
end;

procedure dxPSResetStyles(AStyles: TcxStyles);
begin
  cxStyles_SetStyleSheet(AStyles, nil);
  cxStyles_ResetStyles(AStyles);
end;

function dxPSPictureDialog: TOpenPictureDialog;
begin
  if FPictureDialog = nil then
  begin
    FPictureDialog := TOpenPictureDialog.Create(nil);
    FPictureDialog.Title := cxGetResourceString(@sdxLoadBitmapDlgTitle);
    //if dxPSEngn.InitialDir <> '' then
    //  FPictureDialog.InitialDir := dxPSEngn.InitialDir;
  end;
  Result := FPictureDialog;
end;

procedure dxPSGetViewParams(AStyle: TcxStyle; const AStyleName: string;
  AStyles: TdxCustomReportLinkStyles; out AParams: TcxViewParams; out AParamsScaleFactor: TdxScaleFactor);
begin
  AStyles.GetDefaultViewParamsByCaption(AStyleName, nil, AParams);
  AParamsScaleFactor := dxGetScaleFactor(AStyles.GetOwner);
  if AStyle <> nil then
  begin
    AParamsScaleFactor := dxGetScaleFactor(AStyle.StyleRepository);
    AParams.Bitmap := AStyle.Bitmap;
    AParams.Font := AStyle.Font;
    AParams.TextColor := clWindowText;
    if AStyle.Color <> clDefault then
      AParams.Color := AStyle.Color;
  end;
end;

{ TdxPSDataMaps }

function dxPSDataMaps: TdxPSDataMaps;
begin
  Result := TdxPSDataMaps.Instance;
end;

class function TdxPSDataMaps.Instance: TdxPSDataMaps;
begin
  Result := inherited Instance as TdxPSDataMaps;
end;

function TdxPSDataMaps.DoesItemParticipateInAutoHeightCalculation(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := MapClasses[AProperties].DoesItemParticipateInAutoHeightCalculation(AProperties);
end;

function TdxPSDataMaps.DoesItemParticipateInAutoWidthCalculation(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := MapClasses[AProperties].DoesItemParticipateInAutoWidthCalculation(AProperties);
end;

function TdxPSDataMaps.DoesItemParticipateInBestFitCalculation(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := MapClasses[AProperties].DoesItemParticipateInBestFitCalculation(AProperties);
end;

procedure TdxPSDataMaps.GetImageLists(AProperties: TcxCustomEditProperties; AProc: TdxPSGetImageListProc);
begin
  MapClasses[AProperties].GetImageLists(AProperties, AProc);
end;

procedure TdxPSDataMaps.InitializeItem(AnItem: TAbstractdxReportCellData;
  AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
  const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
  AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil);
begin
  MapClasses[AProperties].InitializeItem(AnItem, AProperties, AValue, ACellParams, AViewParams, AnIsPreview, ARecordIndex, AOwner);
end;

function TdxPSDataMaps.ItemClass(AProperties: TcxCustomEditProperties; AnIsPreview: Boolean = False): TdxReportCellDataClass;
begin
  Result := MapClasses[AProperties].ItemClass(AProperties, AnIsPreview);
end;

function TdxPSDataMaps.ItemClass(AProperties: TcxCustomEditProperties;
  const AViewParams: TdxReportItemViewParams; AIsPreview: Boolean): TdxReportCellDataClass;
begin
  Result := MapClasses[AProperties].ItemClass(AProperties, AViewParams, AIsPreview);
end;

function TdxPSDataMaps.MeasureWidth(AProperties: TcxCustomEditProperties;
  const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; AFont: TFont;
  ARecordIndex: Integer = 0): Integer;
begin
  Result := MapClasses[AProperties].MeasureWidth(AProperties, AValue, ACellParams, AFont, ARecordIndex);
end;

function TdxPSDataMaps.GetMapClass(Properties: TcxCustomEditProperties): TdxPSDataMapClass;
begin
  Result := TdxPSDataMapClass(PairClasses[Properties.ClassType]);
end;

{ TdxPSCustomDataMap }

class function TdxPSCustomDataMap.PairClass: TClass;
begin
  Result := PropertiesClass;
end;

class procedure TdxPSCustomDataMap.Register;
begin
  dxPSDataMaps.Register(Self);
end;

class procedure TdxPSCustomDataMap.Unregister;
begin
  dxPSDataMaps.Unregister(Self);
end;

class function TdxPSCustomDataMap.DoesItemParticipateInAutoHeightCalculation(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := True;
end;

class function TdxPSCustomDataMap.DoesItemParticipateInAutoWidthCalculation(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := True;
end;

class function TdxPSCustomDataMap.DoesItemParticipateInBestFitCalculation(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := True;
end;

class procedure TdxPSCustomDataMap.GetImageLists(AProperties: TcxCustomEditProperties;
  AProc: TdxPSGetImageListProc);
begin
end;

class function TdxPSCustomDataMap.GetText(AProperties: TcxCustomEditProperties;
  const AValue: TcxEditValue): string;
begin
  Result := AProperties.GetDisplayText(AValue, True);
end;

class function TdxPSCustomDataMap.HasText(AProperties: TcxCustomEditProperties;
  const AValue: TcxEditValue): Boolean;
begin
  Result := GetText(AProperties, AValue) <> '';
end;

class procedure TdxPSCustomDataMap.InitializeItem(AnItem: TAbstractdxReportCellData;
  AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
  const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
  AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil);
begin
  (AnItem as TdxReportCellString).EndEllipsis := ACellParams.EndEllipsis;
  (AnItem as TdxReportCellString).Text := GetText(AProperties, AValue);
end;

class function TdxPSCustomDataMap.ItemClass(
  AProperties: TcxCustomEditProperties; AIsPreview: Boolean = False): TdxReportCellDataClass;
begin
  Result := TdxReportCellString;
end;

class function TdxPSCustomDataMap.ItemClass(AProperties: TcxCustomEditProperties;
  const AViewParams: TdxReportItemViewParams; AIsPreview: Boolean = False): TdxReportCellDataClass;
begin
  Result := ItemClass(AProperties, AIsPreview);
end;

class function TdxPSCustomDataMap.MeasureWidth(AProperties: TcxCustomEditProperties;
  const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; AFont: TFont;
  ARecordIndex: Integer = 0): Integer;
begin
  Result := 0;
  if HasText(AProperties, AValue) then
    Result := dxPSMeasureTextWidth(ACellParams.Canvas, GetText(AProperties, AValue), AFont);
end;

class function TdxPSCustomDataMap.PropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomEditProperties;
end;

{ TdxPSTextDataMap }

class procedure TdxPSTextDataMap.InitializeItem(AnItem: TAbstractdxReportCellData;
  AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
  const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
  AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil);

  procedure GetPreviewParams(out ALeftIndent, ARightIndent, AMaxLineCount: Integer);
  var
    CellParams2: IdxPSCellParams2;
  begin
    if Supports(ACellParams, IdxPSCellParams2, CellParams2) then
    begin
      ALeftIndent := CellParams2.PreviewMarginLeft;
      ARightIndent := CellParams2.PreviewMarginRight;
      AMaxLineCount := CellParams2.PreviewMaxLineCount;
    end
    else
    begin
      ALeftIndent := 20;
      ARightIndent := 5;
      AMaxLineCount := -1;
    end;
  end;

var
  LeftIndent, RightIndent, MaxLineCount: Integer;
begin
  with TdxReportCellString(AnItem) do
  begin
    EndEllipsis := ACellParams.EndEllipsis;
    Multiline := AnIsPreview or ACellParams.Multiline;
    AdjustFont := Multiline;
    Text := GetText(AProperties, AValue);
    TextAlignX := EditTextAlignXMap[cxEditProperties_GetAlignment(AProperties).Horz];
    if AnIsPreview then
      TextAlignY := taTop
    else
      TextAlignY := EditTextAlignYMap[cxEditProperties_GetAlignment(AProperties).Vert];
  end;
  if AnIsPreview then
  begin
    GetPreviewParams(LeftIndent, RightIndent, MaxLineCount);
    if AnIsPreview then
    begin
      TdxReportCellPreviewText(AnItem).LeftIndent := LeftIndent;
      TdxReportCellPreviewText(AnItem).RightIndent := RightIndent;
      TdxReportCellPreviewText(AnItem).MaxLineCount := MaxLineCount;
    end;
  end;
end;

class function TdxPSTextDataMap.ItemClass(AProperties: TcxCustomEditProperties;
  AnIsPreview: Boolean = False): TdxReportCellDataClass;
const
  ItemClasses: array[Boolean] of TdxReportCellDataClass = (TdxReportCellString, TdxReportCellPreviewText);
begin
  Result := ItemClasses[AnIsPreview];
end;

class function TdxPSTextDataMap.PropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomTextEditProperties;
end;

{ TdxPSMemoDataMap }

class function TdxPSMemoDataMap.DoesItemParticipateInAutoWidthCalculation(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := not cxMemoProperties_GetWordWrap(AProperties);
end;

class function TdxPSMemoDataMap.DoesItemParticipateInBestFitCalculation(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := False;
end;

class procedure TdxPSMemoDataMap.InitializeItem(AnItem: TAbstractdxReportCellData;
  AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
  const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
  AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil);
begin
  inherited;
  with TdxReportCellString(AnItem) do
  begin
    Multiline := cxMemoProperties_GetWordWrap(AProperties);
    TextAlignX := TextAlignXMap[cxMemoProperties_GetAlignment(AProperties)];
  end;
end;

class function TdxPSMemoDataMap.PropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomMemoProperties;
end;

{ TdxPSHyperLinkDataMap }

class procedure TdxPSHyperLinkDataMap.InitializeItem(AnItem: TAbstractdxReportCellData;
  AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
  const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
  AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil);
begin
  inherited;
  AViewParams.FontStyle := AViewParams.FontStyle + [fsUnderline];
  AViewParams.NativeParams.TextColor := cxHyperLinkEditProperties_GetLinkColor(AProperties);
end;

class function TdxPSHyperLinkDataMap.PropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomHyperLinkEditProperties;
end;

{ TdxPSCheckDataMap }

class procedure TdxPSCheckDataMap.InitializeItem(AnItem: TAbstractdxReportCellData;
  AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
  const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
  AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil);
var
  ACheckDisplayValue: TcxEditValue;
  ACheckNullStyle: TcxCheckBoxNullValueShowingStyle;
  ACheckGlyph: TdxSmartGlyph;
  AImage: TdxReportCellCheckImage;
begin
  AImage := AnItem as TdxReportCellCheckImage;
  AProperties.PrepareDisplayValue(AValue, ACheckDisplayValue, False);
  AImage.ButtonEdgeStyle := EdgeStyleMap[ACellParams.FlatCheckMarks];
  ACheckNullStyle := cxCheckBoxProperties_GetNullStyle(AProperties);
  AImage.Checked := (ACheckDisplayValue = Integer(cbsChecked)) or ((ACheckDisplayValue = Integer(cbsGrayed)) and (ACheckNullStyle = nssGrayedChecked));
  AImage.Enabled := not ((ACheckDisplayValue = Integer(cbsGrayed)) and (ACheckNullStyle in [nssInactive, nssGrayedChecked]));
  ACheckGlyph := cxCheckBoxProperties_GetGlyph(AProperties);
  if not ACheckGlyph.Empty then
  begin
    AImage.Glyph.Assign(ACheckGlyph);
    AImage.GlyphCount := cxCheckBoxProperties_GetGlyphCount(AProperties);
  end;
  AImage.Multiline := cxCheckBoxProperties_GetIsMultilined(AProperties);
  AImage.TextAlignX := dxTextAlignX[cxCheckBoxProperties_GetAlignment(AProperties)];
  AImage.TextAlignY := taCenterY;
end;

class function TdxPSCheckDataMap.ItemClass(AProperties: TcxCustomEditProperties;
  AnIsPreview: Boolean = False): TdxReportCellDataClass;
begin
  Result := TdxReportCellCheckImage;
end;

class function TdxPSCheckDataMap.MeasureWidth(AProperties: TcxCustomEditProperties;
  const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; AFont: TFont;
  ARecordIndex: Integer = 0): Integer;
begin
  Result := 2 + dxPSGlbl.CheckWidth + 2;
end;

class function TdxPSCheckDataMap.PropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomCheckBoxProperties;
end;

{ TdxPSToggleSwitchDataMap }

class procedure TdxPSToggleSwitchDataMap.InitializeItem(AnItem: TAbstractdxReportCellData;
  AProperties: TcxCustomEditProperties; const AValue: TcxEditValue; const ACellParams: IdxPSCellParams;
  var AViewParams: TdxReportItemViewParams; AnIsPreview: Boolean; ARecordIndex: Integer; AOwner: TObject);

  procedure DrawToggleSwitch(ABitmap: TcxBitmap);
  var
    AViewInfo: TdxToggleSwitchViewInfoAccess;
    AViewData: TdxCustomToggleSwitchViewData;
    ACheckDisplayValue: TcxEditValue;
  begin
    AProperties.PrepareDisplayValue(AValue, ACheckDisplayValue, False);
    AViewInfo := TdxToggleSwitchViewInfoAccess(AProperties.GetViewInfoClass.Create);
    try
      AViewData := TdxCustomToggleSwitchViewData(AProperties.CreateViewData(DefaultEditStyleController.Style, True));
      try
        ABitmap.SetSize(AnItem.BoundsRect);
        AViewInfo.State := ACheckDisplayValue;
        AViewInfo.Transparent := True;
        AViewData.Calculate(ABitmap.cxCanvas, cxRectInflate(cxRectSetNullOrigin(AnItem.BoundsRect), -2, -2), Point(0, 0), cxmbNone, [], AViewInfo, False);
        TdxReportCellCheckImage(AnItem).GlyphCount := 1;
        AViewInfo.Paint(ABitmap.cxCanvas);
      finally
        AViewData.Free;
      end;
    finally
      AViewInfo.Free;
    end;
  end;

var
  ABitmap: TcxBitmap;
begin
  ABitmap := TcxBitmap.Create;
  try
    DrawToggleSwitch(ABitmap);
    TdxReportCellCheckImage(AnItem).Glyph.Assign(ABitmap);
  finally
    ABitmap.Free;
  end;
end;

class function TdxPSToggleSwitchDataMap.PropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxCustomToggleSwitchProperties;
end;

{ TdxSparklineDataMap }

class procedure TdxSparklineDataMap.InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
  const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
  AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil);
begin
  if TdxReportCellSparklineImage(AnItem).ViewData = nil then
  begin
    TdxReportCellSparklineImage(AnItem).ViewData := AProperties.CreateViewData(DefaultEditStyleController.Style, True);
    TdxReportCellSparklineImage(AnItem).ViewInfo := AProperties.GetViewInfoClass.Create as TdxSparkLineViewInfo;
    TdxReportCellSparklineImage(AnItem).Transparent := True;
    TdxReportCellSparklineImage(AnItem).ViewData.EditValueToDrawValue(AValue, TdxReportCellSparklineImage(AnItem).ViewInfo);
  end;
  inherited;
  TdxReportCellSparklineImage(AnItem).BoundsChanged;
end;

class function TdxSparklineDataMap.ItemClass(AProperties: TcxCustomEditProperties; AnIsPreview: Boolean): TdxReportCellDataClass;
begin
  Result := TdxReportCellSparklineImage;
end;

class function TdxSparklineDataMap.PropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxSparklineProperties;
end;

{ TdxTokenEditDataMap }

class procedure TdxTokenEditDataMap.InitializeItem(AnItem: TAbstractdxReportCellData;
  AProperties: TcxCustomEditProperties; const AValue: TcxEditValue; const ACellParams: IdxPSCellParams;
  var AViewParams: TdxReportItemViewParams; AnIsPreview: Boolean; ARecordIndex: Integer; AOwner: TObject);
var
  AReportCell: TdxReportCellTokens;
  AViewInfo: TdxTokenEditViewInfo;
  I: Integer;
begin
  if TdxTokenEditProperties(AProperties).CloseGlyphPosition <> teepNone then
  begin
    AProperties := AProperties.Clone(nil);
    try
      TdxTokenEditProperties(AProperties).CloseGlyphPosition := teepNone;
      InitializeItem(AnItem, AProperties, AValue, ACellParams, AViewParams, AnIsPreview, ARecordIndex, AOwner);
    finally
      AProperties.Free;
    end;
  end
  else
  begin
    AViewInfo := CalculateViewInfo(AProperties, AnItem.BoundsRect, AValue);
    try
      AReportCell := TdxReportCellTokens(AnItem);
      AReportCell.Transparent := True;
      for I := 0 to AViewInfo.Tokens.Count - 1 do
        AReportCell.AddToken.Assign(AViewInfo.Tokens[I]);
    finally
      AViewInfo.Free;
    end;
  end;
end;

class function TdxTokenEditDataMap.ItemClass(AProperties: TcxCustomEditProperties; AnIsPreview: Boolean): TdxReportCellDataClass;
begin
  Result := TdxReportCellTokens;
end;

class function TdxTokenEditDataMap.PropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxTokenEditProperties;
end;

class function TdxTokenEditDataMap.CalculateViewInfo(
  AProperties: TcxCustomEditProperties; ABounds: TRect; const AValue: TcxEditValue): TdxTokenEditViewInfo;
var
  AViewData: TdxTokenEditViewData;
begin
  AViewData := AProperties.CreateViewData(DefaultEditStyleController.Style, True) as TdxTokenEditViewData;
  try
    Result := AProperties.GetViewInfoClass.Create as TdxTokenEditViewInfo;
    Result.UpdateEditValue(AValue);
    try
      AViewData.Calculate(cxScreenCanvas, cxRectSetHeight(ABounds, cxMaxRectSize), cxInvalidPoint, cxmbNone, [], Result, False);
    finally
      cxScreenCanvas.Dormant;
    end;
  finally
    AViewData.Free;
  end;
end;

{ TdxPSCustomButtonGroupDataMap }

class function TdxPSCustomButtonGroupDataMap.ButtonGroupClass(AProperties: TcxCustomEditProperties): TdxCustomReportButtonGroupClass;
begin
  Result := TdxCustomReportButtonGroup;
end;

class procedure TdxPSCustomButtonGroupDataMap.InitializeGroupButton(AProperties: TcxCustomEditProperties;
  const AValue: TcxEditValue; AButton: TdxCustomReportCellCheck; AnIndex: Integer);
begin
end;

class procedure TdxPSCustomButtonGroupDataMap.InitializeItem(
  AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
  const AValue: TcxEditValue; const ACellParams: IdxPSCellParams;
  var AViewParams: TdxReportItemViewParams; AnIsPreview: Boolean = False;
  ARecordIndex: Integer = 0; AOwner: TObject = nil);
var
  Button: TdxCustomReportCellCheck;
  GroupItems: TcxButtonGroupItems;
  I: Integer;
begin
  with TdxCustomReportButtonGroup(AnItem) do
  begin
    ButtonEdgeStyle := EdgeStyleMap[ACellParams.FlatCheckMarks];
    CheckPos := ccpLeft;
    ColumnCount := cxButtonGroupProperties_GetColumnCount(AProperties);
    GroupItems := cxButtonGroupProperties_GetItems(AProperties);
    for I := 0 to GroupItems.Count - 1 do
    begin
      Button := Add(cxButtonGroupItem_GetCaption(GroupItems[I]));
      InitializeGroupButton(AProperties, AValue, Button, I);
    end;
    if AViewParams.NativeParams.Font <> nil then
      Font := AViewParams.NativeParams.Font;
    Indents := cxNullRect;
    InterColumnsMinSpace := 0;
    InterRowsMinSpace := 0;
    AdjustContent(ACellParams.Canvas);
    for I := 0 to ItemCount - 1 do
      Items[I].Visible := True;
  end;
end;

class function TdxPSCustomButtonGroupDataMap.ItemClass(AProperties: TcxCustomEditProperties;
  AnIsPreview: Boolean = False): TdxReportCellDataClass;
begin
  Result := TdxReportCellDataClass(ButtonGroupClass(AProperties));
end;

class function TdxPSCustomButtonGroupDataMap.MeasureWidth(
  AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
  const ACellParams: IdxPSCellParams; AFont: TFont; ARecordIndex: Integer = 0): Integer;
var
  GroupColumnCount: Integer;
  GroupItems: TcxButtonGroupItems;
  I, J, Index, RowItemCount: Integer;
  MaxColumnWidths: array of Integer;
begin
  Result := 0;
  GroupItems := cxButtonGroupProperties_GetItems(AProperties);
  if GroupItems.Count <> 0 then
  begin
    GroupColumnCount := cxButtonGroupProperties_GetColumnCount(AProperties);

    SetLength(MaxColumnWidths, GroupColumnCount);
    RowItemCount := GroupColumnCount div GroupItems.Count;
    if (GroupColumnCount mod GroupItems.Count) <> 0 then
      Inc(RowItemCount);

    for I := 0 to GroupColumnCount - 1 do
      for J := 0 to RowItemCount - 1 do
      begin
        Index := I * RowItemCount + J;
        if Index < GroupItems.Count then
        begin
          MaxColumnWidths[I] := Max(MaxColumnWidths[I],
            dxPSMeasureTextWidth(ACellParams.Canvas,
            cxButtonGroupItem_GetCaption(GroupItems[Index]), AFont));
        end;
      end;

    for I := 0 to Length(MaxColumnWidths) - 1 do
      Inc(Result, MaxColumnWidths[I]);
    if Result <> 0 then
    begin
      Inc(Result, GroupColumnCount * dxPSGlbl.CheckWidth);
      Inc(Result, (GroupColumnCount - 1) * dxPSCore.dxRadioGroupInterColumnsMinSpace);
      Inc(Result, dxPSCore.dxRadioGroupBoundsIndent + dxPSCore.dxRadioGroupBoundsIndent);
    end;
  end;
end;

class function TdxPSCustomButtonGroupDataMap.PropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomButtonGroupProperties;
end;

{ TdxPSRadioButtonGroupDataMap }

class function TdxPSRadioButtonGroupDataMap.ButtonGroupClass(AProperties: TcxCustomEditProperties): TdxCustomReportButtonGroupClass;
begin
  Result := TdxReportRadioGroup;
end;

class procedure TdxPSRadioButtonGroupDataMap.InitializeItem(AnItem: TAbstractdxReportCellData;
  AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
  const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
  AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil);
begin
  inherited;
  with TdxReportRadioGroup(AnItem) do
    ItemIndex := FindItem(cxRadioGroupProperties_GetDisplayText(AProperties, AValue));
end;

class function TdxPSRadioButtonGroupDataMap.PropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomRadioGroupProperties;
end;

{ TdxPSGraphicDataMap }

class function TdxPSGraphicDataMap.DoesItemParticipateInAutoHeightCalculation(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := True;//lcm: not cxImageProperties_GetStretch(AProperties);
end;

class function TdxPSGraphicDataMap.DefaultGraphicClass: TGraphicClass;
begin
  Result := TBitmap;
end;

class function TdxPSGraphicDataMap.GetGraphic(AProperties: TcxCustomEditProperties;
  AGraphicClass: TGraphicClass; const AValue: TcxEditValue): TGraphic;
begin
  if HasGraphic(AValue) then
    cxImage.LoadPicture(Picture, AGraphicClass, AValue)
  else
    Picture.Assign(nil);
  Result := Picture.Graphic;
end;

class function TdxPSGraphicDataMap.GetGraphicClass(AProperties: TcxCustomEditProperties;
  ARecordIndex: Integer; AOwner: TObject = nil): TGraphicClass;
begin
  Result := cxImageProperties_GetGraphicClass(AProperties, ARecordIndex, AOwner);
  if Result = nil then
    Result := DefaultGraphicClass;
end;

class function TdxPSGraphicDataMap.HasGraphic(const AValue: TcxEditValue): Boolean;
begin
  Result := dxVarIsBlob(AValue);
end;

class procedure TdxPSGraphicDataMap.InitializeItem(AnItem: TAbstractdxReportCellData;
  AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
  const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
  AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil);
var
  AGraphicClass: TGraphicClass;
  AIsIcon: Boolean;
  AItem: TdxReportCellGraphic;
begin
  if HasGraphic(AValue) then
    AGraphicClass := GetGraphicClass(AProperties, ARecordIndex, AOwner)
  else
    AGraphicClass := nil;

  AIsIcon := (AGraphicClass <> nil) and AGraphicClass.InheritsFrom(TIcon);

  AItem := TdxReportCellGraphic(AnItem);
  AItem.Center := cxImageProperties_GetCenter(AProperties);
  AItem.Image := GetGraphic(AProperties, AGraphicClass, AValue);
  AItem.ImageTransparent := AIsIcon or ACellParams.TransparentGraphics or (cxImageProperties_GetTransparency(AProperties) = gtTransparent);
  AItem.Stretch := (AGraphicClass <> nil) and not AIsIcon and cxImageProperties_GetStretch(AProperties);
  AItem.Proportional := (AGraphicClass <> nil) and not AIsIcon and cxImageProperties_GetProportional(AProperties);
end;

class function TdxPSGraphicDataMap.MeasureWidth(AProperties: TcxCustomEditProperties;
  const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; AFont: TFont;
  ARecordIndex: Integer = 0): Integer;
var
  GraphicClass: TGraphicClass;
begin
  if HasGraphic(AValue) then
  begin
    GraphicClass := GetGraphicClass(AProperties, ARecordIndex);
    Result := GetGraphic(AProperties, GraphicClass, AValue).Width;
  end
  else
    Result := 0;
end;

class function TdxPSGraphicDataMap.ItemClass(AProperties: TcxCustomEditProperties;
  AnIsPreview: Boolean = False): TdxReportCellDataClass;
begin
  Result := TdxReportCellGraphic;
end;

class function TdxPSGraphicDataMap.PropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomImageProperties;
end;

{ TdxPSGraphicAsTextDataMap }

class procedure TdxPSGraphicAsTextDataMap.InitializeItem(AnItem: TAbstractdxReportCellData;
  AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
  const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
  AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil);
begin
  TdxReportCellString(AnItem).Text := ACellParams.GraphicsText;
  TdxReportCellString(AnItem).TextAlignX := taLeft;
  TdxReportCellString(AnItem).TextAlignY := taTop;
end;

class function TdxPSGraphicAsTextDataMap.ItemClass(AProperties: TcxCustomEditProperties;
  AnIsPreview: Boolean = False): TdxReportCellDataClass;
begin
  Result := TdxReportCellString;
end;

class function TdxPSGraphicAsTextDataMap.MeasureWidth(AProperties: TcxCustomEditProperties;
  const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; AFont: TFont;
  ARecordIndex: Integer = 0): Integer;
begin
  Result := dxPSMeasureTextWidth(ACellParams.Canvas, ACellParams.GraphicsText, AFont);
end;

class function TdxPSGraphicAsTextDataMap.PropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomImageProperties;
end;

{ TdxPSImageDataMap }

class function TdxPSImageDataMap.DoesItemParticipateInBestFitCalculation(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := not IsMultilinedText(AProperties);
end;

class function TdxPSImageDataMap.GetComboBoxItem(AProperties: TcxCustomEditProperties;
  const AValue: TcxEditValue): TcxImageComboBoxItem;
begin
  Result := cxImageComboBoxProperties_FindItemByValue(AProperties, AValue);
end;

class function TdxPSImageDataMap.GetImageIndex(AProperties: TcxCustomEditProperties;
  const AValue: TcxEditValue): Integer;
var
  CompoBoxItem: TcxImageComboBoxItem;
begin
  CompoBoxItem := GetComboBoxItem(AProperties, AValue);
  if CompoBoxItem <> nil then
    Result := CompoBoxItem.ImageIndex
  else
    Result := cxImageComboBoxProperties_GetDefaultImageIndex(AProperties);
end;

class procedure TdxPSImageDataMap.GetImageLists(AProperties: TcxCustomEditProperties;
  AProc: TdxPSGetImageListProc);
begin
  AProc(GetImages(AProperties));
end;

class function TdxPSImageDataMap.GetImages(AProperties: TcxCustomEditProperties): TCustomImageList;
begin
  Result := cxImageComboBoxProperties_GetImages(AProperties);
end;

class function TdxPSImageDataMap.GetText(AProperties: TcxCustomEditProperties;
  const AValue: TcxEditValue): string;
var
  CompoBoxItem: TcxImageComboBoxItem;
begin
  Result := '';
  if cxImageComboBoxProperties_GetShowDescription(AProperties) then
  begin
    CompoBoxItem := GetComboBoxItem(AProperties, AValue);
    if CompoBoxItem <> nil then
      Result := CompoBoxItem.Description
    else
      Result := cxImageComboBoxProperties_GetDefaultDescription(AProperties);
  end;
end;

class function TdxPSImageDataMap.HasImages(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := GetImages(AProperties) <> nil;
end;

class procedure TdxPSImageDataMap.InitializeItem(AnItem: TAbstractdxReportCellData;
  AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
  const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
  AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil);
begin
  inherited;
  with TdxReportCellImage(AnItem) do
  begin
    ImageList := GetImages(AProperties);
    ImageIndex := GetImageIndex(AProperties, AValue);
    ImageLayout := ImageLayoutMap[cxImageComboBoxProperties_GetImageAlignment(AProperties)];
    MultiLine := IsMultilinedText(AProperties);
    if not cxImageComboBoxProperties_GetShowDescription(AProperties) then
    begin
      ImageLayout := ilImageCenterCenter;
      Text := '';
    end;
  end;
end;

class function TdxPSImageDataMap.IsMultilinedText(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := cxImageComboBoxProperties_GetIsMultilined(AProperties);
end;

class function TdxPSImageDataMap.ItemClass(AProperties: TcxCustomEditProperties;
  AnIsPreview: Boolean = False): TdxReportCellDataClass;
begin
  Result := TdxReportCellImage;
end;

class function TdxPSImageDataMap.MeasureWidth(AProperties: TcxCustomEditProperties;
  const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; AFont: TFont;
  ARecordIndex: Integer = 0): Integer;
begin
  Result := inherited MeasureWidth(AProperties, AValue, ACellParams, AFont, ARecordIndex);
  if HasImages(AProperties) then
    Inc(Result, 1 + GetImages(AProperties).Width + 1);
end;

class function TdxPSImageDataMap.PropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomImageComboBoxProperties;
end;

{ TdxPSDelegateDataMap }

class function TdxPSDelegateDataMap.DataMapClass(AProperties: TcxCustomEditProperties): TdxPSDataMapClass;
begin
  Result := nil; {actually should be an abstract method, but C++ syntax does not allow us "static virtual abstract" methods :-( }
end;

class function TdxPSDelegateDataMap.DoesItemParticipateInAutoHeightCalculation(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := DataMapClass(AProperties).DoesItemParticipateInAutoHeightCalculation(AProperties);
end;

class function TdxPSDelegateDataMap.DoesItemParticipateInAutoWidthCalculation(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := DataMapClass(AProperties).DoesItemParticipateInAutoWidthCalculation(AProperties);
end;

class function TdxPSDelegateDataMap.DoesItemParticipateInBestFitCalculation(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := DataMapClass(AProperties).DoesItemParticipateInBestFitCalculation(AProperties);
end;

class procedure TdxPSDelegateDataMap.GetImageLists(AProperties: TcxCustomEditProperties;
  AProc: TdxPSGetImageListProc);
begin
  DataMapClass(AProperties).GetImageLists(AProperties, AProc);
end;

class procedure TdxPSDelegateDataMap.InitializeItem(AnItem: TAbstractdxReportCellData;
  AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
  const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
  AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil);
begin
  DataMapClass(AProperties).InitializeItem(AnItem, AProperties, AValue, ACellParams, AViewParams, AnIsPreview, ARecordIndex, AOwner);
end;

class function TdxPSDelegateDataMap.ItemClass(AProperties: TcxCustomEditProperties;
  AnIsPreview: Boolean = False): TdxReportCellDataClass;
begin
  Result := DataMapClass(AProperties).ItemClass(nil, AnIsPreview);
end;

class function TdxPSDelegateDataMap.MeasureWidth(AProperties: TcxCustomEditProperties;
  const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; AFont: TFont;
  ARecordIndex: Integer = 0): Integer;
begin
  Result := DataMapClass(AProperties).MeasureWidth(AProperties, AValue, ACellParams, AFont, ARecordIndex);
end;

{ TdxPSBlobDataMap }

class function TdxPSBlobDataMap.DataMapClass(AProperties: TcxCustomEditProperties): TdxPSDataMapClass;
const
  DataMapClasses: array[TcxBlobPaintStyle] of TdxPSDataMapClass =
    (TdxPSBlobDefaultDataMap, TdxPSBlobPictureDataMap, TdxPSBlobTextDataMap);
begin
  Result := DataMapClasses[cxBlobEditProperties_GetPaintStyle(AProperties)];
end;

class function TdxPSBlobDataMap.PropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomBlobEditProperties;
end;

{ TdxPSBlobDefaultDataMap }

class procedure TdxPSBlobDefaultDataMap.InitializeItem(AnItem: TAbstractdxReportCellData;
  AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
  const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
  AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil);
begin
  inherited;
  //TdxReportCellString(AnItem).Text := AValue;
  TdxReportCellString(AnItem).Text := cxBlobEdit.GetBlobText(AValue, TcxCustomBlobEditProperties(AProperties), False);
end;

{ TdxPSBlobPictureDataMap }

class procedure TdxPSBlobPictureDataMap.GetImageLists(AProperties: TcxCustomEditProperties;
  AProc: TdxPSGetImageListProc);
begin
  AProc(GetBlobImages);
end;

class procedure TdxPSBlobPictureDataMap.InitializeItem(AnItem: TAbstractdxReportCellData;
  AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
  const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
  AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil);
const
  ImageIndexes: array[TcxBlobEditKind] of Integer = (0, 2, 4, 6, 0);
begin
  inherited;
  with TdxReportCellGraphic(AnItem) do
  begin
    ImageList := GetBlobImages;
    ImageIndex := ImageIndexes[cxBlobEditProperties_GetEditKind(AProperties)] + Ord(not cxVariants.VarIsSoftNull(AValue));
    Center := True;
  end;
end;

class function TdxPSBlobPictureDataMap.ItemClass(AProperties: TcxCustomEditProperties;
  AnIsPreview: Boolean = False): TdxReportCellDataClass;
begin
  Result := TdxReportCellGraphic;
end;

class function TdxPSBlobPictureDataMap.MeasureWidth(AProperties: TcxCustomEditProperties;
  const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; AFont: TFont;
  ARecordIndex: Integer = 0): Integer;
begin
  Result := GetBlobImages.Width;
end;

{ TdxPSBlobTextDataMap }

class function TdxPSBlobTextDataMap.DoesItemParticipateInAutoWidthCalculation(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := False;
end;

class function TdxPSBlobTextDataMap.DoesItemParticipateInBestFitCalculation(AProperties: TcxCustomEditProperties): Boolean;
begin
  Result := False;
end;

class procedure TdxPSBlobTextDataMap.InitializeItem(AnItem: TAbstractdxReportCellData;
  AProperties: TcxCustomEditProperties; const AValue: TcxEditValue;
  const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
  AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil);
begin
  inherited;
  TdxReportCellString(AnItem).Multiline := True;//False; {3.1}
  //TdxReportCellString(AnItem).EndEllipsis := True; {3.1}
end;

{ TdxReportCellPreviewText }

procedure TdxReportCellPreviewText.Assign(Source: TPersistent);
begin
  if Source is TdxReportCellPreviewText then
    with TdxReportCellPreviewText(Source) do
    begin
      Self.MaxLineCount := MaxLineCount;
      Self.RightIndent := RightIndent;
    end;
  inherited;
end;

procedure TdxReportCellPreviewText.DrawText(ACanvas: TdxPSReportRenderCustomCanvas);
var
  R: TRect;
begin
  R := GetTextBounds(ACanvas);
  if not IsRectEmpty(R) then
    Renderer.DrawTextEx(ACanvas, R, MaxLineCount, GetText, Font, DTFormat, Indents.Rect);
end;

procedure TdxReportCellPreviewText.ReadData(AReader: TdxPSDataReader);
begin
  inherited ReadData(AReader);
  MaxLineCount := AReader.ReadInteger;
  RightIndent := AReader.ReadInteger;
end;

procedure TdxReportCellPreviewText.WriteData(AWriter: TdxPSDataWriter);
begin
  inherited WriteData(AWriter);
  AWriter.WriteInteger(MaxLineCount);
  AWriter.WriteInteger(RightIndent);
end;

function TdxReportCellPreviewText.GetLeftIndent: Integer;
begin
  Result := Indents.Left;
end;

procedure TdxReportCellPreviewText.SetLeftIndent(Value: Integer);
begin
  Indents.Left := Value;
end;

procedure TdxReportCellPreviewText.SetMaxLineCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  FMaxLineCount := Value;
end;

procedure TdxReportCellPreviewText.SetRightIndent(Value: Integer);
begin
  if Value < 0 then Value := 0;
  FRightIndent := Value;
end;

{ TdxReportCellSparklineImage }

procedure TdxReportCellSparklineImage.BoundsChanged;
var
  R: TRect;
  AImage: TcxBitmap32;
begin
  inherited BoundsChanged;
  if ViewData <> nil then
  begin
     R := cxRectInflate(cxRectSetNullOrigin(BoundsRect), -2);
     AImage := TcxBitmap32.CreateSize(R);
     try
       AImage.Canvas.Brush.Color := clWindow;
       AImage.Canvas.FillRect(AImage.ClientRect);
       ViewData.Calculate(AImage.cxCanvas, R, Point(0, 0), cxmbNone, [], ViewInfo, False);
       ViewInfo.Transparent := True;
       ViewInfo.Paint(AImage.cxCanvas);
       Image := AImage;
     finally
       AImage.Free;
     end;
  end;
end;

destructor TdxReportCellSparklineImage.Destroy;
begin
  FreeAndNil(ViewData);
  FreeAndNil(ViewInfo);
  inherited Destroy;
end;

{ TdxReportCellToken }

constructor TdxReportCellToken.Create(AOwner: TAbstractdxReportCellData);
begin
  FOwner := AOwner;
end;

destructor TdxReportCellToken.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

procedure TdxReportCellToken.Assign(ASource: TObject);
begin
  if ASource is TdxReportCellToken then
    AssignFromTokenCell(TdxReportCellToken(ASource))
  else
    if ASource is TdxTokenEditTokenViewInfo then
      AssignFromTokenItem(TdxTokenEditTokenViewInfo(ASource));
end;

procedure TdxReportCellToken.ConvertCoords(ANumerator, ADenominator: Integer);
begin
  FBounds := cxRectScale(FBounds, ANumerator, ADenominator);
  FTextBounds := cxRectScale(FTextBounds, ANumerator, ADenominator);
  FGlyphBounds := cxRectScale(FGlyphBounds, ANumerator, ADenominator);
end;

procedure TdxReportCellToken.Draw(ACanvas: TdxPSReportRenderCustomCanvas);
var
  AHelper: TdxPSAdjustFontSizeHelper;
begin
  if not cxRectIsEmpty(FBounds) then
  begin
//    FOwner.BorderPainter.DrawBorders(ACanvas, FBounds);
    if (FGlyph <> nil) and not cxRectIsEmpty(FGlyphBounds) then
      ACanvas.DrawPicture(FGlyph, FGlyphBounds, ppmStretch, PixelsNumerator, PixelsDenominator);
    if (FText <> '') and not cxRectIsEmpty(FTextBounds) then
    begin
      AHelper := TdxPSAdjustFontSizeHelper.Create(ACanvas);
      try
        AHelper.Font.Assign(FOwner.Font);
        AHelper.Calculate(cxRectWidth(FTextBounds), FText);
        ACanvas.DrawText(FTextBounds, FText, AHelper.Font, FOwner.DefaultDTFormat);
      finally
        AHelper.Free;
      end;
    end;
  end;
end;

procedure TdxReportCellToken.AssignFromTokenCell(ASource: TdxReportCellToken);
begin
  FText := ASource.FText;
  FTextBounds := ASource.FTextBounds;
  FBounds := ASource.FBounds;

  FGlyphBounds := ASource.FGlyphBounds;
  if ASource.FGlyph <> nil then
    SetGlyph(ASource.FGlyph.Clone)
  else
    SetGlyph(nil);
end;

procedure TdxReportCellToken.AssignFromTokenItem(ASource: TdxTokenEditTokenViewInfo);
var
  AImage: TdxSmartImage;
  AImageCanvas: TdxGPCanvas;
  AImageCanvasDC: HDC;
begin
  FText := ASource.Caption;
  FTextBounds := ASource.CaptionBounds;
  FBounds := ASource.Bounds;

  FGlyphBounds := ASource.GlyphBounds;
  if TdxTokenEditTokenViewInfoAccess(ASource).IsGlyphVisible then
  begin
    AImage := TdxSmartImage.CreateSize(FGlyphBounds);
    AImageCanvas := AImage.CreateCanvas;
    try
      AImageCanvasDC := AImageCanvas.GetHDC;
      try
        cxPaintCanvas.BeginPaint(AImageCanvasDC);
        try
          cxPaintCanvas.FillRect(AImage.ClientRect, FOwner.ContentBkColor);
          TdxTokenEditTokenViewInfoAccess(ASource).DrawGlyph(cxPaintCanvas, AImage.ClientRect);
        finally
          cxPaintCanvas.EndPaint;
        end;
      finally
        AImageCanvas.ReleaseHDC(AImageCanvasDC);
      end;
    finally
      AImageCanvas.Free;
    end;
    SetGlyph(AImage);
  end;
end;

procedure TdxReportCellToken.SetGlyph(AImage: TdxGPImage);
begin
  if FGlyph <> AImage then
  begin
    FreeAndNil(FGlyph);
    FGlyph := AImage;
  end;
end;

procedure TdxReportCellToken.ReadData(AReader: TdxPSDataReader);
var
  AImage: TdxSmartImage;
begin
  FBounds := AReader.ReadRect;
  FText := AReader.ReadString;
  FTextBounds := AReader.ReadRect;
  FGlyphBounds := AReader.ReadRect;
  if AReader.ReadBoolean then
  begin
    AImage := TdxSmartImage.Create;
    AReader.ReadImage(AImage);
    SetGlyph(AImage);
  end
  else
    SetGlyph(nil);
end;

procedure TdxReportCellToken.WriteData(AWriter: TdxPSDataWriter);
begin
  AWriter.WriteRect(FBounds);
  AWriter.WriteString(FText);
  AWriter.WriteRect(FTextBounds);
  AWriter.WriteRect(FGlyphBounds);
  AWriter.WriteBoolean(FGlyph <> nil);
  if FGlyph <> nil then
    AWriter.WriteImage(FGlyph);
end;

{ TdxReportCellTokens }

constructor TdxReportCellTokens.Create(AParent: TdxReportCell);
begin
  inherited Create(AParent);
  FTokens := TcxObjectList.Create;
end;

destructor TdxReportCellTokens.Destroy;
begin
  FreeAndNil(FTokens);
  inherited Destroy;
end;

function TdxReportCellTokens.AddToken: TdxReportCellToken;
begin
  Result := TdxReportCellToken.Create(Self);
  FTokens.Add(Result);
end;

procedure TdxReportCellTokens.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited Assign(Source);
  if Source is TdxReportCellTokens then
  begin
    FTokens.Clear;
    for I := 0 to TdxReportCellTokens(Source).TokenCount - 1 do
      AddToken.Assign(TdxReportCellTokens(Source).Tokens[I]);
  end;
end;

procedure TdxReportCellTokens.ConvertCoords(APixelsNumerator, APixelsDenominator: Integer);
var
  I: Integer;
begin
  inherited ConvertCoords(APixelsNumerator, APixelsDenominator);
  for I := 0 to TokenCount - 1 do
    Tokens[I].ConvertCoords(APixelsNumerator, APixelsDenominator);
end;

procedure TdxReportCellTokens.DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
var
  ADelta: Integer;
  I: Integer;
begin
  inherited DrawContent(ACanvas, AStage);

  if (rsSecondPass in AStage) and (TokenCount > 0) then
  begin
    ACanvas.SaveState;
    try
      ACanvas.IntersectClipRgn(BoundsRect);
      ADelta := MeasureContentHeight(ACanvas) - cxRectHeight(GetInnerBounds(ACanvas));
      if ADelta < 0 then
        ACanvas.WindowOrg := cxPointOffset(ACanvas.WindowOrg, 0, ADelta div 2);
      for I := 0 to TokenCount - 1 do
        Tokens[I].Draw(ACanvas);
    finally
      ACanvas.RestoreState;
    end;
  end;
end;

function TdxReportCellTokens.MeasureContentHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  if TokenCount > 0 then
    Result := Tokens[TokenCount - 1].FBounds.Bottom
  else
    Result := 0;
end;

function TdxReportCellTokens.MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to TokenCount - 1 do
    Result := Max(Result, Tokens[I].FBounds.Right);
end;

procedure TdxReportCellTokens.ReadData(AReader: TdxPSDataReader);
var
  ACount: Integer;
begin
  inherited ReadData(AReader);
  ACount := AReader.ReadInteger;
  while ACount > 0 do
  begin
    AddToken.ReadData(AReader);
    Dec(ACount);
  end;
end;

procedure TdxReportCellTokens.WriteData(AWriter: TdxPSDataWriter);
var
  I: Integer;
begin
  inherited WriteData(AWriter);
  AWriter.WriteInteger(TokenCount);
  for I := 0 to TokenCount - 1 do
    Tokens[I].WriteData(AWriter);
end;

function TdxReportCellTokens.GetToken(Index: Integer): TdxReportCellToken;
begin
  Result := TdxReportCellToken(FTokens.Items[Index]);
end;

function TdxReportCellTokens.GetTokenCount: Integer;
begin
  Result := FTokens.Count;
end;

{ TdxCustomReportLinkStyles }

procedure TdxCustomReportLinkStyles.DesignerFinalize;
begin
  if ReportLink <> nil then
    ReportLink.DesignerTabIndex := 0;
end;

procedure TdxCustomReportLinkStyles.DesignerInitialize;
begin
  if ReportLink <> nil then
    ReportLink.DesignerTabIndex := DesignerTabIndex;
end;

function TdxCustomReportLinkStyles.DesignerTabIndex: Integer;
begin
  Result := 0;
end;

procedure TdxCustomReportLinkStyles.Changed(AIndex: Integer);
begin
  inherited;
  if ReportLink <> nil then
    ReportLink.StyleChanged(GetStyleCaption(AIndex), Values[AIndex]);
end;

procedure TdxCustomReportLinkStyles.GetDefaultViewParamsByCaption(
  const ACaption: string; AData: Pointer; out AParams: TcxViewParams);
var
  Index: Integer;
begin
  FillChar(AParams, SizeOf(AParams), 0);
  Index := StyleIndexesByCaption[ACaption];
  if Index = -1 then
  begin
    AParams.Bitmap := nil;
    AParams.Color := clWindow;
    AParams.Font := ReportLink.Font;
    AParams.TextColor := AParams.Font.Color;
  end
  else
    GetDefaultViewParams(Index, AData, AParams);
end;

class function TdxCustomReportLinkStyles.GetStyleCaption(AnIndex: Integer): string;
begin
  Result := '';
end;

function TdxCustomReportLinkStyles.GetStyleByCaption(const Caption: string): TcxStyle;
var
  Index: Integer;
begin
  Index := StyleIndexesByCaption[Caption];
  if Index <> -1 then
    Result := Values[Index]
  else
    Result := nil;
end;

{function TdxCustomReportLinkStyles.GetStyleIndexByCaption(const Caption: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if dxPSUtl.dxSameText(Caption, GetStyleCaption(Items[Result].Index)) then
      Exit;
  Result := -1;
end;}

procedure TdxCustomReportLinkStyles.SetStyleByCaption(const Caption: string; Value: TcxStyle);
var
  Index: Integer;
begin
  Index := StyleIndexesByCaption[Caption];
  if Index <> -1 then
    Values[Index] := Value;
end;

function TdxCustomReportLinkStyles.GetReportLink: TdxCustomcxControlReportLink;
begin
  if GetOwner is TdxCustomcxControlReportLink then
    Result := TdxCustomcxControlReportLink(GetOwner)
  else
    Result := nil;
end;

{ TdxCustomReportLinkStyleSheet }

class procedure TdxCustomReportLinkStyleSheet.Register;
begin
  Classes.RegisterClass(Self);
end;

class procedure TdxCustomReportLinkStyleSheet.Unregister;
begin
  Classes.UnregisterClass(Self);
end;

{ TdxCustomReportLinkOptions }

constructor TdxCustomReportLinkOptions.Create(AReportLink: TdxCustomcxControlReportLink);
begin
  inherited Create;
  FReportLink := AReportLink;
  if ReportLink <> nil then
    ReportLink.AddOptions(Self);
  CreateObjects;
  RestoreDefaults;
  if ReportLink <> nil then
    ReportLink.LinkModified(False);
end;

destructor TdxCustomReportLinkOptions.Destroy;
begin
  DestroyObjects;
  if ReportLink <> nil then
    ReportLink.RemoveOptions(Self);
  inherited;
end;

procedure TdxCustomReportLinkOptions.Assign(Source: TPersistent);
begin
  if Source is TdxCustomReportLinkOptions then
  else
    inherited;
end;

procedure TdxCustomReportLinkOptions.RestoreDefaults;
begin
end;

{ IUnknown }
function TdxCustomReportLinkOptions.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TdxCustomReportLinkOptions._AddRef: Integer;
begin
  Result := -1;
end;

function TdxCustomReportLinkOptions._Release: Integer;
begin
  Result := -1;
end;

{ IdxReportLinkOptionsDesignerSupport }
procedure TdxCustomReportLinkOptions.DesignerFinalize;
begin
  if ReportLink <> nil then ReportLink.DesignerTabIndex := 0;
end;

procedure TdxCustomReportLinkOptions.DesignerInitialize;
begin
  if ReportLink <> nil then ReportLink.DesignerTabIndex := DesignerTabIndex;
end;

function TdxCustomReportLinkOptions.DesignerTabIndex: Integer;
begin
  Result := 0;
end;

procedure TdxCustomReportLinkOptions.Changed;
begin
  if ReportLink <> nil then ReportLink.OptionsChanged(Self);
end;

procedure TdxCustomReportLinkOptions.Notification(AComponent: TComponent; AOperation: TOperation);
begin
end;

procedure TdxCustomReportLinkOptions.CreateObjects;
begin
end;

procedure TdxCustomReportLinkOptions.DestroyObjects;
begin
end;

procedure TdxCustomReportLinkOptions.DesignerModified;
begin
  if ReportLink <> nil then ReportLink.DesignerModified;
end;

{ TdxGridReportLinkOptionsFormatting }

procedure TdxCustomReportLinkOptionsFormatting.Assign(Source: TPersistent);
begin
  if Source is TdxCustomReportLinkOptionsFormatting then
    with TdxCustomReportLinkOptionsFormatting(Source) do
    begin
      Self.GridLineColor := GridLineColor;
      Self.LookAndFeelKind := LookAndFeelKind;
      Self.SuppressBackgroundBitmaps := SuppressBackgroundBitmaps;
      Self.UseLookAndFeelColors := UseLookAndFeelColors;
      Self.UseNativeStyles := UseNativeStyles;
    end;
  inherited;
end;

procedure TdxCustomReportLinkOptionsFormatting.RestoreDefaults;
begin
  inherited;
  GridLineColor := clDefault;
  LookAndFeelKind := lfUltraFlat;
  SuppressBackgroundBitmaps := False;
  UseLookAndFeelColors := False;
  UseNativeStyles := False;
end;

procedure TdxCustomReportLinkOptionsFormatting.SetGridLineColor(Value: TColor);
begin
  if FGridLineColor <> Value then
  begin
    FGridLineColor := Value;
    Changed;
  end;
end;

procedure TdxCustomReportLinkOptionsFormatting.SetLookAndFeelKind(Value: TcxLookAndFeelKind);
begin
  if Value = lfOffice11 then Value := lfFlat;
  if FLookAndFeelKind <> Value then
  begin
    FLookAndFeelKind := Value;
    Changed;
  end;
end;

procedure TdxCustomReportLinkOptionsFormatting.SetSuppressBackgroundBitmaps(Value: Boolean);
begin
  if FSuppressBackgroundBitmaps <> Value then
  begin
    FSuppressBackgroundBitmaps := Value;
    Changed;
  end;
end;

procedure TdxCustomReportLinkOptionsFormatting.SetUseLookAndFeelColors(Value: Boolean);
begin
  if FUseLookAndFeelColors <> Value then
  begin
    FUseLookAndFeelColors := Value;
    if dxThemeManager.AreVisualStylesAvailable then Changed;
  end;
end;

procedure TdxCustomReportLinkOptionsFormatting.SetUseNativeStyles(Value: Boolean);
begin
  if FUseNativeStyles <> Value then
  begin
    FUseNativeStyles := Value;
    Changed;
  end;
end;

{ TdxCustomReportLinkOptionsRefinements }

constructor TdxCustomReportLinkOptionsRefinements.Create(AReportLink: TdxCustomcxControlReportLink);
begin
  inherited;
  FDisplayTrackBarsAsText := True;
end;

procedure TdxCustomReportLinkOptionsRefinements.Assign(Source: TPersistent);
begin
  if Source is TdxCustomReportLinkOptionsRefinements then
    with TdxCustomReportLinkOptionsRefinements(Source) do
    begin
      Self.DisplayGraphicsAsText := DisplayGraphicsAsText;
      Self.FlatCheckMarks := FlatCheckMarks;
      Self.GraphicsText := GraphicsText;
      Self.TransparentGraphics := TransparentGraphics;
      Self.TransparentRichEdits := TransparentRichEdits;
      Self.FIsGraphicsTextAssigned := FIsGraphicsTextAssigned;
    end;
  inherited;
end;

function TdxCustomReportLinkOptionsRefinements.DefaultGraphicsText: string;
begin
  Result := cxGetResourceString(@sdxGraphicAsTextValue);
end;

procedure TdxCustomReportLinkOptionsRefinements.RestoreDefaults;
begin
  inherited;
  DisplayGraphicsAsText := False;
  FlatCheckMarks := True;
  GraphicsText := '';
  TransparentGraphics := False;
  TransparentRichEdits := False;
  FIsGraphicsTextAssigned := False;
end;

procedure TdxCustomReportLinkOptionsRefinements.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('IsGraphicsAsTextAssigned', ReadIsGraphicsTextAssigned, WriteIsGraphicsTextAssigned,
    FIsGraphicsTextAssigned and (GraphicsText = ''));
end;

function TdxCustomReportLinkOptionsRefinements.GetGraphicsText: string;
begin
  if FIsGraphicsTextAssigned then
    Result := FGraphicsText
  else
    Result := DefaultGraphicsText;
end;

function TdxCustomReportLinkOptionsRefinements.IsGraphicsTextStored: Boolean;
begin
  Result := FIsGraphicsTextAssigned and (GraphicsText <> DefaultGraphicsText);
end;

procedure TdxCustomReportLinkOptionsRefinements.SetDisplayGraphicsAsText(Value: Boolean);
begin
  if FDisplayGraphicsAsText <> Value then
  begin
    FDisplayGraphicsAsText := Value;
    Changed;
  end;
end;

procedure TdxCustomReportLinkOptionsRefinements.SetDisplayTrackBarsAsText(Value: Boolean);
begin
  if FDisplayTrackBarsAsText <> Value then
  begin
    FDisplayTrackBarsAsText := Value;
    Changed;
  end;
end;

procedure TdxCustomReportLinkOptionsRefinements.SetFlatCheckMarks(Value: Boolean);
begin
  if FFlatCheckMarks <> Value then
  begin
    FFlatCheckMarks := Value;
    Changed;
  end;
end;

procedure TdxCustomReportLinkOptionsRefinements.SetGraphicsText(const Value: string);
begin
  if GraphicsText <> Value then
  begin
    FGraphicsText := Value;
    FIsGraphicsTextAssigned := True;
    if DisplayGraphicsAsText then Changed;
  end;
end;

procedure TdxCustomReportLinkOptionsRefinements.SetTransparentGraphics(Value: Boolean);
begin
  if FTransparentGraphics <> Value then
  begin
    FTransparentGraphics := Value;
    Changed;
  end;
end;

procedure TdxCustomReportLinkOptionsRefinements.SetTransparentRichEdits(Value: Boolean);
begin
  if FTransparentRichEdits <> Value then
  begin
    FTransparentRichEdits := Value;
    Changed;
  end;
end;

procedure TdxCustomReportLinkOptionsRefinements.ReadIsGraphicsTextAssigned(Reader: TReader);
begin
  FIsGraphicsTextAssigned := Reader.ReadBoolean;
end;

procedure TdxCustomReportLinkOptionsRefinements.WriteIsGraphicsTextAssigned(Writer: TWriter);
begin
  Writer.WriteBoolean(FIsGraphicsTextAssigned);
end;

{ TcxVerticalGridReportLinkOptionsSize }

procedure TdxCustomReportLinkOptionsSize.Assign(Source: TPersistent);
begin
  if Source is TdxCustomReportLinkOptionsSize then
    with TdxCustomReportLinkOptionsSize(Source) do
    begin
      Self.AutoWidth := AutoWidth;
    end;
  inherited;
end;

procedure TdxCustomReportLinkOptionsSize.RestoreDefaults;
begin
  inherited;
  AutoWidth := False;
end;

procedure TdxCustomReportLinkOptionsSize.SetAutoWidth(Value: Boolean);
begin
  if FAutoWidth <> Value then
  begin
    FAutoWidth := Value;
    Changed;
  end;
end;

{ TdxCustomcxControlReportLink }

constructor TdxCustomcxControlReportLink.Create(AOwner: TComponent);
begin
  inherited;
  FDelimitersHorz := TList.Create;
  FDelimitersVert := TList.Create;
  FImageLists := TList.Create;
  FOptions := TList.Create;
  FStyles := GetStylesClass.Create(Self);
  CreateOptions;
end;

destructor TdxCustomcxControlReportLink.Destroy;
begin
  FreeAndNil(FStyles);
  FreeAndNil(FImageLists);
  FreeAndNil(FDelimitersVert);
  FreeAndNil(FDelimitersHorz);
  DestroyOptions;
  FreeAndNil(FOptions);
  inherited;
end;

procedure TdxCustomcxControlReportLink.Assign(Source: TPersistent);
begin
  if Source is TdxCustomcxControlReportLink then
    with TdxCustomcxControlReportLink(Source) do
    begin
      Self.OptionsExpanding := OptionsExpanding;
      Self.OptionsFormatting := OptionsFormatting;
      Self.OptionsPagination := OptionsPagination;
      Self.OptionsRefinements := OptionsRefinements;
      Self.OptionsSize := OptionsSize;
      Self.OptionsView := OptionsView;
      Self.Styles := Styles;
      Self.SupportedCustomDraw := SupportedCustomDraw;
    end;
  inherited;
end;

procedure TdxCustomcxControlReportLink.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if not (csDestroying in ComponentState) and (Operation = opRemove) and (AComponent = StyleRepository) then
    StyleRepository := nil;
  if FOptions <> nil then
    NotifyOptions(AComponent, Operation);
end;

procedure TdxCustomcxControlReportLink.BeforeDesignReport;
begin
  DesignWindow.DesignerTabIndex := FDesignerTabIndex;
  inherited;
end;

procedure TdxCustomcxControlReportLink.ConvertCoords;
begin
  inherited;
  ConvertDelimiters(DelimitersHorz);
  ConvertDelimiters(DelimitersVert);
end;

procedure TdxCustomcxControlReportLink.GetImageLists(AProc: TdxPSGetImageListProc);
var
  I: Integer;
begin
  for I := 0 to ImageListCount - 1 do
    AProc(ImageLists[I]);
end;

procedure TdxCustomcxControlReportLink.InternalRestoreDefaults;
begin
  inherited;
  OptionsExpanding.RestoreDefaults;
  OptionsFormatting.RestoreDefaults;
  OptionsPagination.RestoreDefaults;
  OptionsRefinements.RestoreDefaults;
  OptionsSize.RestoreDefaults;
  OptionsView.RestoreDefaults;
  SupportedCustomDraw := False;
end;

function TdxCustomcxControlReportLink.IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean;
begin
  Result := SupportedCustomDraw;
end;

procedure TdxCustomcxControlReportLink.MakeDelimiters(AReportCells: TdxReportCells;
  AHorzDelimiters, AVertDelimiters: TList);
begin
  inherited;
  dxCopyList(DelimitersHorz, AHorzDelimiters);
  dxCopyList(DelimitersVert, AVertDelimiters);
end;

procedure TdxCustomcxControlReportLink.AddHorizontalDelimiter(ADelimiter: TdxReportCell);
var
  R: TRect;
begin
  R := ADelimiter.AbsoluteRect;
  AddHorizontalDelimiter(R.Left);
  AddHorizontalDelimiter(R.Right);
end;

procedure TdxCustomcxControlReportLink.AddHorizontalDelimiter(ADelimiter: Integer);
begin
  FDelimitersHorz.Add(TObject(ADelimiter));
end;

procedure TdxCustomcxControlReportLink.AddVerticalDelimiter(ADelimiter: TdxReportCell);
var
  R: TRect;
begin
  R := ADelimiter.AbsoluteRect;
  AddVerticalDelimiter(R.Top);
  AddVerticalDelimiter(R.Bottom);
end;

procedure TdxCustomcxControlReportLink.AddVerticalDelimiter(ADelimiter: Integer);
begin
  FDelimitersVert.Add(TObject(ADelimiter));
end;

procedure TdxCustomcxControlReportLink.CreateOptions;
begin
  FOptionsExpanding := GetOptionsExpandingClass.Create(Self);
  FOptionsFormatting := GetOptionsFormattingClass.Create(Self);
  FOptionsPagination := GetOptionsPaginationClass.Create(Self);
  FOptionsRefinements := GetOptionsRefinementsClass.Create(Self);
  FOptionsSize := GetOptionsSizeClass.Create(Self);
  FOptionsView := GetOptionsViewClass.Create(Self);
end;

procedure TdxCustomcxControlReportLink.DestroyOptions;
begin
  FreeAndNil(FOptionsView);
  FreeAndNil(FOptionsSize);
  FreeAndNil(FOptionsRefinements);
  FreeAndNil(FOptionsPagination);
  FreeAndNil(FOptionsFormatting);
  FreeAndNil(FOptionsExpanding);
end;

function TdxCustomcxControlReportLink.GetOptionsExpandingClass: TdxCustomReportLinkOptionsExpandingClass;
begin
  Result := TdxCustomReportLinkOptionsExpanding;
end;

function TdxCustomcxControlReportLink.GetOptionsFormattingClass: TdxCustomReportLinkOptionsFormattingClass;
begin
  Result := TdxCustomReportLinkOptionsFormatting;
end;

function TdxCustomcxControlReportLink.GetOptionsPaginationClass: TdxCustomReportLinkOptionsPaginationClass;
begin
  Result := TdxCustomReportLinkOptionsPagination;
end;

function TdxCustomcxControlReportLink.GetOptionsRefinementsClass: TdxCustomReportLinkOptionsRefinementsClass;
begin
  Result := TdxCustomReportLinkOptionsRefinements;
end;

function TdxCustomcxControlReportLink.GetOptionsSizeClass: TdxCustomReportLinkOptionsSizeClass;
begin
  Result := TdxCustomReportLinkOptionsSize;
end;

function TdxCustomcxControlReportLink.GetOptionsViewClass: TdxCustomReportLinkOptionsViewClass;
begin
  Result := TdxCustomReportLinkOptionsView;
end;

procedure TdxCustomcxControlReportLink.OptionsChanged(AnOptions: TdxCustomReportLinkOptions);
begin
  LinkModified(True);
end;

procedure TdxCustomcxControlReportLink.AddOptions(AnOptions: TdxCustomReportLinkOptions);
begin
  FOptions.Remove(AnOptions);
end;

procedure TdxCustomcxControlReportLink.NotifyOptions(AComponent: TComponent; AOperation: TOperation);
var
  I: Integer;
begin
  for I := 0 to OptionsCount - 1 do
    Options[I].Notification(AComponent, AOperation);
end;

procedure TdxCustomcxControlReportLink.RemoveOptions(AnOptions: TdxCustomReportLinkOptions);
begin
  FOptions.Add(AnOptions);
end;

procedure TdxCustomcxControlReportLink.AppendImageList(AnImageList: TCustomImageList);
begin
  if (AnImageList <> nil) and (FImageLists.IndexOf(AnImageList) = -1) then
    FImageLists.Add(AnImageList);
end;

function TdxCustomcxControlReportLink.CanCreateComponent: Boolean;
begin
  Result := (Owner = nil) or not (csInline in Owner.ComponentState);
end;

procedure TdxCustomcxControlReportLink.ConvertDelimiters(ADelimiters: TList);
var
  I, Value: Integer;
begin
  for I := 0 to ADelimiters.Count - 1 do
  begin
    Value := Integer(ADelimiters[I]);
    Value := MulDiv(Value, PixelsNumerator, PixelsDenominator);
    ADelimiters[I] := Pointer(Value);
  end;
end;

function TdxCustomcxControlReportLink.CreateStyle(const ACaption: string): TcxStyle;
begin
  Result := GetStyleRepository.CreateItemEx(TcxStyle, GetChildComponentOwner) as TcxStyle;
  if IsDesigning then
    Result.Name := GetUniqueName(GetChildComponentOwner, DropT(TcxStyle.ClassName));
  StyleRestoreDefaults(ACaption, Result);
  Styles.StylesByCaption[ACaption] := Result;
end;

function TdxCustomcxControlReportLink.CreateStyleRepository: TcxStyleRepository;
begin
  Result := TcxStyleRepository.Create(Owner);
  Result.ScaleForPPI(PixelsPerInch);
  if IsDesigning then
  begin
    Result.Name := GetUniqueName(Owner, DropT(TcxStyleRepository.ClassName));
    DesignerModified;
    MessageWarning(Format(cxGetResourceString(@sdxNewStyleRepositoryWasCreated), [Result.Name]));
  end;
end;

function TdxCustomcxControlReportLink.CreateStyleSheet(APrototype: TcxCustomStyleSheet;
  const ACaption: string; AUseStyles: Boolean): TdxCustomReportLinkStyleSheet;
var
  ANewStyles: TdxCustomReportLinkStyles;
  AStyle: TcxStyle;
  AStyleCaption: string;
  AStyles: TStrings;
  I: Integer;
begin
  Result := GetStyleRepository.CreateStyleSheetEx(GetStyleSheetClass, GetChildComponentOwner) as TdxCustomReportLinkStyleSheet;
  if IsDesigning then
    Result.Name := GetUniqueName(GetChildComponentOwner, DropT(GetStyleSheetClass.ClassName));
  Result.Caption := ACaption;

  if AUseStyles then
  begin
    DesignWindow.GetStyleNames(AStyles);
    ANewStyles := TdxCustomReportLinkStyles(Result.GetStyles);
    for I := 0 to AStyles.Count - 1 do
    begin
      AStyleCaption := AStyles[I];
      AStyle := TcxStyle(AStyles.Objects[I]);
      InitiateStyle(AStyleCaption, AStyle, AStyle = nil);// or (Style.AssignedValues = []));
      ANewStyles.StylesByCaption[AStyleCaption] := AStyle;
    end;
  end
  else
  begin
    if APrototype = nil then
      APrototype := GetStyleSheetPrototype;
    if APrototype <> nil then
    begin
      Result.CopyFrom(APrototype);
      if IsDesigning then
        for I := 0 to cxStyles_GetCount(Result.GetStyles) - 1 do
        begin
          AStyle := TcxStyle(Result.GetStyles.Values[I]);
          if AStyle <> nil then
            AStyle.Name := GetUniqueName(GetChildComponentOwner, DropT(TcxStyle.ClassName));
        end;
    end;
  end;
end;

function TdxCustomcxControlReportLink.FindStyleRepositoryInStyles(AStyles: TcxStyles): TcxStyleRepository;
var
  I: Integer;
  cxStyle: TcxCustomStyle;
  StyleSheet: TcxCustomStyleSheet;
begin
  for I := 0 to cxStyles_GetCount(AStyles) - 1 do
  begin
    cxStyle := AStyles.Values[I];
    if cxStyle <> nil then
    begin
      Result := cxStyle.StyleRepository;
      if Result <> nil then
        Exit;
    end;
  end;
  Result := nil;

  StyleSheet := cxStyles_GetStyleSheet(AStyles);
  if StyleSheet <> nil then
    Result := StyleSheet.StyleRepository;
end;

function TdxCustomcxControlReportLink.GetChildComponentOwner: TComponent;
begin
  Result := GetStyleRepository.Owner;
end;

function TdxCustomcxControlReportLink.GetStyleConsumerCount(AStyle: TcxStyle): Integer;

  function GetStyleConsumerCountInStyles(AStyles: TcxCustomStyles): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to cxStyles_GetCount(AStyles) - 1 do
      if AStyles.Values[I] = AStyle then
        Inc(Result);

    if (Result = 0) and (cxStyles_GetStyleSheet(AStyles) <> nil) then
    begin
      I := GetStyleConsumerCountInStyles(cxStyles_GetStyleSheet(AStyles).GetStyles);
      Inc(Result, I);
    end;
  end;

begin
  Result := GetStyleConsumerCountInStyles(Styles);
end;

function TdxCustomcxControlReportLink.GetStyleRepository: TcxStyleRepository;
begin
  Result := FStyleRepository;
  if Result = nil then
  begin
    Result := FindStyleRepositoryInStyles(Styles);
    if (Result = nil) and CanCreateComponent then
    begin
      FStyleRepository := CreateStyleRepository;
      Result := FStyleRepository;
    end;
  end;
end;

function TdxCustomcxControlReportLink.GetStyleSheetCaption(
  ACheckProc: TdxStyleSheetHasCaptionProc; var ACaption: string): Boolean;

  function QueryStyleSheetCaption(var ACaption: string): Boolean;
  begin
    Result := dxInputQuery(Application.Title, cxGetResourceString(@sdxCreateNewStyleQueryNamePrompt), ACaption);
  end;

  procedure Error(const ACaption: string);
  begin
    MessageError(Format(cxGetResourceString(@sdxStyleSheetNameAlreadyExists), [ACaption]));
  end;

var
  Unnamed: string;
  I: Integer;
begin
  if ACaption = '' then
  begin
    I := 1;
    Unnamed := cxGetResourceString(@sdxUnnamedStyleSheet);
    repeat
      ACaption := Unnamed + ' <' + IntToStr(I) + '>';
      Inc(I);
    until not ACheckProc(ACaption) or (I = MaxInt);
  end;

  repeat
    Result := QueryStyleSheetCaption(ACaption);
    if Result and ACheckProc(ACaption) then
      Error(ACaption);
  until not Result or not ACheckProc(ACaption);
end;

function TdxCustomcxControlReportLink.InitiateStyle(const ACaption: string;
  var AStyle: TcxStyle; AForceCreation: Boolean): Boolean;
begin
  Result := AForceCreation or (AStyle = nil) or (GetStyleConsumerCount(AStyle) > 1);
  if Result then
    AStyle := CreateStyle(ACaption);
end;

procedure TdxCustomcxControlReportLink.StyleRestoreDefaults(const ACaption: string; AStyle: TcxStyle);
var
  AParams: TcxViewParams;
begin
  Styles.GetDefaultViewParamsByCaption(ACaption, nil, AParams);

  AStyle.Bitmap.Assign(AParams.Bitmap);
  AStyle.Color := AParams.Color;
  AStyle.Font := AParams.Font;
  AStyle.TextColor := AParams.TextColor;
end;

procedure TdxCustomcxControlReportLink.StyleChanged(const ACaption: string;
  AStyle: TcxStyle);
begin
  if DesignWindow <> nil then
    DesignWindow.DoStyleChanged(ACaption, AStyle);
  LinkModified(True);
end;

function TdxCustomcxControlReportLink.GetAreNativeStylesAvailable: Boolean;
begin
  Result := False;
end;

function TdxCustomcxControlReportLink.GetStylesClass: TdxCustomReportLinkStylesClass;
begin
  Result := TdxCustomReportLinkStyles;
end;

function TdxCustomcxControlReportLink.GetStyleSheetClass: TdxCustomReportLinkStyleSheetClass;
begin
  Result := TdxCustomReportLinkStyleSheet;
end;

function TdxCustomcxControlReportLink.GetStyleSheetPrototype: TdxCustomReportLinkStyleSheet;
begin
  Result := nil;
end;

procedure TdxCustomcxControlReportLink.PrepareConstruct;
begin
  FScreenCanvas := TdxPSReportRenderScreenCanvas.Create;
  FDelimitersHorz.Clear;
  FDelimitersVert.Clear;
  FImageLists.Clear;
  if OptionsRefinements.DisplayGraphicsAsText then
    TdxPSGraphicAsTextDataMap.Register;
end;

procedure TdxCustomcxControlReportLink.UnprepareConstruct;
begin
  if OptionsRefinements.DisplayGraphicsAsText then
    TdxPSGraphicAsTextDataMap.Unregister;
  FreeAndNil(FScreenCanvas);
end;

function TdxCustomcxControlReportLink.GetActiveStyles: TdxCustomReportLinkStyles;
begin
  Result := Styles;
  if Result.StyleSheet <> nil then
    Result := Result.StyleSheet.GetStyles as TdxCustomReportLinkStyles;
end;

function TdxCustomcxControlReportLink.GetDesignWindow: TdxfmCustomcxControlReportLinkDesignWindow;
begin
  Result := inherited DesignWindow as TdxfmCustomcxControlReportLinkDesignWindow;
end;

function TdxCustomcxControlReportLink.GetEffects3D: Boolean;
begin
  Result := OptionsFormatting.LookAndFeelKind <> lfUltraFlat;
end;

function TdxCustomcxControlReportLink.GetImageList(Index: Integer): TCustomImageList;
begin
  Result := TCustomImageList(FImageLists[Index]);
end;

function TdxCustomcxControlReportLink.GetImageListCount: Integer;
begin
  Result := FImageLists.Count;
end;

function TdxCustomcxControlReportLink.GetOptions(Index: Integer): TdxCustomReportLinkOptions;
begin
  Result := TdxCustomReportLinkOptions(FOptions[Index]);
end;

function TdxCustomcxControlReportLink.GetOptionsCount: Integer;
begin
  if FOptions <> nil then
    Result := FOptions.Count
  else
    Result := 0;
end;

function TdxCustomcxControlReportLink.GetSoft3D: Boolean;
begin
  Result := OptionsFormatting.LookAndFeelKind = lfFlat;
end;

procedure TdxCustomcxControlReportLink.SetOptionsExpanding(Value: TdxCustomReportLinkOptionsExpanding);
begin
  OptionsExpanding.Assign(Value);
end;

procedure TdxCustomcxControlReportLink.SetOptionsFormatting(Value: TdxCustomReportLinkOptionsFormatting);
begin
  OptionsFormatting.Assign(Value);
end;

procedure TdxCustomcxControlReportLink.SetOptionsPagination(Value: TdxCustomReportLinkOptionsPagination);
begin
  OptionsPagination.Assign(Value);
end;

procedure TdxCustomcxControlReportLink.SetOptionsRefinements(Value: TdxCustomReportLinkOptionsRefinements);
begin
  OptionsRefinements.Assign(Value);
end;

procedure TdxCustomcxControlReportLink.SetOptionsSize(Value: TdxCustomReportLinkOptionsSize);
begin
  OptionsSize.Assign(Value);
end;

procedure TdxCustomcxControlReportLink.SetOptionsView(Value: TdxCustomReportLinkOptionsView);
begin
  OptionsView.Assign(Value);
end;

procedure TdxCustomcxControlReportLink.SetStyleRepository(Value: TcxStyleRepository);
begin
  if FStyleRepository <> Value then
  begin
    FStyleRepository := Value;
    if FStyleRepository <> nil then
      FStyleRepository.FreeNotification(Self);
  end;
end;

procedure TdxCustomcxControlReportLink.SetStyles(Value: TdxCustomReportLinkStyles);
begin
  Styles.Assign(Value);
end;

procedure TdxCustomcxControlReportLink.SetSupportedCustomDraw(Value: Boolean);
begin
  if FSupportedCustomDraw <> Value then
  begin
    FSupportedCustomDraw := Value;
    LinkModified(True);
  end;
end;

{ TdxCustomTableControlReportLinkOptions }

function TdxCustomTableControlReportLinkOptions.GetReportLink: TdxCustomTableControlReportLink;
begin
  Result := inherited ReportLink as TdxCustomTableControlReportLink;
end;

{ TdxCustomTableControlReportLinkOptionsOnEveryPage }

procedure TdxCustomTableControlReportLinkOptionsOnEveryPage.Assign(Source: TPersistent);
begin
  if Source is TdxCustomTableControlReportLinkOptionsOnEveryPage then
    with TdxCustomTableControlReportLinkOptionsOnEveryPage(Source) do
    begin
      Self.BandHeaders := BandHeaders;
      Self.Footers := Footers;
      Self.Headers := Headers;
    end;
  inherited;
end;

procedure TdxCustomTableControlReportLinkOptionsOnEveryPage.RestoreDefaults;
begin
  inherited;
  BandHeaders := True;
  Headers := True;
  Footers := True;
end;

procedure TdxCustomTableControlReportLinkOptionsOnEveryPage.SetBandHeaders(Value: Boolean);
begin
  if FBandHeaders <> Value then
  begin
    FBandHeaders := Value;
    Changed;
  end;
end;

procedure TdxCustomTableControlReportLinkOptionsOnEveryPage.SetFooters(Value: Boolean);
begin
  if FFooters <> Value then
  begin
    FFooters := Value;
    Changed;
  end;
end;

procedure TdxCustomTableControlReportLinkOptionsOnEveryPage.SetHeaders(Value: Boolean);
begin
  if FHeaders <> Value then
  begin
    FHeaders := Value;
    Changed;
  end;
end;

{ TdxCustomTableControlReportLinkOptionsPagination }

procedure TdxCustomTableControlReportLinkOptionsPagination.Assign(Source: TPersistent);
begin
  if Source is TdxCustomTableControlReportLinkOptionsPagination then
    with TdxCustomTableControlReportLinkOptionsPagination(Source) do
    begin
      Self.Band := Band;
      Self.Column := Column;
      Self.Custom := Custom;
      Self.Row := Row;
    end;
  inherited;
end;

procedure TdxCustomTableControlReportLinkOptionsPagination.RestoreDefaults;
begin
  inherited;
  Band := False;
  Column := True;
  Custom := False;
  Row := True;
end;

function TdxCustomTableControlReportLinkOptionsPagination.GetColumn: Boolean;
begin
  Result := ReportLink.UseHorzDelimiters;
end;

function TdxCustomTableControlReportLinkOptionsPagination.GetRow: Boolean;
begin
  Result := ReportLink.UseVertDelimiters;
end;

procedure TdxCustomTableControlReportLinkOptionsPagination.SetBand(Value: Boolean);
begin
  if FBand <> Value then
  begin
    FBand := Value;
    if Value then
      Column := False;
    Changed;
  end;
end;

procedure TdxCustomTableControlReportLinkOptionsPagination.SetColumn(Value: Boolean);
begin
  ReportLink.UseHorzDelimiters := Value;
  if Value then
    Band := False;
end;

procedure TdxCustomTableControlReportLinkOptionsPagination.SetCustom(Value: Boolean);
begin
  if FCustom <> Value then
  begin
    FCustom := Value;
    Changed;
  end;
end;

procedure TdxCustomTableControlReportLinkOptionsPagination.SetRow(Value: Boolean);
begin
  ReportLink.UseVertDelimiters := Value;
end;

{ TdxCustomTableControlReportLinkOptionsPreview }

procedure TdxCustomTableControlReportLinkOptionsPreview.Assign(Source: TPersistent);
begin
  if Source is TdxCustomTableControlReportLinkOptionsPreview then
    with TdxCustomTableControlReportLinkOptionsPreview(Source) do
    begin
      Self.AutoHeight := AutoHeight;
      Self.MaxLineCount := MaxLineCount;
      Self.Visible := Visible;
    end;
  inherited;
end;

procedure TdxCustomTableControlReportLinkOptionsPreview.RestoreDefaults;
begin
  inherited;
  AutoHeight := True;
  MaxLineCount := 0;
  Visible := True;
end;

procedure TdxCustomTableControlReportLinkOptionsPreview.SetAutoHeight(Value: Boolean);
begin
  if FAutoHeight <> Value then
  begin
    FAutoHeight := Value;
    if Visible then Changed;
  end;
end;

procedure TdxCustomTableControlReportLinkOptionsPreview.SetMaxLineCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FMaxLineCount <> Value then
  begin
    FMaxLineCount := Value;
    if Visible then Changed;
  end;
end;

procedure TdxCustomTableControlReportLinkOptionsPreview.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TdxCustomTableControlReportLinkOptionsSelection }

procedure TdxCustomTableControlReportLinkOptionsSelection.Assign(Source: TPersistent);
begin
  if Source is TdxCustomTableControlReportLinkOptionsSelection then
    with TdxCustomTableControlReportLinkOptionsSelection(Source) do
    begin
      Self.ProcessExactSelection := ProcessExactSelection;
      Self.ProcessSelection := ProcessSelection;
    end;
  inherited;
end;

procedure TdxCustomTableControlReportLinkOptionsSelection.RestoreDefaults;
begin
  inherited;
  ProcessExactSelection := False;
  ProcessSelection := False;
end;

procedure TdxCustomTableControlReportLinkOptionsSelection.SetProcessExactSelection(Value: Boolean);
begin
  if FProcessExactSelection <> Value then
  begin
    FProcessExactSelection := Value;
    if ProcessSelection then Changed;
  end;
end;

procedure TdxCustomTableControlReportLinkOptionsSelection.SetProcessSelection(Value: Boolean);
begin
  if FProcessSelection <> Value then
  begin
    FProcessSelection := Value;
    Changed;
  end;
end;

{ TdxCustomTableControlReportLinkOptionsView }

procedure TdxCustomTableControlReportLinkOptionsView.Assign(Source: TPersistent);
begin
  if Source is TdxCustomTableControlReportLinkOptionsView then
    with TdxCustomTableControlReportLinkOptionsView(Source) do
    begin
      Self.BandHeaders := BandHeaders;
      Self.ExpandButtons := ExpandButtons;
      Self.Footers := Footers;
      Self.Headers := Headers;
    end;
  inherited;
end;

procedure TdxCustomTableControlReportLinkOptionsView.RestoreDefaults;
begin
  inherited;
  BandHeaders := True;
  ExpandButtons := True;
  Footers := True;
  Headers := True;
end;

procedure TdxCustomTableControlReportLinkOptionsView.SetBandHeaders(Value: Boolean);
begin
  if FBandHeaders <> Value then
  begin
    FBandHeaders := Value;
    Changed;
  end;
end;

procedure TdxCustomTableControlReportLinkOptionsView.SetExpandButtons(Value: Boolean);
begin
  if FExpandButtons <> Value then
  begin
    FExpandButtons := Value;
    Changed;
  end;
end;

procedure TdxCustomTableControlReportLinkOptionsView.SetFooters(Value: Boolean);
begin
  if FFooters <> Value then
  begin
    FFooters := Value;
    Changed;
  end;
end;

procedure TdxCustomTableControlReportLinkOptionsView.SetHeaders(Value: Boolean);
begin
  if FHeaders <> Value then
  begin
    FHeaders := Value;
    Changed;
  end;
end;

{ TdxCustomTableControlReportLink }

procedure TdxCustomTableControlReportLink.Assign(Source: TPersistent);
begin
  if Source is TdxCustomTableControlReportLink then
    with TdxCustomTableControlReportLink(Source) do
    begin
      Self.OptionsOnEveryPage := OptionsOnEveryPage;
      Self.OptionsPreview := OptionsPreview;
      Self.OptionsSelection := OptionsSelection;
    end;
  inherited;
end;

procedure TdxCustomTableControlReportLink.InternalRestoreDefaults;
begin
  inherited;
  OptionsOnEveryPage.RestoreDefaults;
  OptionsPreview.RestoreDefaults;
  OptionsSelection.RestoreDefaults;
end;

procedure TdxCustomTableControlReportLink.CreateOptions;
begin
  inherited;
  FOptionsOnEveryPage := GetOptionsOnEveryPageClass.Create(Self);
  FOptionsPreview := GetOptionsPreviewClass.Create(Self);
  FOptionsSelection := GetOptionsSelectionClass.Create(Self);
end;

procedure TdxCustomTableControlReportLink.DestroyOptions;
begin
  FreeAndNil(FOptionsSelection);
  FreeAndNil(FOptionsPreview);
  FreeAndNil(FOptionsOnEveryPage);
  inherited;
end;

function TdxCustomTableControlReportLink.GetOptionsOnEveryPageClass: TdxCustomTableControlReportLinkOptionsOnEveryPageClass;
begin
  Result := TdxCustomTableControlReportLinkOptionsOnEveryPage;
end;

function TdxCustomTableControlReportLink.GetOptionsPaginationClass: TdxCustomReportLinkOptionsPaginationClass;
begin
  Result := TdxCustomReportLinkOptionsPagination;
end;

function TdxCustomTableControlReportLink.GetOptionsPreviewClass: TdxCustomTableControlReportLinkOptionsPreviewClass;
begin
  Result := TdxCustomTableControlReportLinkOptionsPreview;
end;

function TdxCustomTableControlReportLink.GetOptionsSelectionClass: TdxCustomTableControlReportLinkOptionsSelectionClass;
begin
  Result := TdxCustomTableControlReportLinkOptionsSelection;
end;

function TdxCustomTableControlReportLink.GetOptionsViewClass: TdxCustomReportLinkOptionsViewClass;
begin
  Result := TdxCustomTableControlReportLinkOptionsView;
end;

function TdxCustomTableControlReportLink.GetOptionsPagination: TdxCustomTableControlReportLinkOptionsPagination;
begin
  Result := inherited OptionsPagination as TdxCustomTableControlReportLinkOptionsPagination;
end;

function TdxCustomTableControlReportLink.GetOptionsView: TdxCustomTableControlReportLinkOptionsView;
begin
  Result := inherited OptionsView as TdxCustomTableControlReportLinkOptionsView;
end;

procedure TdxCustomTableControlReportLink.SetOptionsOnEveryPage(Value: TdxCustomTableControlReportLinkOptionsOnEveryPage);
begin
  OptionsOnEveryPage.Assign(Value);
end;

procedure TdxCustomTableControlReportLink.SetOptionsPagination(Value: TdxCustomTableControlReportLinkOptionsPagination);
begin
  inherited OptionsPagination := Value;
end;

procedure TdxCustomTableControlReportLink.SetOptionsPreview(Value: TdxCustomTableControlReportLinkOptionsPreview);
begin
  OptionsPreview.Assign(Value);
end;

procedure TdxCustomTableControlReportLink.SetOptionsSelection(Value: TdxCustomTableControlReportLinkOptionsSelection);
begin
  OptionsSelection.Assign(Value);
end;

procedure TdxCustomTableControlReportLink.SetOptionsView(Value: TdxCustomTableControlReportLinkOptionsView);
begin
  inherited OptionsView := Value;
end;

{ TdxfmCustomcxControlReportLinkDesignWindow }

procedure TdxfmCustomcxControlReportLinkDesignWindow.DoInitialize;
begin
  inherited;
  RefreshStyleSheetList;
end;

function TdxfmCustomcxControlReportLinkDesignWindow.CanCopyStyleSheet: Boolean;
begin
  Result := CanCreateStyleSheet and (ActiveStyleSheet <> nil);
end;

function TdxfmCustomcxControlReportLinkDesignWindow.CanCreateStyleSheet: Boolean;
begin
  Result := AreNativeStylesAvailable and ReportLink.CanCreateComponent;
end;

function TdxfmCustomcxControlReportLinkDesignWindow.CanDeleteStyleSheet: Boolean;
begin
  Result := AreNativeStylesAvailable and (ActiveStyleSheet <> nil) and
    (IsDesigning or not ActiveStyleSheet.BuiltIn);
end;

function TdxfmCustomcxControlReportLinkDesignWindow.CanRenameStyleSheet: Boolean;
begin
  Result := AreNativeStylesAvailable and (ActiveStyleSheet <> nil) and
    (IsDesigning or not ActiveStyleSheet.BuiltIn);
end;

function TdxfmCustomcxControlReportLinkDesignWindow.CanSaveStyles: Boolean;
begin
  Result := AreNativeStylesAvailable and ReportLink.CanCreateComponent;
end;

function TdxfmCustomcxControlReportLinkDesignWindow.PerformStyleSheetCopy: Boolean;
var
  Caption: string;
begin
  Result := CanCopyStyleSheet;
  if Result then
  begin
    Caption := '';
    if (ActiveStyleSheet <> nil) and (ActiveStyleSheet.Caption <> '') then
      Caption := cxGetResourceString(@sdxCopyOfItem) + ActiveStyleSheet.Caption;

    Result := GetStyleSheetCaption(Caption);
    if Result then
    begin
      ActiveStyleSheet := ReportLink.CreateStyleSheet(ActiveStyleSheet, Caption, False);
      RefreshStyleSheetList;
    end;
  end;
end;

function TdxfmCustomcxControlReportLinkDesignWindow.PerformStyleSheetDelete: Boolean;

  function QueryDeleteStyleSheet(AStyleSheet: TcxCustomStyleSheet): Boolean;
  begin
    Result := dxPSUtl.MessageQuestion(Format(cxGetResourceString(@sdxDeleteStyleSheet), [AStyleSheet.Caption]));
  end;

var
  StyleSheet: TcxCustomStyleSheet;
  Index: Integer;
begin
  Result := CanDeleteStyleSheet;
  if Result then
  begin
    StyleSheet := ActiveStyleSheet;
    Result := (StyleSheet <> nil) and QueryDeleteStyleSheet(StyleSheet);
    if Result then
    begin
      Index := StyleSheet.Index;
      StyleSheet.Free;
      if Index >= StyleRepository.StyleSheetCount then
        Index := StyleRepository.StyleSheetCount - 1;
      if Index <> -1 then
        ActiveStyleSheet := StyleRepository.StyleSheets[Index];
      RefreshStyleSheetList;
    end;
  end;
end;

procedure TdxfmCustomcxControlReportLinkDesignWindow.PerformStyleSheetDrawItem(ACanvas: TCanvas;
  AnIndex: Integer; R: TRect; AState: TOwnerDrawState; AEnabled: Boolean);

  function CalculateMaxWidth(AStrings: TStrings): Integer;
  var
    I, W: Integer;
  begin
    Result := 0;
    with AStrings do
      for I := 0 to Count - 1 do
      begin
        W := ACanvas.TextWidth(Strings[I]);
        if Result < W then Result := W;
      end;
  end;

var
  StyleSheets: TStrings;
  StyleSheet: TcxCustomStyleSheet;
  X, Y, W: Integer;
  S: string;
begin
  with ACanvas do
  begin
    FillRect(R);

    GetStyleSheetNames(StyleSheets);
    if StyleSheets <> nil then
    begin
      with R do
      begin
        X := Left + ScaleFactor.Apply(2);
        Y := Top + (Bottom - Top - cxTextHeight(Handle)) div 2;
      end;
      S := StyleSheets[AnIndex];
      if not AEnabled then
        Font.Color := clBtnShadow;
      TextOut(X, Y, S);

      if not IsDesigning then
      begin
        StyleSheet := TcxCustomStyleSheet(StyleSheets.Objects[AnIndex]);
        if StyleSheet.BuiltIn then
          S := cxGetResourceString(@sdxBuiltIn)
        else
          S := cxGetResourceString(@sdxUserDefined);
        W := CalculateMaxWidth(StyleSheets);
        if W <> 0 then
          X := R.Left + W + ScaleFactor.Apply(15);

        if not (odSelected in AState) and AEnabled then
          Font.Color := clBlue;
        TextOut(X, Y, S);
        Font.Color := clWindowText;
      end;
    end;
  end;
end;

function TdxfmCustomcxControlReportLinkDesignWindow.PerformStyleSheetKeyDown(Sender: TObject;
  var AKey: Word; AShift: TShiftState): Boolean;
begin
  Result := True;
  case AKey of
    VK_F2:
      Result := PerformStyleSheetRename;
    VK_INSERT:
      if ssCtrl in AShift then
        Result := PerformStyleSheetCopy
      else
        Result := PerformStyleSheetNew;
    VK_DELETE:
      Result := PerformStyleSheetDelete;
  end;
end;

function TdxfmCustomcxControlReportLinkDesignWindow.PerformStyleSheetNew: Boolean;
var
  Caption: string;
begin
  Caption := '';
  Result := CanCreateStyleSheet and GetStyleSheetCaption(Caption);
  if Result then
    ActiveStyleSheet := ReportLink.CreateStyleSheet(nil, Caption, False);
end;

function TdxfmCustomcxControlReportLinkDesignWindow.PerformStyleSheetRename: Boolean;
var
  Caption: string;
begin
  Caption := ActiveStyleSheet.Caption;
  Result := CanRenameStyleSheet and GetStyleSheetCaption(Caption);
  if Result then
  begin
    ActiveStyleSheet.Caption := Caption;
    RefreshStyleSheetList;
  end;
end;

function TdxfmCustomcxControlReportLinkDesignWindow.PerformStylesChangeBitmap: Boolean;

  procedure ShowError(const AFileName: string);
  begin
    dxPSUtl.MessageError(Format(cxGetResourceString(@sdxCannotLoadImage), [AFileName]));
  end;

var
  Styles: TStrings;
  Picture: TPicture;
  I: Integer;
  cxStyle: TcxStyle;
begin
  Styles := TStringList.Create;
  try
    GetSelectedStyleNames(Styles);
    Result := Styles.Count <> 0;
    if Result then
    begin
      Picture := TPicture.Create;
      try
        Result := dxPSPictureDialog.Execute;
        if Result then
        begin
          try
            Picture.LoadFromFile(dxPSPictureDialog.FileName);
            dxPSUtl.ForcePictureToBitmap(Picture);
          except
            ShowError(dxPSPictureDialog.FileName);
            raise;
          end;

          for I := 0 to Styles.Count - 1 do
          begin
            cxStyle := TcxStyle(Styles.Objects[I]);
            InitiateStyle(Styles[I], cxStyle, False);
            cxStyle.Bitmap := Picture.Bitmap;
          end;
          DoStylesChanged(Styles, False);
        end;
      finally
        Picture.Free;
      end;
    end;
  finally
    Styles.Free;
  end;
end;

function TdxfmCustomcxControlReportLinkDesignWindow.PerformStylesChangeColor: Boolean;
var
  AParams: TcxViewParams;
  AParamsScaleFactor: TdxScaleFactor;
  AStyle: TcxStyle;
  AStyles: TStrings;
  I: Integer;
begin
  AStyles := TStringList.Create;
  try
    GetSelectedStyleNames(AStyles);
    Result := AStyles.Count <> 0;
    if Result then
    begin
      AStyle := TcxStyle(AStyles.Objects[0]);
      dxPSGetViewParams(AStyle, AStyles[0], ReportLink.Styles, AParams, AParamsScaleFactor);
      dxPSGlbl.ColorDialog.Color := AParams.Color;

      Result := dxPSGlbl.ColorDialog.Execute;
      if Result then
      begin
        for I := 0 to AStyles.Count - 1 do
        begin
          AStyle := TcxStyle(AStyles.Objects[I]);
          InitiateStyle(AStyles[I], AStyle, False);
          AStyle.Color := dxPSGlbl.ColorDialog.Color;
        end;
        DoStylesChanged(AStyles, False);
      end;
    end;
  finally
    AStyles.Free;
  end;
end;

function TdxfmCustomcxControlReportLinkDesignWindow.PerformStylesChangeFont: Boolean;
var
  AParams: TcxViewParams;
  AParamsScaleFactor: TdxScaleFactor;
  AStyle: TcxStyle;
  AStyles: TStrings;
  I: Integer;
begin
  AStyles := TStringList.Create;
  try
    GetSelectedStyleNames(AStyles);
    Result := AStyles.Count <> 0;
    if Result then
    begin
      AStyle := TcxStyle(AStyles.Objects[0]);
      dxPSGetViewParams(AStyle, AStyles[0], ReportLink.Styles, AParams, AParamsScaleFactor);
      dxPSGlbl.FontDialog.Font := AParams.Font;
      dxPSGlbl.FontDialog.Font.Color := AParams.TextColor;

      Result := dxPSGlbl.FontDialog.Execute;
      if Result then
      begin
        for I := 0 to AStyles.Count - 1 do
        begin
          AStyle := TcxStyle(AStyles.Objects[I]);
          InitiateStyle(AStyles[I], AStyle, False);
          AStyle.Font := dxPSGlbl.FontDialog.Font;
          AStyle.TextColor := AStyle.Font.Color;
        end;
        DoStylesChanged(AStyles, True);
      end;
    end;
  finally
    AStyles.Free;
  end;
end;

function TdxfmCustomcxControlReportLinkDesignWindow.PerformStylesClearBitmap: Boolean;
var
  Names: TStrings;
  I: Integer;
  cxStyle: TcxStyle;
begin
  Names := TStringList.Create;
  try
    GetSelectedStyleNames(Names);
    Result := Names.Count <> 0;

    for I := 0 to Names.Count - 1 do
    begin
      cxStyle := TcxStyle(Names.Objects[I]);
      if cxStyle <> nil then
        cxStyle.Bitmap := nil;
    end;
    DoStylesChanged(Names, False);
  finally
    Names.Free;
  end;
end;

function TdxfmCustomcxControlReportLinkDesignWindow.PerformStylesRestoreDefaults: Boolean;
var
  Names: TStrings;
  I: Integer;
  cxStyle: TcxStyle;
begin
  Names := TStringList.Create;
  try
    GetSelectedStyleNames(Names);
    Result := Names.Count <> 0;
    for I := 0 to Names.Count - 1 do
    begin
      cxStyle := TcxStyle(Names.Objects[I]);
      if cxStyle <> nil then
        ReportLink.StyleRestoreDefaults(Names[I], cxStyle);
    end;
    DoStylesChanged(Names, True);
  finally
    Names.Free;
  end;
end;

function TdxfmCustomcxControlReportLinkDesignWindow.PerformStylesSaveAsStyleSheet: Boolean;
var
  Caption: string;
begin
  Caption := '';
  Result := CanSaveStyles and GetStyleSheetCaption(Caption);
  if Result then
    ActiveStyleSheet := ReportLink.CreateStyleSheet(nil, Caption, True);
end;

procedure TdxfmCustomcxControlReportLinkDesignWindow.RefreshStyleSheetList;
var
  Names: TStrings;
  K, I: Integer;
  Unnamed, S: string;
  StyleSheet: TcxCustomStyleSheet;
begin
  GetStyleSheetNames(Names);
  if Names <> nil then
  begin
    Names.BeginUpdate;
    try
      Names.Clear;
      if StyleRepository <> nil then
      begin
        K := 0;
        Unnamed := cxGetResourceString(@sdxUnnamedStyleSheet);
        for I := 0 to StyleRepository.StyleSheetCount - 1 do
        begin
          StyleSheet := StyleRepository.StyleSheets[I];
          if StyleSheet is ReportLink.GetStyleSheetClass then
          begin
            S := StyleSheet.Caption;
            if S = '' then
            begin
              Inc(K);
              S := Unnamed + ' <' + IntToStr(K) + '>';
            end;
            Names.AddObject(S, StyleSheet);
          end;
        end;
      end;
      DoActiveStyleSheetChanged;
    finally
      Names.EndUpdate;
    end;
  end;
end;

procedure TdxfmCustomcxControlReportLinkDesignWindow.RefreshStylesList;
begin
  DoRefreshStylesList;
end;

procedure TdxfmCustomcxControlReportLinkDesignWindow.DoActiveStyleSheetChanged;
begin
  RefreshStylesList;
  //DoStylesChanged(nil, True);
end;

procedure TdxfmCustomcxControlReportLinkDesignWindow.DoFormActivated(AnActive: Boolean);
begin
end;

procedure TdxfmCustomcxControlReportLinkDesignWindow.DoRefreshStylesList;
begin
end;

procedure TdxfmCustomcxControlReportLinkDesignWindow.DoStyleChanged(const ACaption: string;
  AStyle: TcxStyle);
begin
end;

procedure TdxfmCustomcxControlReportLinkDesignWindow.DoStylesChanged(AStrings: TStrings;
  ARecreate: Boolean);
begin
end;

function TdxfmCustomcxControlReportLinkDesignWindow.GetDesignerTabIndex: Integer;
begin
  Result := 0;
end;

procedure TdxfmCustomcxControlReportLinkDesignWindow.SetDesignerTabIndex(Value: Integer);
begin
end;

procedure TdxfmCustomcxControlReportLinkDesignWindow.GetSelectedStyleNames(AStrings: TStrings);
begin
end;

procedure TdxfmCustomcxControlReportLinkDesignWindow.GetStyleNames(out AStrings: TStrings);
begin
  AStrings := nil;
end;

procedure TdxfmCustomcxControlReportLinkDesignWindow.GetStyleSheetNames(out AStrings: TStrings);
begin
  AStrings := nil;
end;

function TdxfmCustomcxControlReportLinkDesignWindow.GetStyleConsumerCount(AStyle: TcxStyle): Integer;
begin
  Result := ReportLink.GetStyleConsumerCount(AStyle);
end;

function TdxfmCustomcxControlReportLinkDesignWindow.GetStyleSheetCaption(var ACaption: string): Boolean;
begin
  Result := ReportLink.GetStyleSheetCaption(HasStyleSheetWithCaption, ACaption);
end;

function TdxfmCustomcxControlReportLinkDesignWindow.HasStyleSheetWithCaption(const ACaption: string): Boolean;
var
  Strings: TStrings;
  I: Integer;
begin
  Result := True;
  GetStyleSheetNames(Strings);
  if Strings <> nil then
    for I := 0 to Strings.Count - 1 do
      if dxSameText(Strings[I], ACaption) then Exit;
  Result := False;
end;

procedure TdxfmCustomcxControlReportLinkDesignWindow.InitiateStyle(const ACaption: string;
  var AStyle: TcxStyle; AForceCreation: Boolean);
var
  Names: TStrings;
begin
  if ReportLink.InitiateStyle(ACaption, AStyle, AForceCreation) then
  begin
    GetStyleNames(Names);
    if Names <> nil then
      Names.Objects[Names.IndexOf(ACaption)] := AStyle;
  end;
end;

function TdxfmCustomcxControlReportLinkDesignWindow.GetActiveStyleSheet: TcxCustomStyleSheet;
begin
  Result := ReportLink.Styles.StyleSheet;
end;

function TdxfmCustomcxControlReportLinkDesignWindow.GetAreNativeStylesAvailable: Boolean;
begin
  Result := ReportLink.AreNativeStylesAvailable;
end;

function TdxfmCustomcxControlReportLinkDesignWindow.GetReportLink: TdxCustomcxControlReportLink;
begin
  Result := inherited ReportLink as TdxCustomcxControlReportLink;
end;

function TdxfmCustomcxControlReportLinkDesignWindow.GetStyleRepository: TcxStyleRepository;
begin
  Result := ReportLink.StyleRepository;
end;

procedure TdxfmCustomcxControlReportLinkDesignWindow.SetActiveStyleSheet(Value: TcxCustomStyleSheet);
begin
  if ReportLink.Styles.StyleSheet <> Value then
  begin
    ReportLink.Styles.StyleSheet := Value;
    RefreshStyleSheetList;
  end;
end;

procedure TdxfmCustomcxControlReportLinkDesignWindow.WMActivate(var message: TWMActivate);
begin
  inherited;
  DoFormActivated(message.Active <> WA_INACTIVE);
end;

{ TdxStylesListBoxToolTipsWindow }

procedure TdxStylesListBoxToolTipsWindow.Activate(const R: TRect; AnIndex: Integer);
const
  ShowFlags = SWP_SHOWWINDOW or SWP_NOACTIVATE;
begin
  Application.CancelHint;
  FStyleIndex := AnIndex;
  with R do
    SetWindowPos(Handle, HWND_TOPMOST, Left, Top, Right - Left, Bottom - Top, ShowFlags);
  Update;
end;

procedure TdxStylesListBoxToolTipsWindow.Deactivate;
const
  HideFlags =  SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW;
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0, HideFlags);
  FStyleIndex := -1;
end;

function TdxStylesListBoxToolTipsWindow.GetListBox: TdxStylesListBox;
begin
  Result := TdxStylesListBox(Owner);
end;

procedure TdxStylesListBoxToolTipsWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    Style := WS_POPUP;// or WS_DISABLED;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    if IsWinXPOrLater then
      WindowClass.Style := WindowClass.Style or CS_DROPSHADOW;
  end;
end;

procedure TdxStylesListBoxToolTipsWindow.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  DrawBorder(Canvas.Handle, R);
  InflateRect(R, -1, -1);
  DrawStyle(R);
end;

procedure TdxStylesListBoxToolTipsWindow.CalculateStyleBarViewInfo(const R: TRect; var AViewInfo: TdxStyleBarViewInfo);
var
  cxStyle: TcxStyle;
  Pt: TPoint;
begin
  with ListBox do
  begin
    cxStyle := ReportLinkStyles.StylesByCaption[HotTrackStyleName];
    CalculateStyleBarViewInfo(HotTrackStyleName, StyleIndex, cxStyle, R, False, ScaleFactor, AViewInfo);
  end;
  with AViewInfo do
  begin
    BarColor := clInfoBk;

    Pt := R.TopLeft;
    MapWindowPoints(Handle, ListBox.Handle, Pt, 1);
    StyleBitmapOrg.X := -(Pt.X - R.Left);
    StyleBitmapOrg.Y := -(Pt.Y - R.Top);
  end;
end;

procedure TdxStylesListBoxToolTipsWindow.DrawBorder(DC: HDC; const R: TRect);
var
  R2: TRect;
begin
  if cxIsVCLThemesEnabled then
  begin
    dxDrawThemeEdge(DC, cxStyleServices.GetElementDetails(twWindowRoot), R, BDR_RAISEDOUTER, BF_RECT);
    Exit;
  end;
  R2 := R;
  DrawEdge(DC, R2, BDR_RAISEDOUTER, BF_RECT)
end;

procedure TdxStylesListBoxToolTipsWindow.DrawStyle(R: TRect);
var
  ViewInfo: TdxStyleBarViewInfo;
begin
  dxPScxCommon.InitializeStyleBarViewInfo(ViewInfo);
  try
    CalculateStyleBarViewInfo(R, ViewInfo);
    dxPScxCommon.dxPSDrawStyleBar(Canvas, R, ViewInfo);
  finally
    dxPScxCommon.DeinitializeStyleBarViewInfo(ViewInfo);
  end;
end;

procedure TdxStylesListBoxToolTipsWindow.WMEraseBkgnd(var message: TWMEraseBkgnd);
begin
  message.Result := 1;
end;

procedure TdxStylesListBoxToolTipsWindow.WMNCHitTest(var message: TWMNCHitTest);
begin
  message.Result := HTTRANSPARENT;
end;

{ TdxStylesListBox }

constructor TdxStylesListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FToolTips := True;
  MultiSelect := True;
  HotTrackStyleIndex := -1;
  ListStyle := lbOwnerDrawVariable;
  OnMeasureItem := DoMeasureItem;
  FToolTipsLongHideTimer := CreateTimer(5000, ToolTipsLongHideTimerHandler);
  FToolTipsShortHideTimer := CreateTimer(1, ToolTipsShortHideTimerHandler);
end;

function TdxStylesListBox.CreateTimer(AInterval: Integer; ATimerEvent: TNotifyEvent): TTimer;
begin
  Result := TTimer.Create(Self);
  Result.Enabled := False;
  Result.Interval := AInterval;
  Result.OnTimer := ATimerEvent;
end;

procedure TdxStylesListBox.HideToolTips;
begin
  if Assigned(ToolTipsWindow) then
    ToolTipsWindow.Deactivate;
  HotTrackStyleIndex := -1;
  FToolTipsLongHideTimer.Enabled := False;
  FToolTipsShortHideTimer.Enabled := False;
end;

function TdxStylesListBox.IndexOfStyle(AStyle: TcxStyle): Integer;
begin
  Result := Items.IndexOfObject(AStyle);
end;

procedure TdxStylesListBox.DoMeasureItem(AControl: TcxListBox; AIndex: Integer; var Height: Integer);
var
  AHeight: Integer;
  AStyleFont: TFont;
  AStyleFontScaleFactor: TdxScaleFactor;
  AText: string;
begin
  AText := Items[AIndex];
  Canvas.Font := Font;
  AHeight := Canvas.TextHeight(AText);
  if GetStyleFontByText(AText, AStyleFont, AStyleFontScaleFactor) then
    dxAssignFont(Canvas.Font, AStyleFont, ScaleFactor, AStyleFontScaleFactor);
  Height := Max(cxTextHeight(Canvas.Handle), AHeight) + ScaleFactor.Apply(2);
end;

function TdxStylesListBox.DrawItem(ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState): Boolean;
var
  AStyle: TcxStyle;
  AText: string;
  AViewInfo: TdxStyleBarViewInfo;
begin
  Result := True;
  InitializeStyleBarViewInfo(AViewInfo);
  try
    AText := Items[AIndex];
    AStyle := ReportLinkStyles.StylesByCaption[AText];
    CalculateStyleBarViewInfo(AText, -1, AStyle, ARect, odSelected in AState, ScaleFactor, AViewInfo);
    dxPSDrawStyleBar(ACanvas.Canvas, ARect, AViewInfo);
    Perform(LB_SETHORIZONTALEXTENT, AViewInfo.MaxWidth, 0);
  finally
    DeinitializeStyleBarViewInfo(AViewInfo);
  end;
end;

procedure TdxStylesListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if ToolTips then
    UpdateToolTips(Point(X, Y));
end;

procedure TdxStylesListBox.CalculateStyleBarViewInfoSizes(AnIndex: Integer;
  const R: TRect; AScaleFactor: TdxScaleFactor; var AViewInfo: TdxStyleBarViewInfo);

  procedure CalculateStyleBarViewInfoSizesForRow(AnIndex: Integer; var ACaptionMaxWidth, AFontInfoMaxWidth: Integer);
  var
    AStyleFont: TFont;
    AStyleFontScaleFactor: TdxScaleFactor;
    AText: string;
  begin
    AText := Items[AnIndex];
    Canvas.Font := Font;
    ACaptionMaxWidth := Canvas.TextWidth(AText);
    if GetStyleFontByText(AText, AStyleFont, AStyleFontScaleFactor) then
      dxAssignFont(Canvas.Font, AStyleFont, ScaleFactor, AStyleFontScaleFactor);
    AFontInfoMaxWidth := Canvas.TextWidth(FormatFontInfo(Canvas.Font)) + AScaleFactor.Apply(2);
  end;

  function GetMaxValue(AList: TList): Integer;
  begin
    AList.Sort(dxCompareValues);
    Result := Integer(AList[0]);
  end;

var
  ACaptionMaxWidth: Integer;
  ATempFont: TFont;
  AWidthList: TList;
  I, W, FW: Integer;
begin
  AWidthList := TList.Create;
  try
    AWidthList.Count := Items.Count;

    Canvas.Font := Font;
    ATempFont := TFont.Create;
    try
      ATempFont.Assign(Canvas.Font);
      ACaptionMaxWidth := 0;
      for I := 0 to Items.Count - 1 do
      begin
        FW := Integer(AWidthList[I]);
        CalculateStyleBarViewInfoSizesForRow(I, W, FW);
        AWidthList[I] := Pointer(FW);
        ACaptionMaxWidth := Max(W, ACaptionMaxWidth);
      end;
      if AnIndex <> -1 then
      begin
        AWidthList.Count := 1;
        FW := Integer(AWidthList[0]);
        CalculateStyleBarViewInfoSizesForRow(AnIndex, W, FW);
        AWidthList[0] := Pointer(FW);
        Canvas.Font := ATempFont;
      end;
    finally
      ATempFont.Free;
    end;
    if ACaptionMaxWidth <> 0 then
      Inc(ACaptionMaxWidth, AScaleFactor.Apply(6));

    AViewInfo.MaxWidth := ACaptionMaxWidth + AScaleFactor.Apply(ColorBarWidth) + GetMaxValue(AWidthList);

    AViewInfo.ScaleFactor := AScaleFactor;
    AViewInfo.StyleCaptionBoxBounds := cxRectSetWidth(R, ACaptionMaxWidth);
    AViewInfo.StyleColorBoxBounds := R;
    AViewInfo.StyleColorBoxBounds.Left := AViewInfo.StyleCaptionBoxBounds.Right;
    AViewInfo.StyleColorBoxBounds := cxRectSetWidth(AViewInfo.StyleColorBoxBounds, AScaleFactor.Apply(ColorBarWidth));
    AViewInfo.StyleColorBoxContentBounds := cxRectInflate(AViewInfo.StyleColorBoxBounds, -ScaleFactor.Apply(2));

    AViewInfo.StyleFontInfoBoxBounds := R;
    AViewInfo.StyleFontInfoBoxBounds.Left := AViewInfo.StyleColorBoxBounds.Right;
    AViewInfo.StyleFontInfoBoxBounds.Right := AViewInfo.StyleFontInfoBoxBounds.Left + Integer(AWidthList[0]);

    AViewInfo.RestSpaceBounds := R;
    AViewInfo.RestSpaceBounds.Left := AViewInfo.StyleFontInfoBoxBounds.Right;
  finally
    AWidthList.Free;
  end;
end;

procedure TdxStylesListBox.CalculateStyleBarViewInfoViewParams(const ACaption: string;
  AStyle: TcxStyle; const R: TRect; ASelected: Boolean; var AViewInfo: TdxStyleBarViewInfo);
var
  AParams: TcxViewParams;
  AParamsScaleFactor: TdxScaleFactor;
begin
  AViewInfo.StyleCaption := ACaption;
  dxPSGetViewParams(AStyle, ACaption, ReportLinkStyles, AParams, AParamsScaleFactor);

  AViewInfo.BarColor := GetStyleBarItemColor(ASelected);
  AViewInfo.BarStyleColorBoxFrameColor := clBtnShadow;
  if ColorToRGB(AViewInfo.BarStyleColorBoxFrameColor) = ColorToRGB(AViewInfo.BarColor) then
    AViewInfo.BarStyleColorBoxFrameColor := clHighlightText;
  AViewInfo.BarFont.Assign(Font);
  AViewInfo.BarFont.Color := GetStyleBarItemTextColor(ASelected, AViewInfo.BarFont.Color, AViewInfo.BarColor);
  AViewInfo.StyleTextColor := GetStyleBarItemTextColor(ASelected, AParams.TextColor, AViewInfo.BarColor);

  AViewInfo.StyleBitmap.Assign(AParams.Bitmap);
  AViewInfo.StyleBitmapOrg := cxNullPoint;
  AViewInfo.StyleColor := AParams.Color;
  if Assigned(AParams.Font) then
  begin
    AViewInfo.StyleFont.Assign(AParams.Font);
    AViewInfo.StyleFontScaleFactor.Assign(AParamsScaleFactor);
  end;
end;

procedure TdxStylesListBox.CalculateStyleBarViewInfo(const ACaption: string;
  AnIndex: Integer; AStyle: TcxStyle; const R: TRect; ASelected: Boolean;
  AScaleFactor: TdxScaleFactor; var AViewInfo: TdxStyleBarViewInfo);
begin
  CalculateStyleBarViewInfoSizes(AnIndex, R, AScaleFactor, AViewInfo);
  CalculateStyleBarViewInfoViewParams(ACaption, AStyle, R, ASelected, AViewInfo);
end;

function TdxStylesListBox.GetStyleBarItemColor(ASelected: Boolean): TColor;
begin
  if ASelected then
    Result := StyleLookAndFeelPainter.DefaultSelectionColor
  else
    Result := StyleLookAndFeelPainter.DefaultEditorBackgroundColor(not Enabled);

  if Result = clDefault then
  begin
    if ASelected then
      Result := clHighlight
    else
      Result := ActiveStyle.Color;
  end;
end;

function TdxStylesListBox.GetStyleBarItemTextColor(ASelected: Boolean; AColor, ABkColor: TColor): TColor;
begin
  if ASelected then
    Result := LookAndFeelPainter.DefaultSelectionTextColor
  else
    Result := LookAndFeelPainter.DefaultEditorTextColor(not Enabled);

  if Result = clDefault then
  begin
    Result := AColor;
    if not Enabled then
      Result := clGrayText;
    if ASelected then
      Result := clHighlightText;
    if Result = ABkColor then
      Result := dxInvertColor(Result);
  end;
end;

function TdxStylesListBox.GetStyleFontByText(const AText: string; out AFont: TFont; out AScaleFactor: TdxScaleFactor): Boolean;
var
  AParams: TcxViewParams;
begin
  dxPSGetViewParams(ReportLinkStyles.StylesByCaption[AText], AText, ReportLinkStyles, AParams, AScaleFactor);
  AFont := AParams.Font;
  Result := AFont <> nil;
end;

function TdxStylesListBox.AreToolTipsNeeded: Boolean;
begin
  Result := IsHotTrackStyleViolateControlBounds and  GetParentForm(Self).Active and dxPSGlbl.CanShowHints;
end;

function TdxStylesListBox.FindHotTrackStyle(const Pt: TPoint): Integer;
begin
  Result := ItemAtPos(Pt, True);
end;

function TdxStylesListBox.IsHotTrackStyleViolateControlBounds: Boolean;
begin
  Result := (HotTrackStyleIndex <> -1) and
    ((HotTrackStyleBounds.Right - HotTrackStyleBounds.Left) > ClientWidth);
end;

function TdxStylesListBox.IsMouseOver: Boolean;
var
  APoint: TPoint;
begin
  APoint := GetMouseCursorPos;
  MapWindowPoints(0, Parent.Handle, APoint, 1);
  Result := PtInRect(BoundsRect, APoint);
end;

procedure TdxStylesListBox.ShowToolTips;
begin
  if FToolTipsWindow = nil then
    FToolTipsWindow := TdxStylesListBoxToolTipsWindow.Create(Self);
  ToolTipsWindow.Activate(ToolTipsBounds, HotTrackStyleIndex);
  FToolTipsLongHideTimer.Enabled := True;
  FToolTipsShortHideTimer.Enabled := True;
end;

procedure TdxStylesListBox.UpdateToolTips(const Pt: TPoint);
var
  NewHotTrackStyleIndex: Integer;
begin
  NewHotTrackStyleIndex := FindHotTrackStyle(Pt);
  if NewHotTrackStyleIndex <> HotTrackStyleIndex then
  begin
    HideToolTips;
    HotTrackStyleIndex := NewHotTrackStyleIndex;
    if AreToolTipsNeeded then
      ShowToolTips;
  end;
end;

function TdxStylesListBox.GetHotTrackStyleBounds: TRect;
var
  ViewInfo: TdxStyleBarViewInfo;
begin
  Result := ItemRect(HotTrackStyleIndex);
  MapWindowPoints(Handle, 0, Result, 2);

  InitializeStyleBarViewInfo(ViewInfo);
  try
    CalculateStyleBarViewInfoSizes(HotTrackStyleIndex, Result, ScaleFactor, ViewInfo);
    Result.Right := Result.Left + ViewInfo.MaxWidth;
  finally
    DeinitializeStyleBarViewInfo(ViewInfo);
  end;
end;

function TdxStylesListBox.GetHotTrackStyleName: string;
begin
  Result := Items[HotTrackStyleIndex];
end;

function TdxStylesListBox.GetSelectedStyle: TcxStyle;
begin
  if (ItemIndex < 0) or (ItemIndex >= Count) then
    Result := nil
  else
    Result := Styles[ItemIndex];
end;

function TdxStylesListBox.GetStyle(Index: Integer): TcxStyle;
begin
  Result := TcxStyle(Items.Objects[Index]);
end;

function TdxStylesListBox.GetStyleLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := ActiveStyle.LookAndFeel.Painter;
end;

function TdxStylesListBox.GetToolTipsBounds: TRect;
var
  SI: TScrollInfo;
begin
  Result := HotTrackStyleBounds;

  FillChar(SI, SizeOf(SI), 0);
  SI.cbSize := SizeOf(SI);
  SI.fMask := SIF_POS;
  GetScrollInfo(Handle, SB_HORZ, SI);

  OffsetRect(Result, -SI.nPos, 0);
  InflateRect(Result, 1, 1);
end;

procedure TdxStylesListBox.SetReportLinkStyles(Value: TdxCustomReportLinkStyles);
begin
  if FReportLinkStyles <> Value then
  begin
    FReportLinkStyles := Value;
    RecreateWnd;
  end;
end;

procedure TdxStylesListBox.SetToolTips(Value: Boolean);
var
  Pt: TPoint;
begin
  if FToolTips <> Value then
  begin
    FToolTips := Value;
    if ToolTips then
    begin
      Pt := GetMouseCursorPos;
      MapWindowPoints(0, Handle, Pt, 1);
      UpdateToolTips(Pt);
    end
    else
      HideToolTips;
  end;
end;

procedure TdxStylesListBox.ToolTipsLongHideTimerHandler(Sender: TObject);
begin
  HideToolTips;
end;

procedure TdxStylesListBox.ToolTipsShortHideTimerHandler(Sender: TObject);
begin
  if not IsMouseOver then
    HideToolTips;
end;

procedure TdxStylesListBox.WMHScroll(var Message: TWMHScroll);
begin
  inherited;
  HideToolTips;
end;

procedure TdxStylesListBox.WMMouseWheel(var Message: TWMMouseWheel);
begin
  inherited;
  {if message.WheelDelta > 0 then CheckUpScroll}
  HideToolTips;
end;

procedure TdxStylesListBox.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
  HideToolTips;
end;

procedure RegisterAssistants;
begin
  TdxPSCustomDataMap.Register;
  TdxPSTextDataMap.Register;
  TdxPSMemoDataMap.Register;
  TdxPSHyperLinkDataMap.Register;
  TdxPSCheckDataMap.Register;
  TdxPSRadioButtonGroupDataMap.Register;
  TdxPSGraphicDataMap.Register;
  TdxPSImageDataMap.Register;
  TdxPSBlobDataMap.Register;
  TdxPSToggleSwitchDataMap.Register;
  TdxSparklineDataMap.Register;
  TdxTokenEditDataMap.Register;
end;

procedure RegisterItems;
begin
  TdxReportCellPreviewText.Register;
  TdxReportCellSparklineImage.Register;
  TdxReportCellTokens.Register;
end;

procedure UnregisterAssistants;
begin
  TdxPSDataMaps.ReleaseInstance;
end;

procedure UnregisterItems;
begin
  TdxReportCellPreviewText.Unregister;
  TdxReportCellSparklineImage.Unregister;
  TdxReportCellTokens.Unregister;
end;

procedure FreeAndNilPreviewPictures;
var
  I: Integer;
begin
  for I := 0 to dxPSPreviewCarLogoCount - 1 do
    FreeAndNil(FPreviewPictures[I]);
end;

initialization
  RegisterAssistants;
  RegisterItems;

finalization
  UnregisterItems;
  UnregisterAssistants;

  FreeAndNilPreviewPictures;
  FreeAndNil(FPictureDialog);
  FreeAndNil(FPicture);
end.
