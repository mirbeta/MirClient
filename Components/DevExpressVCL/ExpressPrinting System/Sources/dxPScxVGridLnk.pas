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
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPScxVGridLnk;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, SysUtils, Classes, Graphics, Controls, StdCtrls, ExtCtrls, ComCtrls,
  Menus, ImgList, cxClasses, dxCore, cxControls, cxGraphics, cxLookAndFeels, cxStyles,
  cxCustomData, cxInplaceContainer, cxVGrid, cxDBVGrid, cxOI, cxEdit, cxTextEdit,
  cxMemo, cxImage, dxBase, dxPSSngltn, dxPSGlbl, dxPScxCommon, dxPSCore, dxPrnPg,
  dxExtCtrls, cxDrawTextUtils, cxPC, cxContainer, cxCheckBox, cxLabel, cxMaskEdit,
  cxDropDownEdit, cxLookAndFeelPainters, cxButtons, dxPSReportRenderCanvas,
  dxLayoutControlAdapters, dxLayoutLookAndFeels, dxLayoutContainer, dxLayoutControl,
  dxLayoutcxEditAdapters, cxImageList, dxSpreadSheetStyles, cxDataControllerConditionalFormatting;

const
  vspsVGridFirst = 0;
  vspsVGridContent = vspsVGridFirst + 0;
  vspsVGridHeader = vspsVGridFirst + 1;
  vspsVGridCategory = vspsVGridFirst + 2;
  vspsVGridLast = vspsVGridFirst + 2;

  cxVerticalGridBaseAttributeID = 0;
  cxVerticalGridUndefinedID = cxVerticalGridBaseAttributeID + 0;
  cxVerticalGridCategoryRowID = cxVerticalGridBaseAttributeID + 1;
  cxVerticalGridRowHeaderID = cxVerticalGridBaseAttributeID + 2;
  cxVerticalGridRowHeaderSeparatorID = cxVerticalGridBaseAttributeID + 3;
  cxVerticalGridRowValueID = cxVerticalGridBaseAttributeID + 4;
  cxVerticalGridRowValueSeparatorID = cxVerticalGridBaseAttributeID + 5;
  cxVerticalGridRowIndentID = cxVerticalGridBaseAttributeID + 6;

type
  TcxCustomVerticalGridReportLink = class;
  TcxfmVerticalGridReportLinkDesignWindow = class;
  TcxVerticalGridReportLinkOptionsView = class;

  TcxCustomVerticalGridAdapterClass = class of TcxCustomVerticalGridAdapter;
  TcxCustomVerticalGridAdapter = class;
  TcxCustomVerticalGridReportLinkBuilderClass = class of TcxCustomVerticalGridReportLinkBuilder;
  TcxCustomVerticalGridReportLinkBuilder = class;
  TcxCustomVerticalGridReportLinkFormatterClass = class of TcxCustomVerticalGridReportLinkFormatter;
  TcxCustomVerticalGridReportLinkFormatter = class;

  TcxVerticalGridCustomRowHelperClass = class of TcxVerticalGridCustomRowHelper;
  TcxVerticalGridCustomRowHelper = class;
  TcxVerticalGridCategoryRowHelper = class;
  TcxVerticalGridCustomEditorRowHelper = class;
  TcxVerticalGridCustomMultiEditorRowHelper = class;

  TcxVerticalGridHostInfo = class;

  TcxVerticalGridAttributeID = type Integer;

  TcxVerticalGridCellCustomDrawInfo = record
    AttributeID: TcxVerticalGridAttributeID;
    Row: TcxCustomRow;
    CellIndex: Integer;
    RecordIndex: Integer;
  end;

  { Row Places }

  TcxVerticalGridRowElementPlace = class
  public
    Offset: Integer;
    Width: Integer;
    procedure Clear;
  end;

  TcxVerticalGridCustomRowPlaceClass = class of TcxVerticalGridCustomRowPlace;

  TcxVerticalGridCustomRowPlace = class
  private
    FFormatter: TcxCustomVerticalGridReportLinkFormatter;
    FRow: TcxCustomRow;
    FWrapIndex: Integer;
    function GetHasHeader: Boolean;
    function GetIndentArea: Integer;
    function GetInterRecordsSpace: Integer;
    function GetItemCount: Integer;
    function GetRecordCount: Integer;
    function GetRecordWidth(RecordIndex: Integer): Integer;
    function GetScreenCanvas: TdxPSReportRenderCustomCanvas;
    function GetStartRecordIndex: Integer;
    function GetStopRecordIndex: Integer;
  protected
    procedure AddDelimiters(AReportLink: TcxCustomVerticalGridReportLink); virtual;

    procedure Calculate; virtual;
    function CalculatedHeaderWidth: Integer; virtual;
    function CalculatedMinHeight: Integer; virtual;
    function CalculatedMinWidth: Integer; virtual;
    function CalculatedRecordWidth(ARecordIndex: Integer): Integer; virtual;
    function CalculatedTextPatternHeight(AFont: TFont): Integer;
    function CalculatedTextWidth(const AText: string; AFont: TFont): Integer;
    procedure Clear; virtual;

    function GetHeaderAvailableWidth: Integer; virtual;
    function GetValueAvailableWidth(RecordIndex: Integer): Integer; virtual;

    function Row: TcxCustomRow; overload; virtual;
    function RowHelper: TcxVerticalGridCustomRowHelper; overload; virtual;

    property Formatter: TcxCustomVerticalGridReportLinkFormatter read FFormatter;
    property HasHeader: Boolean read GetHasHeader;
    property HeaderAvailableWidth: Integer read GetHeaderAvailableWidth;
    property InterRecordsSpace: Integer read GetInterRecordsSpace;
    property ItemCount: Integer read GetItemCount;
    property RecordCount: Integer read GetRecordCount;
    property RecordWidths[RecordIndex: Integer]: Integer read GetRecordWidth;
    property ScreenCanvas: TdxPSReportRenderCustomCanvas read GetScreenCanvas;
    property StartRecordIndex: Integer read GetStartRecordIndex;
    property StopRecordIndex: Integer read GetStopRecordIndex;
    property ValueAvailableWidths[RecordIndex: Integer]: Integer read GetValueAvailableWidth;
  public
    constructor Create(AFormatter: TcxCustomVerticalGridReportLinkFormatter; ARow: TcxCustomRow; AWrapIndex: Integer); virtual;

    property IndentArea: Integer read GetIndentArea;
    property WrapIndex: Integer read FWrapIndex;
  end;

  TcxVerticalGridCategoryRowPlace = class(TcxVerticalGridCustomRowPlace)
  private
    FPlace: TcxVerticalGridRowElementPlace;
    function GetCaption: string;
    function GetFont: TFont;
    function GetOffset: Integer;
    function GetWidth: Integer;
  protected
    procedure Calculate; override;
    function CalculatedHeaderWidth: Integer; override;
    function CalculatedMinHeight: Integer; override;
    function CalculatedMinWidth: Integer; override;

    function Row: TcxCategoryRow; reintroduce; overload;
    function RowHelper: TcxVerticalGridCategoryRowHelper; reintroduce; overload;

    property Caption: string read GetCaption;
    property Font: TFont read GetFont;
  public
    constructor Create(AFormatter: TcxCustomVerticalGridReportLinkFormatter; ARow: TcxCustomRow; AWrapIndex: Integer); override;
    destructor Destroy; override;

    property Offset: Integer read GetOffset;
    property Width: Integer read GetWidth;
  end;

  TcxVerticalGridCustomEditorRowPlace = class(TcxVerticalGridCustomRowPlace)
  private
    FHeaderPlace: TcxVerticalGridRowElementPlace;
    FValuePlaces: TList;
    function GetHeaderFont: TFont;
    function GetProperty(RecordIndex: Integer): TcxCustomEditProperties;
    function GetValueFont(RecordIndex: Integer): TFont;
    function GetValuePlace(RecordIndex: Integer): TcxVerticalGridRowElementPlace;
    function GetValuePlaceCount: Integer;
    function GetValue(RecordIndex: Integer): TcxEditValue;
    function GetValuesOffset: Integer;
  protected
    procedure AddDelimiters(AReportLink: TcxCustomVerticalGridReportLink); override;

    procedure Calculate; override;
    function CalculatedHeaderWidth: Integer; override;
    function CalculatedMinHeight: Integer; override;
    function CalculatedRecordWidth(ARecordIndex: Integer): Integer; override;
    procedure CalculateHeaderPlace; virtual;
    procedure CalculateValuePlaces; virtual;
    procedure Clear; override;

    procedure ClearValuePlaces;
    procedure CreateValuePlaces;
    procedure FreeAndNilValuePlaces;

    function DoesItemParticipateInBestFitCalculation(ARecordIndex: Integer): Boolean;
    function MeasureWidth(ARecordIndex: Integer): Integer;

    function Row: TcxCustomEditorRow; reintroduce; overload;
    function RowHelper: TcxVerticalGridCustomEditorRowHelper; reintroduce; overload;

    property HeaderFont: TFont read GetHeaderFont;
    property Properties[RecordIndex: Integer]: TcxCustomEditProperties read GetProperty;
    property ValueFonts[RecordIndex: Integer]: TFont read GetValueFont;
    property Values[RecordIndex: Integer]: TcxEditValue read GetValue;
    property ValuesOffset: Integer read GetValuesOffset;
  public
    constructor Create(AFormatter: TcxCustomVerticalGridReportLinkFormatter; ARow: TcxCustomRow; AWrapIndex: Integer); override;
    destructor Destroy; override;

    property HeaderPlace: TcxVerticalGridRowElementPlace read FHeaderPlace;
    property ValuePlaceCount: Integer read GetValuePlaceCount;
    property ValuePlaces[RecordIndex: Integer]: TcxVerticalGridRowElementPlace read GetValuePlace;
  end;

  TcxVerticalGridCustomMultiEditorRowPlace = class(TcxVerticalGridCustomRowPlace)
  private
    FHeaderPlaces: TList;
    FHeaderSeparatorPlaces: TList;
    FValuePlaces: TList;
    FValueSeparatorPlaces: TList;
    function GetHasHeaderSeparators: Boolean;
    function GetHasValueSeparators: Boolean;
    function GetHeaderFont(CellIndex: Integer): TFont;
    function GetHeaderPlace(CellIndex: Integer): TcxVerticalGridRowElementPlace;
    function GetHeaderPlaceCount: Integer;
    function GetHeaderSeparatorCount: Integer;
    function GetHeaderSeparatorPlace(CellIndex: Integer): TcxVerticalGridRowElementPlace;
    function GetHeaderSeparatorPlaceCount: Integer;
    function GetHeaderSeparatorsArea: Integer;
    function GetHeaderSeparatorsFont(CellIndex: Integer): TFont;
    function GetProperty(CellIndex, RecordIndex: Integer): TcxCustomEditProperties;
    function GetValue(CellIndex, RecordIndex: Integer): TcxEditValue;
    function GetValueFlatIndex(CellIndex, RecordIndex: Integer): Integer;
    function GetValueFont(CellIndex, RecordIndex: Integer): TFont;
    function GetValuePlace(CellIndex, RecordIndex: Integer): TcxVerticalGridRowElementPlace;
    function GetValuePlaceCount: Integer;
    function GetValueSeparatorCount: Integer;
    function GetValueSeparatorFlatIndex(CellIndex, RecordIndex: Integer): Integer;
    function GetValueSeparatorPlace(CellIndex, RecordIndex: Integer): TcxVerticalGridRowElementPlace;
    function GetValueSeparatorPlaceCount: Integer;
    function GetValueSeparatorsArea(RecordIndex: Integer): Integer;
    function GetValueSeparatorsFont(CellIndex, RecordIndex: Integer): TFont;
    function GetValuesOffset: Integer;
  protected
    procedure AddDelimiters(AReportLink: TcxCustomVerticalGridReportLink); override;

    procedure Calculate; override;
    function CalculatedHeaderWidth: Integer; override;
    function CalculatedMinHeight: Integer; override;
    function CalculatedRecordWidth(ARecordIndex: Integer): Integer; override;
    procedure CalculateHeaderPartWidths;
    procedure CalculateHeaderPlaces; virtual;
    procedure CalculateHeaderSeparatorPlaces; virtual;
    procedure CalculateHeaderSeparatorWidths; virtual;
    procedure CalculatePartWidths(AnAutoWidthObject: TcxAutoWidthObject; AnAvailableWidth: Integer);
    procedure CalculateValuePartWidths;
    procedure CalculateValuePlaces; virtual;
    procedure CalculateValueSeparatorPlaces; virtual;
    procedure CalculateValueSeparatorWidths; virtual;
    procedure Clear; override;

    function GetHeaderAvailableWidth: Integer; override;
    function GetValueAvailableWidth(RecordIndex: Integer): Integer; override;

    procedure ClearPlaces;
    procedure CreatePlaces;
    procedure FreeAndNilPlaces;

    function DoesItemParticipateInBestFitCalculation(ACellIndex, ARecordIndex: Integer): Boolean;
    function MeasureWidth(ACellIndex, ARecordIndex: Integer): Integer;

    function Row: TcxCustomMultiEditorRow; reintroduce; overload;
    function RowHelper: TcxVerticalGridCustomMultiEditorRowHelper; reintroduce; overload;

    property HasHeaderSeparators: Boolean read GetHasHeaderSeparators;
    property HasValueSeparators: Boolean read GetHasValueSeparators;
    property HeaderFonts[CellIndex: Integer]: TFont read GetHeaderFont;
    property HeaderSeparatorCount: Integer read GetHeaderSeparatorCount;
    property HeaderSeparatorsArea: Integer read GetHeaderSeparatorsArea;
    property HeaderSeparatorsFonts[Index: Integer]: TFont read GetHeaderSeparatorsFont;
    property Properties[CellIndex, RecordIndex: Integer]: TcxCustomEditProperties read GetProperty;
    property ValueFlatIndexes[CellIndex, RecordIndex: Integer]: Integer read GetValueFlatIndex;
    property ValueFonts[CellIndex, RecordIndex: Integer]: TFont read GetValueFont;
    property Values[CellIndex, RecordIndex: Integer]: TcxEditValue read GetValue;
    property ValueSeparatorCount: Integer read GetValueSeparatorCount;
    property ValueSeparatorFlatIndexes[CellIndex, RecordIndex: Integer]: Integer read GetValueSeparatorFlatIndex;
    property ValueSeparatorsAreas[RecordIndex: Integer]: Integer read GetValueSeparatorsArea;
    property ValueSeparatorsFonts[CellIndex, RecordIndex: Integer]: TFont read GetValueSeparatorsFont;
    property ValuesOffset: Integer read GetValuesOffset;
  public
    constructor Create(AFormatter: TcxCustomVerticalGridReportLinkFormatter; ARow: TcxCustomRow; AWrapIndex: Integer); override;
    destructor Destroy; override;

    property HeaderPlaceCount: Integer read GetHeaderPlaceCount;
    property HeaderPlaces[CellIndex: Integer]: TcxVerticalGridRowElementPlace read GetHeaderPlace;
    property HeaderSeparatorPlaceCount: Integer read GetHeaderSeparatorPlaceCount;
    property HeaderSeparatorPlaces[CellIndex: Integer]: TcxVerticalGridRowElementPlace read GetHeaderSeparatorPlace;
    property ValuePlaceCount: Integer read GetValuePlaceCount;
    property ValuePlaces[CellIndex, RecordIndex: Integer]: TcxVerticalGridRowElementPlace read GetValuePlace;
    property ValueSeparatorPlaceCount: Integer read GetValueSeparatorPlaceCount;
    property ValueSeparatorPlaces[CellIndex, RecordIndex: Integer]: TcxVerticalGridRowElementPlace read GetValueSeparatorPlace;
  end;

  { Row Producers }

  TcxVerticalGridReportLinkCustomElementProducerClass = class of TcxVerticalGridReportLinkCustomElementProducer;

  TcxVerticalGridReportLinkCustomElementProducer = class
  private
    FBuilder: TcxCustomVerticalGridReportLinkBuilder;
    FHost: TdxReportCell;
    FRow: TdxReportCell;
    FRowHeight: Integer;
    FWrapIndex: Integer;
    function GetScreenCanvas: TdxPSReportRenderCustomCanvas;
    function GetRowWidth: Integer;
  protected
    procedure CalculateRowHeight; virtual;
    procedure CreateRow; virtual;
    procedure CreateRowHost(AHostInfo: TcxVerticalGridHostInfo); virtual;
    procedure InitializeRow; virtual;

    property ScreenCanvas: TdxPSReportRenderCustomCanvas read GetScreenCanvas;
    property WrapIndex: Integer read FWrapIndex;
  public
    constructor Create(ABuilder: TcxCustomVerticalGridReportLinkBuilder); virtual;

    function Adapter: TcxCustomVerticalGridAdapter; overload; virtual;
    function Builder: TcxCustomVerticalGridReportLinkBuilder; overload; virtual;
    function Formatter: TcxCustomVerticalGridReportLinkFormatter; overload; virtual;

    function Produce(AHostInfo: TcxVerticalGridHostInfo; AWrapIndex: Integer): TdxReportCell; virtual;

    property Host: TdxReportCell read FHost;
    property Row: TdxReportCell read FRow;
    property RowHeight: Integer read FRowHeight write FRowHeight;
    property RowWidth: Integer read GetRowWidth;
  end;

  TcxVerticalGridReportLinkWrapSeparatorProducerClass = class of TcxVerticalGridReportLinkWrapSeparatorProducer;

  TcxVerticalGridReportLinkWrapSeparatorProducer = class(TcxVerticalGridReportLinkCustomElementProducer)
  private
    FSeparator: TdxReportCellString;
    function GetSeparatorBounds: TRect;
  protected
    procedure CreateRow; override;
    procedure CreateSeparator(AParent: TdxReportCell); virtual;
    procedure InitializeRow; override;
    procedure InitializeSeparator; virtual;
  public
    property Separator: TdxReportCellString read FSeparator;
    property SeparatorBounds: TRect read GetSeparatorBounds;
  end;

  TcxVerticalGridReportLinkCustomRowProducerClass = class of TcxVerticalGridReportLinkCustomRowProducer;

  TcxVerticalGridReportLinkCustomRowProducer = class(TcxVerticalGridReportLinkCustomElementProducer)
  private
    FGridRow: TcxCustomRow;
    FIndents: TList;
    function GetIndent(Index: Integer): TdxReportCellExpandButton;
    function GetIndentBounds(Index: Integer): TRect;
    function GetIndentCount: Integer;
    function GetIndentWidth: Integer;
    function GetItemCount: Integer;
    function GetRecordCount: Integer;
    function GetStartRecordIndex: Integer;
    function GetStopRecordIndex: Integer;
  protected
    procedure CalculateRowAutoHeight; virtual;
    procedure CalculateRowHeight; override;
    procedure ClearItems; virtual;
    procedure CreateRow; override;
    function CreateRowIndent(AParent: TdxReportCell): TdxReportCellExpandButton;
    procedure CreateRowIndents(AParent: TdxReportCell); virtual;
    function DoesItemParticipateInRowAutoHeightCalculation(AnItem: TAbstractdxReportCellData): Boolean; virtual;
    procedure FixupRowDataHeight; virtual;
    procedure FixupRowHeight; virtual;
    procedure FixupRowItselfHeight; virtual;
    procedure InitializeRow; override;
    procedure InitializeRowIndent(AnItem: TdxReportCellExpandButton; AnIndex: Integer); virtual;

    function GetAutoHeight: Boolean; virtual;
    function GetHasIndents: Boolean; virtual;
    function GetLineHeight: Integer; virtual;

    property ItemCount: Integer read GetItemCount;
    property RecordCount: Integer read GetRecordCount;
  public
    constructor Create(ABuilder: TcxCustomVerticalGridReportLinkBuilder); override;
    destructor Destroy; override;

    function GridRow: TcxCustomRow; overload; virtual;
    function GridRowHelper: TcxVerticalGridCustomRowHelper; overload; virtual;
    function GridRowPlace: TcxVerticalGridCustomRowPlace; overload; virtual;

    function Produce(AHostInfo: TcxVerticalGridHostInfo; AGridRow: TcxCustomRow;
      AWrapIndex: Integer): TdxReportCell; reintroduce; overload;

    property AutoHeight: Boolean read GetAutoHeight;
    property HasIndents: Boolean read GetHasIndents;
    property IndentBounds[Index: Integer]: TRect read GetIndentBounds;
    property IndentCount: Integer read GetIndentCount;
    property Indents[Index: Integer]: TdxReportCellExpandButton read GetIndent;
    property IndentWidth: Integer read GetIndentWidth;
    property LineHeight: Integer read GetLineHeight;
    property StartRecordIndex: Integer read GetStartRecordIndex;
    property StopRecordIndex: Integer read GetStopRecordIndex;
  end;

  TcxVerticalGridReportLinkCategoryRowProducer = class(TcxVerticalGridReportLinkCustomRowProducer)
  private
    function GetCategoryBounds: TRect;
  protected
    procedure CreateCategoryRow(AParent: TdxReportCell); virtual;
    procedure CreateRow; override;
  public
    function GridRow: TcxCategoryRow; reintroduce; overload;
    function GridRowHelper: TcxVerticalGridCategoryRowHelper; reintroduce; overload;
    function GridRowPlace: TcxVerticalGridCategoryRowPlace; reintroduce; overload;

    property CategoryBounds: TRect read GetCategoryBounds;
  end;

  TcxVerticalGridReportLinkCustomEditableRowProducer = class(TcxVerticalGridReportLinkCustomRowProducer)
  private
    FHeaders: TList;
    FValues: TList;
    function GetHeader(Index: Integer): TdxReportCellImage;
    function GetHeaderCount: Integer;
    function GetValue(Index: Integer): TAbstractdxReportCellData;
    function GetValueCount: Integer;
  protected
    procedure ClearItems; override;
    function CreateHeaderItem(AParent: TdxReportCell; ACellIndex: Integer): TdxReportCellImage;
    function CreateValueItem(AParent: TdxReportCell; ACellIndex, ARecordIndex: Integer): TAbstractdxReportCellData;
    procedure DoInitializeHeader(AnItem: TdxReportCellImage; AnIndex: Integer); virtual;
    procedure DoInitializeValue(AnItem: TAbstractdxReportCellData; ACellIndex, ARecordIndex: Integer); virtual;
    function DoesItemParticipateInRowAutoHeightCalculation(AnItem: TAbstractdxReportCellData): Boolean; override;

    property HeaderCount: Integer read GetHeaderCount;
    property Headers[Index: Integer]: TdxReportCellImage read GetHeader;
    property ValueCount: Integer read GetValueCount;
    property Values[Index: Integer]: TAbstractdxReportCellData read GetValue;
  public
    constructor Create(ABuilder: TcxCustomVerticalGridReportLinkBuilder); override;
    destructor Destroy; override;

    property RecordCount;
  end;

  TcxVerticalGridReportLinkCustomEditorRowProducer = class(TcxVerticalGridReportLinkCustomEditableRowProducer)
  private
    function GetHasHeader: Boolean;
    function GetHeader: TdxReportCellImage;
    function GetHeaderBounds: TRect;
    function GetValueBounds(Index: Integer): TRect;
  protected
    procedure CreateHeader(AParent: TdxReportCell); virtual;
    procedure CreateRow; override;
    procedure CreateValues(AParent: TdxReportCell); virtual;

    property HasHeader: Boolean read GetHasHeader;
    property HeaderBounds: TRect read GetHeaderBounds;
    property ValueBounds[Index: Integer]: TRect read GetValueBounds;
  public
    function GridRow: TcxCustomEditorRow; reintroduce; overload;
    function GridRowHelper: TcxVerticalGridCustomEditorRowHelper; reintroduce; overload;
    function GridRowPlace: TcxVerticalGridCustomEditorRowPlace; reintroduce; overload;

    property Header: TdxReportCellImage read GetHeader;
    property ValueCount;
    property Values;
  end;

  TcxVerticalGridReportLinkCustomMultiEditorRowProducer = class(TcxVerticalGridReportLinkCustomEditableRowProducer)
  private
    FHeaderSeparators: TList;
    FValueSeparators: TList;
    function GetHasHeaderSeparators: Boolean;
    function GetHasValueSeparators: Boolean;
    function GetHeaderBounds(Index: Integer): TRect;
    function GetHeaderSeparator(Index: Integer): TdxReportCellString;
    function GetHeaderSeparatorBounds(Index: Integer): TRect;
    function GetHeaderSeparatorCount: Integer;
    function GetHeaderSeparatorText(Index: Integer): string;
    function GetValue(CellIndex, RecordIndex: Integer): TAbstractdxReportCellData;
    function GetValueBounds(CellIndex, RecordIndex: Integer): TRect;
    function GetValueSeparator(Index: Integer): TdxReportCellString;
    function GetValueSeparatorBounds(Index, RecordIndex: Integer): TRect;
    function GetValueSeparatorCount: Integer;
    function GetValueSeparatorText(Index: Integer): string;
  protected
    procedure ClearItems; override;
    procedure CreateRow; override;

    procedure CreateHeaders(AParent: TdxReportCell); virtual;
    function CreateHeaderSeparator(AParent: TdxReportCell): TdxReportCellString;
    procedure CreateHeaderSeparators(AParent: TdxReportCell); virtual;
    procedure CreateValues(AParent: TdxReportCell); virtual;
    function CreateValueSeparator(AParent: TdxReportCell): TdxReportCellString;
    procedure CreateValueSeparators(AParent: TdxReportCell); virtual;
    procedure DoInitializeHeaderSeparator(AnItem: TdxReportCellString; AnIndex: Integer); virtual;
    procedure DoInitializeValueSeparator(AnItem: TdxReportCellString; AnIndex, ARecordIndex: Integer); virtual;

    property HasHeaderSeparators: Boolean read GetHasHeaderSeparators;
    property HasValueSeparators: Boolean read GetHasValueSeparators;
    property HeaderBounds[Index: Integer]: TRect read GetHeaderBounds;
    property HeaderSeparatorBounds[Index: Integer]: TRect read GetHeaderSeparatorBounds;
    property HeaderSeparatorTexts[Index: Integer]: string read GetHeaderSeparatorText;
    property ValueSeparatorBounds[Index, RecordIndex: Integer]: TRect read GetValueSeparatorBounds;
    property ValueBounds[CellIndex, RecordIndex: Integer]: TRect read GetValueBounds;
    property ValueSeparatorTexts[Index: Integer]: string read GetValueSeparatorText;
  public
    constructor Create(ABuilder: TcxCustomVerticalGridReportLinkBuilder); override;
    destructor Destroy; override;

    function GridRow: TcxCustomMultiEditorRow; reintroduce; overload;
    function GridRowHelper: TcxVerticalGridCustomMultiEditorRowHelper; reintroduce; overload;
    function GridRowPlace: TcxVerticalGridCustomMultiEditorRowPlace; reintroduce; overload;

    property HeaderCount;
    property Headers;
    property HeaderSeparatorCount: Integer read GetHeaderSeparatorCount;
    property HeaderSeparators[Index: Integer]: TdxReportCellString read GetHeaderSeparator;
    property ValueCount;
    property Values[CellIndex, RecordIndex: Integer]: TAbstractdxReportCellData read GetValue;
    property ValueSeparatorCount: Integer read GetValueSeparatorCount;
    property ValueSeparators[Index: Integer]: TdxReportCellString read GetValueSeparator;
  end;

  { Row Helpers }

  TcxVerticalGridCustomRowHelper = class(TdxCustomClassMapItem)
  private
    FAdapter: TcxCustomVerticalGridAdapter;
    FRow: TcxCustomRow;
  protected
    function Adapter: TcxCustomVerticalGridAdapter; overload; virtual;

    function GetDisplayText(Index, RecordIndex: Integer): string; virtual;
    function GetEditProperties(Index, RecordIndex: Integer): TcxCustomEditProperties; virtual;
    function GetHasHeaderImage(Index: Integer): Boolean; virtual;
    function GetHasHeaderSeparators: Boolean; virtual;
    function GetHasValueSeparators: Boolean; virtual;
    function GetHeaderAlignmentHorz(Index: Integer): TcxTextAlignX; virtual;
    function GetHeaderAlignmentVert(Index: Integer): TcxTextAlignY; virtual;
    function GetHeaderCaption(Index: Integer): string; virtual;
    function GetImageIndex(Index: Integer): Integer; virtual;
    function GetIndentCount: Integer; virtual;
    function GetItemCount: Integer; virtual;
    function GetRecordMinWidth: Integer; virtual;
    function GetRowProperties(Index: Integer): TcxCustomRowProperties; virtual;
    function GetSeparatorsAlignmentVert: TcxAlignmentVert; virtual;
    function GetValue(Index, RecordIndex: Integer): TcxEditValue; virtual;

    property HasHeaderSeparators: Boolean read GetHasHeaderSeparators;
    property HasValueSeparators: Boolean read GetHasValueSeparators;
    property ItemCount: Integer read GetItemCount;
    property RecordMinWidth: Integer read GetRecordMinWidth;
    property SeparatorsAlignmentVert: TcxAlignmentVert read GetSeparatorsAlignmentVert;
  public
    constructor Create(AnAdapter: TcxCustomVerticalGridAdapter); virtual;
    procedure Initialize(ARow: TcxCustomRow); virtual;

    class function PairClass: TClass; override;
    class procedure Register;
    class procedure Unregister;

    class function ProducerClass: TcxVerticalGridReportLinkCustomRowProducerClass; virtual;
    function Properties(Index: Integer): TcxCustomRowProperties; overload; virtual;
    function Row: TcxCustomRow; overload; virtual;
    class function RowClass: TcxCustomRowClass; virtual;
    class function RowPlaceClass: TcxVerticalGridCustomRowPlaceClass; virtual;

    property DisplayTexts[Index, RecordIndex: Integer]: string read GetDisplayText;
    property EditProperties[Index, RecordIndex: Integer]: TcxCustomEditProperties read GetEditProperties;
    property HasHeaderImages[Index: Integer]: Boolean read GetHasHeaderImage;
    property HeaderAlignmentHorzs[Index: Integer]: TcxTextAlignX read GetHeaderAlignmentHorz;
    property HeaderAlignmentVerts[Index: Integer]: TcxTextAlignY read GetHeaderAlignmentVert;
    property HeaderCaptions[Index: Integer]: string read GetHeaderCaption;
    property ImageIndexes[Index: Integer]: Integer read GetImageIndex;
    property IndentCount: Integer read GetIndentCount;
    property Values[Index, RecordIndex: Integer]: TcxEditValue read GetValue;
  end;

  TcxVerticalGridCategoryRowHelper = class(TcxVerticalGridCustomRowHelper)
  protected
    function GetCaption: string;
    function GetHeaderAlignmentHorz(Index: Integer): TcxTextAlignX; override;
    function GetHeaderAlignmentVert(Index: Integer): TcxTextAlignY; override;
    function GetHeaderCaption(Index: Integer): string; override;
    function GetImageIndex(Index: Integer): Integer; override;
  public
    class function ProducerClass: TcxVerticalGridReportLinkCustomRowProducerClass; override;
    function Properties(Index: Integer): TcxCaptionRowProperties; reintroduce; overload;
    function Row: TcxCategoryRow; reintroduce; overload;
    class function RowClass: TcxCustomRowClass; override;
    class function RowPlaceClass: TcxVerticalGridCustomRowPlaceClass; override;

    property Caption: string read GetCaption;
  end;

  TcxVerticalGridCustomEditorRowHelper = class(TcxVerticalGridCustomRowHelper)
  protected
    function GetDisplayText(Index, RecordIndex: Integer): string; override;
    function GetEditProperties(Index, RecordIndex: Integer): TcxCustomEditProperties; override;
    function GetHeaderAlignmentHorz(Index: Integer): TcxTextAlignX; override;
    function GetHeaderAlignmentVert(Index: Integer): TcxTextAlignY; override;
    function GetHeaderCaption(Index: Integer): string; override;
    function GetImageIndex(Index: Integer): Integer; override;
    function GetRecordMinWidth: Integer; override;
    function GetValue(Index, RecordIndex: Integer): TcxEditValue; override;
  public
    class function ProducerClass: TcxVerticalGridReportLinkCustomRowProducerClass; override;
    function Properties(Index: Integer): TcxCustomEditorRowProperties; reintroduce; overload;
    function Row: TcxCustomEditorRow; reintroduce; overload;
    class function RowClass: TcxCustomRowClass; override;
    class function RowPlaceClass: TcxVerticalGridCustomRowPlaceClass; override;

    property RecordMinWidth;
  end;

  TcxVerticalGridEditorRowHelper = class(TcxVerticalGridCustomEditorRowHelper)
  public
    function Properties(Index: Integer): TcxEditorRowProperties; reintroduce; overload;
    function Row: TcxEditorRow; reintroduce; overload;
    class function RowClass: TcxCustomRowClass; override;
  end;

  TcxVerticalGridDBEditorRowHelper = class(TcxVerticalGridCustomEditorRowHelper)
  public
    function Properties(Index: Integer): TcxDBEditorRowProperties; reintroduce; overload;
    function Row: TcxDBEditorRow; reintroduce; overload;
    class function RowClass: TcxCustomRowClass; override;
  end;

  TcxVerticalGridCustomMultiEditorRowHelper = class(TcxVerticalGridCustomRowHelper)
  private
    function GetHeaderSeparatorCount: Integer;
    function GetHeaderSeparatorText(Index: Integer): string;
    function GetValueSeparatorCount: Integer;
    function GetValueSeparatorText(Index: Integer): string;
  protected
    function GetDisplayText(Index, RecordIndex: Integer): string; override;
    function GetEditProperties(Index, RecordIndex: Integer): TcxCustomEditProperties; override;
    function GetHasHeaderSeparators: Boolean; override;
    function GetHasValueSeparators: Boolean; override;
    function GetHeaderAlignmentHorz(Index: Integer): TcxTextAlignX; override;
    function GetHeaderAlignmentVert(Index: Integer): TcxTextAlignY; override;
    function GetHeaderCaption(Index: Integer): string; override;
    function GetImageIndex(Index: Integer): Integer; override;
    function GetItemCount: Integer; override;
    function GetRecordMinWidth: Integer; override;
    function GetRowProperties(Index: Integer): TcxCustomRowProperties; override;
    function GetSeparatorsAlignmentVert: TcxAlignmentVert; override;
    function GetValue(Index, RecordIndex: Integer): TcxEditValue; override;
  public
    class function ProducerClass: TcxVerticalGridReportLinkCustomRowProducerClass; override;
    function Properties(Index: Integer): TcxEditorRowItemProperties; reintroduce; overload;
    function RootProperties: TcxMultiEditorRowProperties; overload; virtual;
    function Row: TcxCustomMultiEditorRow; reintroduce; overload;
    class function RowClass: TcxCustomRowClass; override;
    class function RowPlaceClass: TcxVerticalGridCustomRowPlaceClass; override;

    property HasHeaderSeparators;
    property HasValueSeparators;
    property HeaderSeparatorCount: Integer read GetHeaderSeparatorCount;
    property HeaderSeparatorTexts[Index: Integer]: string read GetHeaderSeparatorText;
    property ItemCount;
    property RecordMinWidth;
    property SeparatorsAlignmentVert;
    property ValueSeparatorCount: Integer read GetValueSeparatorCount;
    property ValueSeparatorTexts[Index: Integer]: string read GetValueSeparatorText;
  end;

  { Caches }

  TcxVerticalGridRowHelperCache = class(TdxCustomCache)
  private
    FAdapter: TcxCustomVerticalGridAdapter;
    function GetHelper(Row: TcxCustomRow): TcxVerticalGridCustomRowHelper;
    function GetItem(Index: Integer): TcxVerticalGridCustomRowHelper;
  protected
    function IndexOf(Row: TcxCustomRow): Integer;
    property Items[Index: Integer]: TcxVerticalGridCustomRowHelper read GetItem;
  public
    constructor Create(AnAdapter: TcxCustomVerticalGridAdapter);

    property Adapter: TcxCustomVerticalGridAdapter read FAdapter;
    property Helpers[Row: TcxCustomRow]: TcxVerticalGridCustomRowHelper read GetHelper; default;
  end;

  TcxVerticalGridReportLinkProducerCache = class(TdxCustomCache)
  private
    FBuilder: TcxCustomVerticalGridReportLinkBuilder;
    function GetItem(Index: Integer): TcxVerticalGridReportLinkCustomRowProducer;
    function GetProducer(ProducerClass: TcxVerticalGridReportLinkCustomRowProducerClass): TcxVerticalGridReportLinkCustomRowProducer;
  protected
    function IndexOf(AProducerClass: TcxVerticalGridReportLinkCustomRowProducerClass): Integer;
    property Items[Index: Integer]: TcxVerticalGridReportLinkCustomRowProducer read GetItem;
  public
    constructor Create(ABuilder: TcxCustomVerticalGridReportLinkBuilder);
    property Builder: TcxCustomVerticalGridReportLinkBuilder read FBuilder;
    property Producers[ProducerClass: TcxVerticalGridReportLinkCustomRowProducerClass]: TcxVerticalGridReportLinkCustomRowProducer read GetProducer; default;
  end;

  TcxCustomVerticalGridReportLinkBuilder = class
  private
    FAdapter: TcxCustomVerticalGridAdapter;
    FFormatter: TcxCustomVerticalGridReportLinkFormatter;
    FProducerCache: TcxVerticalGridReportLinkProducerCache;
    FReportLink: TcxCustomVerticalGridReportLink;
    FReportRows: TList;
    FWrapSeparatorProducer: TcxVerticalGridReportLinkWrapSeparatorProducer;
    function GetAvailableSiteWidth: Integer;
    function GetAvailableWidth: Integer;
    function GetHost: TdxReportCell;
    function GetReportCells: TdxReportCells;
    function GetReportRow(Index: Integer): TdxReportCell;
    function GetReportRowCount: Integer;
    function GetVerticalGrid: TcxCustomVerticalGrid;
  protected
    procedure AddReportRow(ARow: TdxReportCell);
    procedure AfterBuilding; virtual;
    procedure BeforeBuilding; virtual;

    procedure CreateRow(ARow: TcxCustomRow; AWrapIndex: Integer); virtual;
    procedure CreateRows; virtual;
    procedure CreateWrapSeparator(AWrapIndex: Integer); virtual;
    function GetRowProducer(ARow: TcxCustomRow): TcxVerticalGridReportLinkCustomRowProducer;
    function GetRowProducerClass(ARow: TcxCustomRow): TcxVerticalGridReportLinkCustomRowProducerClass; virtual;
    function GetWrapSeparatorProducer(AWrapIndex: Integer): TcxVerticalGridReportLinkWrapSeparatorProducer;
    function GetWrapSeparatorProducerClass: TcxVerticalGridReportLinkWrapSeparatorProducerClass; virtual;

    function AreWrapSeparatorsNeeded: Boolean;
    procedure DoBuild; virtual;
    function IsAborted: Boolean;

    property ProducerCache: TcxVerticalGridReportLinkProducerCache read FProducerCache;
    property ReportLink: TcxCustomVerticalGridReportLink read FReportLink;
  public
    constructor Create(AReportLink: TcxCustomVerticalGridReportLink); virtual;
    destructor Destroy; override;

    procedure Build; virtual;
    procedure Progress(const APercentDone: Double);

    function Adapter: TcxCustomVerticalGridAdapter; overload; virtual;
    class function AdapterClass: TcxCustomVerticalGridAdapterClass; virtual;
    function Formatter: TcxCustomVerticalGridReportLinkFormatter; overload; virtual;
    class function FormatterClass: TcxCustomVerticalGridReportLinkFormatterClass; virtual;

    property AvailableSiteWidth: Integer read GetAvailableSiteWidth;
    property AvailableWidth: Integer read GetAvailableWidth;
    property Host: TdxReportCell read GetHost;
    property ReportCells: TdxReportCells read GetReportCells;
    property ReportRowCount: Integer read GetReportRowCount;
    property ReportRows[Index: Integer]: TdxReportCell read GetReportRow;
    property VerticalGrid: TcxCustomVerticalGrid read GetVerticalGrid;
  end;

  TcxVerticalGridReportLinkBuilderHandler = class
  private
    FBuilder: TcxCustomVerticalGridReportLinkBuilder;
    function GetReportLink: TcxCustomVerticalGridReportLink;
  public
    constructor Create(ABuilder: TcxCustomVerticalGridReportLinkBuilder); virtual;
    function Builder: TcxCustomVerticalGridReportLinkBuilder; overload; virtual;

    property ReportLink: TcxCustomVerticalGridReportLink read GetReportLink;
  end;

  TcxCustomVerticalGridAdapter = class(TcxVerticalGridReportLinkBuilderHandler)
  strict private
    FConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider;
    FHelperCache: TcxVerticalGridRowHelperCache;

    function GetCellAutoHeight: Boolean;
    function GetCellEndEllipsis: Boolean;
    function GetCellMultiline: Boolean;
    function GetDataController: TcxCustomDataController;
    function GetDefaultRowHeight: Integer;
    function GetFirstRecordIndex: Integer;
    function GetGridLinesColor: TColor;
    function GetHeaderAvailableWidth(Row: TcxCustomRow): Integer;
    function GetHeaderMinWidth: Integer;
    function GetHeaderWidth: Integer;
    function GetHelper(Row: TcxCustomRow): TcxVerticalGridCustomRowHelper;
    function GetImages: TCustomImageList;
    function GetIndentArea(Row: TcxCustomRow): Integer;
    function GetIndentCount(Row: TcxCustomRow): Integer;
    function GetIndentWidth: Integer;
    function GetMakeSpaceForEmptyImage: Boolean;
    function GetOptionsView: TcxvgOptionsView;
    function GetPaintStyle: TcxvgPaintStyle;
    function GetRecordCount: Integer;
    function GetRecordMinWidth(RecordIndex: Integer): Integer;
    function GetRecordWidth(RecordCount: Integer): Integer;
    function GetRow(Index: Integer): TcxCustomRow;
    function GetRowCount: Integer;
    function GetRowHeight(Row: TcxCustomRow): Integer;
    function GetShowHeaders: Boolean;
    function GetShowHorzGridLines: Boolean;
    function GetShowVertGridLines: Boolean;
    function GetStyles: TcxVerticalGridStyles;
    function GetValueMinWidth: Integer;
    function GetVerticalGrid: TcxCustomVerticalGrid;
  protected
    procedure AfterBuilding; virtual;
    procedure BeforeBuilding; virtual;

    function GetInterRecordsSpace: Integer; virtual;
    function GetLayoutStyle: TcxvgLayoutStyle; virtual; abstract;
    { properties }
    function GetProperties(ARow: TcxCustomRow; AnIndex, ARecordIndex: Integer): TcxCustomEditProperties;
    function GetPropertiesClass(ARow: TcxCustomRow; AnIndex, ARecordIndex: Integer): TcxCustomEditPropertiesClass;
    { styles }
    function GetCategoryViewParams(ARow: TcxCustomRow): TcxViewParams; virtual;
    function GetContentViewParams(ARow: TcxCustomRow; AnIndex, ARecordIndex: Integer): TcxViewParams; virtual;
    function GetHeaderViewParams(ARow: TcxCustomRow): TcxViewParams; virtual;
    function GetImageViewParams(ARow: TcxCustomRow): TcxViewParams; virtual;
    function GetIndentViewParams(ARow: TcxCustomRow): TcxViewParams; virtual;
    function TryGetAdvancedStyle(ARow: TcxCustomRow; ACellIndex, ARecordIndex: Integer; out AStyle: TdxSpreadSheetCellDisplayStyle): Boolean; virtual;
    { backgrounds }
    function GetBackgroundBitmap(Index: Integer): TBitmap;
    function GetRowIndentBackgroundBitmapIndex(ARow: TcxCustomRow; Alevel: Integer): Integer;
    function HasBackgroundBitmap(Index: Integer): Boolean;

    procedure FullExpand;
    function GetImageHeight(ARow: TcxCustomRow; AnIndex: Integer): Integer;
    function GetImageWidth(ARow: TcxCustomRow; AnIndex: Integer): Integer;
    function GetRowParentAtLevel(ARow: TcxCustomRow; ALevel: Integer): TcxCustomRow;
    function HasCategoryRowAsParentAtLevel(ARow: TcxCustomRow; ALevel: Integer; ACheckFollow: Boolean): Boolean;
    function HasRowImage(ARow: TcxCustomRow; AnIndex: Integer): Boolean;
    function IsRowLastChildAtLevel(ARow: TcxCustomRow; ALevel: Integer): Boolean;
    function IsRowVisible(ARow: TcxCustomRow): Boolean;

    property Helpers[Row: TcxCustomRow]: TcxVerticalGridCustomRowHelper read GetHelper;
    property OptionsView: TcxvgOptionsView read GetOptionsView;
    property Styles: TcxVerticalGridStyles read GetStyles;
    property VerticalGrid: TcxCustomVerticalGrid read GetVerticalGrid;
  public
    constructor Create(ABuilder: TcxCustomVerticalGridReportLinkBuilder); override;
    destructor Destroy; override;

    property DataController: TcxCustomDataController read GetDataController;
    property CellAutoHeight: Boolean read GetCellAutoHeight;
    property CellEndEllipsis: Boolean read GetCellEndEllipsis;
    property CellMultiline: Boolean read GetCellMultiline;
    property ConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider read FConditionalFormattingProvider;
    property DefaultRowHeight: Integer read GetDefaultRowHeight;
    property FirstRecordIndex: Integer read GetFirstRecordIndex;
    property GridLinesColor: TColor read GetGridLinesColor;
    property HeaderAvailableWidths[Row: TcxCustomRow]: Integer read GetHeaderAvailableWidth;
    property HeaderMinWidth: Integer read GetHeaderMinWidth;
    property HeaderWidth: Integer read GetHeaderWidth;
    property Images: TCustomImageList read GetImages;
    property IndentAreas[Row: TcxCustomRow]: Integer read GetIndentArea;
    property IndentCounts[Row: TcxCustomRow]: Integer read GetIndentCount;
    property IndentWidth: Integer read GetIndentWidth;
    property InterRecordsSpace: Integer read GetInterRecordsSpace;
    property LayoutStyle: TcxvgLayoutStyle read GetLayoutStyle;
    property MakeSpaceForEmptyImage: Boolean read GetMakeSpaceForEmptyImage;
    property PaintStyle: TcxvgPaintStyle read GetPaintStyle;
    property RecordCount: Integer read GetRecordCount;
    property RecordMinWidths[RecordIndex: Integer]: Integer read GetRecordMinWidth;
    property RecordWidth[RecordCount: Integer]: Integer read GetRecordWidth;
    property RowCount: Integer read GetRowCount;
    property RowHeights[Row: TcxCustomRow]: Integer read GetRowHeight;
    property Rows[Index: Integer]: TcxCustomRow read GetRow;
    property ShowHeaders: Boolean read GetShowHeaders;
    property ShowHorzGridLines: Boolean read GetShowHorzGridLines;
    property ShowVertGridLines: Boolean read GetShowVertGridLines;
    property ValueMinWidth: Integer read GetValueMinWidth;
  end;

  TcxCustomVerticalGridReportLinkFormatter = class(TcxVerticalGridReportLinkBuilderHandler,
    IUnknown, IdxPSCellParams, IdxPSCellParams2)
  private
    FFont: TFont;
    FHeaderWidth: Integer;
    FInterRecordsSpace: Integer;
    FIsPrevGridMode: Boolean;
    FLookAndFeelItems: TList;
    FRecordWidths: TList;
    FRowHeights: TList;
    FRowPlaces: TList;
    FRows: TList;
    FTransparentColor: TColor;
    FViewWidths: TList;
    FWrappedRecordStartIndexes: TList;
    function GetAutoWidth: Boolean;
    function GetDataController: TcxCustomDataController;
    function GetExpandButtonColor(Row: TcxCustomRow): TColor;
    function GetExpandButtonSize: Integer;
    function GetGridLinesColor: TColor;
    function GetHeaderAvailableWidth(Row: TcxCustomRow): Integer;
    function GetHeaderMinWidth: Integer;
    function GetHeaderWidth: Integer;
    function GetIndentArea(Row: TcxCustomRow): Integer;
    function GetInternalIndexByRecordIndex(RecordIndex: Integer): Integer;
    function GetInternalRecordWidth: Integer;
    function GetInterRecordsSpace: Integer;
    function GetLookAndFeelItem(Index: Integer): TdxReportVisualItem;
    function GetLookAndFeelItemCount: Integer;
    function GetOptionsView: TcxVerticalGridReportLinkOptionsView;
    function GetRecordCount(Index: Integer): Integer;
    function GetRecordMinWidth(RecordIndex: Integer): Integer;
    function GetRecordWidth(RecordIndex: Integer): Integer;
    function GetRenderer: TdxPSReportRenderer;
    function GetRow(Index: Integer): TcxCustomRow;
    function GetRowCount: Integer;
    function GetRowHeight(Row: TcxCustomRow): Integer;
    function GetRowHelper(Row: TcxCustomRow): TcxVerticalGridCustomRowHelper;
    function GetRowPlace(WrapIndex, Index: Integer): TcxVerticalGridCustomRowPlace;
    function GetRowPlaceByRow(Row: TcxCustomRow; WrapIndex: Integer): TcxVerticalGridCustomRowPlace;
    function GetRowPlaceCount: Integer;
    function GetRowPlaceFlatIndex(WrapIndex, Index: Integer): Integer;
    function GetScreenCanvas: TdxPSReportRenderCustomCanvas;
    function GetShowBorders: Boolean;
    function GetShowHeaders: Boolean;
    function GetShowHorzGridLines: Boolean;
    function GetShowVertGridLines: Boolean;
    function GetViewMaxWidth: Integer;
    function GetViewWidth(WrapIndex: Integer): Integer;
    function GetWrapCount: Integer;
    function GetWrappedRecordStartIndex(Index: Integer): Integer;
    function GetWrappedRecordStopIndex(Index: Integer): Integer;
    function GetWrapSeparatorHeight: Integer;
    procedure SetInterRecordsSpace(Value: Integer);
    procedure SetRecordWidth(RecordIndex: Integer; Value: Integer);
    procedure SetRowHeight(Row: TcxCustomRow; Value: Integer);
    procedure SetViewWidth(WrapIndex: Integer; Value: Integer);

    procedure FormatLookAndFeelItems;
  protected
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IdxPSCellParams }
    function IdxPSCellParams.GetAutoHeight = IdxPSCellParams_GetAutoHeight;
    function IdxPSCellParams.GetCanvas = IdxPSCellParams_GetCanvas;
    function IdxPSCellParams.GetDisplayGraphicsAsText = IdxPSCellParams_GetDisplayGraphicsAsText;
    function IdxPSCellParams.GetDisplayTrackBarsAsText = IdxPSCellParams_GetDisplayTrackBarsAsText;
    function IdxPSCellParams.GetEndEllipsis = IdxPSCellParams_GetEndEllipsis;
    function IdxPSCellParams.GetFlatCheckMarks = IdxPSCellParams_GetFlatCheckMarks;
    function IdxPSCellParams.GetGraphicsText = IdxPSCellParams_GetGraphicsText;
    function IdxPSCellParams.GetMultiline = IdxPSCellParams_GetMultiline;
    function IdxPSCellParams.GetTransparentGraphics = IdxPSCellParams_GetTransparentGraphics;
    function IdxPSCellParams_GetAutoHeight: Boolean; virtual;
    function IdxPSCellParams_GetCanvas: TdxPSReportRenderCustomCanvas; virtual;
    function IdxPSCellParams_GetDisplayGraphicsAsText: Boolean; virtual;
    function IdxPSCellParams_GetDisplayTrackBarsAsText: Boolean; virtual;
    function IdxPSCellParams_GetEndEllipsis: Boolean; virtual;
    function IdxPSCellParams_GetFlatCheckMarks: Boolean; virtual;
    function IdxPSCellParams_GetGraphicsText: string; virtual;
    function IdxPSCellParams_GetMultiline: Boolean; virtual;
    function IdxPSCellParams_GetTransparentGraphics: Boolean; virtual;
    { IdxPSCellParams2 }
    function IdxPSCellParams2.GetPreviewMarginLeft = IdxPSCellParams2_GetPreviewMarginLeft;
    function IdxPSCellParams2.GetPreviewMarginRight = IdxPSCellParams2_GetPreviewMarginRight;
    function IdxPSCellParams2.GetPreviewMaxHeight = IdxPSCellParams2_GetPreviewMaxHeight;
    function IdxPSCellParams2.GetPreviewMaxLineCount = IdxPSCellParams2_GetPreviewMaxLineCount;
    function IdxPSCellParams2.GetRichEditGraphicClass = IdxPSCellParams2_GetRichEditGraphicClass;
    function IdxPSCellParams2.GetRichEditTransparent = IdxPSCellParams2_GetRichEditTransparent;
    function IdxPSCellParams2_GetPreviewMarginLeft: Integer; virtual;
    function IdxPSCellParams2_GetPreviewMarginRight: Integer; virtual;
    function IdxPSCellParams2_GetPreviewMaxHeight: Integer; virtual;
    function IdxPSCellParams2_GetPreviewMaxLineCount: Integer; virtual;
    function IdxPSCellParams2_GetRichEditGraphicClass: TGraphicClass; virtual;
    function IdxPSCellParams2_GetRichEditTransparent: Boolean; virtual;

    procedure AddDelimiters; virtual;
    procedure AddHorizontalDelimiters; virtual;
    procedure AddVerticalDelimiters; virtual;
    procedure AfterBuilding; virtual;
    procedure BeforeBuilding; virtual;

    function GetAreRecordsNeededLoading: Boolean; virtual;
    function GetAreRecordsNeededUnloading: Boolean; virtual;
    function GetFirstInternalRecordIndex: Integer; virtual;
    function GetInternalRecordCount: Integer; virtual;
    procedure LoadRecords; virtual;
    procedure UnloadRecords; virtual;

    procedure AddWrappedRecordStartIndex(Value: Integer);
    procedure Calculate; virtual;
    procedure CalculateBestFit; virtual;
    procedure CalculateHeight(const AParams: TdxReportItemViewParams; var AHeight: Integer);
    function CalculatedMinWidth: Integer; virtual;
    procedure CalculateRecordAutoWidths;
    procedure CalculateRecordCounts;
    procedure CalculateRowHeights;
    procedure CalculateRowPlaces;
    procedure CalculateViewWidths;
    procedure CreateRowList; virtual;
    procedure CreateRowPlaces;
    function CreateRowPlace(ARow: TcxCustomRow; AWrapIndex: Integer): TcxVerticalGridCustomRowPlace; virtual;
    procedure InitializeRecordWidths; virtual;
    procedure InitializeRowHeights; virtual;
    procedure InitializeWrappedRecordIndexes; virtual;

    function GetBackgroundBitmap(Index: Integer): TBitmap;
    function GetBackgroundBitmapIndex(Index: Integer): Integer;
    function HasBackgroundBitmap(Index: Integer): Boolean;
    function MapStyleBackgroundBitmapIndex(AVerticalGridBackgroundBitmapIndex: Integer): Integer;

    function GetCategoryRowFont(ARow: TcxCustomRow): TFont;
    function GetRowHeaderFont(ARow: TcxCustomRow; AnIndex: Integer): TFont;
    function GetRowHeaderSeparatorsFont(ARow: TcxCustomRow; AnIndex: Integer): TFont;
    function GetRowValueFont(ARow: TcxCustomRow; AnIndex, ARecordIndex: Integer): TFont;
    function GetRowValueSeparatorsFont(ARow: TcxCustomRow; AnIndex, ARecordIndex: Integer): TFont;

    procedure ClearRowPlaces;
    function FixupHorzCellSides(ARow: TcxCustomRow; ACellSides: TdxCellSides): TdxCellSides;
    procedure FreeAndNilRowPlaces;
    function GetImageHeight(ARow: TcxCustomRow; ACellIndex: Integer): Integer;
    function GetImageWidth(ARow: TcxCustomRow; ACellIndex: Integer): Integer;
    function GetNextRow(ARow: TcxCustomRow): TcxCustomRow;
    function GetPrevRow(ARow: TcxCustomRow): TcxCustomRow;
    function GetRowByReportRow(ARow: TdxReportCell): TcxCustomRow;
    function GetStyleFontIndex(const AParams: TdxReportItemViewParams): Integer;
    function IndexOfRow(ARow: TcxCustomRow): Integer;
    function IsColorTransparent(AColor: TColor): Boolean;
    function IsFirstRow(ARow: TcxCustomRow): Boolean;
    function IsLastRow(ARow: TcxCustomRow): Boolean;
    function MakeCustomDrawCodeData(AnAttribute: TcxVerticalGridAttributeID; ACellIndex, ARecordIndex: Integer): Integer;
    procedure RegisterLookAndFeelItem(AnItem: TdxReportVisualItem; AEdgeStyle: TdxCellEdgeStyle);
    procedure SetViewParams(AnItem: TdxReportVisualItem; const AParams: TdxReportItemViewParams);

    property AreRecordsNeededLoading: Boolean read GetAreRecordsNeededLoading;
    property AreRecordsNeededUnloading: Boolean read GetAreRecordsNeededUnloading;
    property DataController: TcxCustomDataController read GetDataController;
    property ScreenCanvas: TdxPSReportRenderCustomCanvas read GetScreenCanvas;
    property FirstInternalRecordIndex: Integer read GetFirstInternalRecordIndex;
    property HeaderAvailableWidths[Row: TcxCustomRow]: Integer read GetHeaderAvailableWidth;
    property IndentAreas[Row: TcxCustomRow]: Integer read GetIndentArea;
    property InternalIndexesByRecordIndex[RecordIndex: Integer]: Integer read GetInternalIndexByRecordIndex;
    property InternalRecordCount: Integer read GetInternalRecordCount;
    property InternalRecordWidth: Integer read GetInternalRecordWidth;
    property IsPrevGridMode: Boolean read FIsPrevGridMode;
    property LookAndFeelItemCount: Integer read GetLookAndFeelItemCount;
    property LookAndFeelItems[Index: Integer]: TdxReportVisualItem read GetLookAndFeelItem;
    property Renderer: TdxPSReportRenderer read GetRenderer;
    property RowHelpers[Row: TcxCustomRow]: TcxVerticalGridCustomRowHelper read GetRowHelper;
    property RowPlaceCount: Integer read GetRowPlaceCount;
    property RowPlaceFlatIndexes[WrapIndex, Index: Integer]: Integer read GetRowPlaceFlatIndex;
    property RowPlaces[WrapIndex, Index: Integer]: TcxVerticalGridCustomRowPlace read GetRowPlace;
    property RowPlacesByRow[Row: TcxCustomRow; WrapIndex: Integer]: TcxVerticalGridCustomRowPlace read GetRowPlaceByRow;
    property ViewWidths[WrapIndex: Integer]: Integer read GetViewWidth write SetViewWidth;
    property WrappedRecordStartIndexes[Index: Integer]: Integer read GetWrappedRecordStartIndex;
    property WrappedRecordStopIndexes[Index: Integer]: Integer read GetWrappedRecordStopIndex;
  public
    constructor Create(ABuilder: TcxCustomVerticalGridReportLinkBuilder); override;
    destructor Destroy; override;

    function Adapter: TcxCustomVerticalGridAdapter; overload; virtual;

    procedure DoInitializeHost(AHost: TdxReportCell); virtual;
    { Images }
    procedure DoInitializeRowImage(ARow: TcxCustomRow; AnItem: TdxReportCellGraphic; AnIndex: Integer); virtual;
    function GetRowImageCellSides(ARow: TcxCustomRow): TdxCellSides; virtual;
    function GetRowImageClass: TdxReportCellGraphicClass; virtual;
    function GetRowImageViewParams(ARow: TcxCustomRow; AnIndex: Integer): TdxReportItemViewParams; virtual;
    { Indents and Expand Buttons }
    procedure DoInitializeRowIndent(ARow: TcxCustomRow; AnItem: TdxReportCellExpandButton; AnIndex, AnIndentCount: Integer); virtual;
    procedure DoInitializeExpandButton(ARow: TcxCustomRow; AnItem: TdxReportCellExpandButton; AnIndex, AnIndentCount: Integer); virtual;
    procedure DoReportLinkInitializeRowIndent(ARow: TcxCustomRow; AnItem: TdxReportCellExpandButton; AnIndex: Integer); virtual;
    function GetRowIndentCellSides(ARow: TcxCustomRow; AnIndex, AnIndentCount: Integer): TdxCellSides; virtual;
    function GetRowIndentClass: TdxReportCellExpandButtonClass; virtual;
    function GetRowIndentViewParams(ARow: TcxCustomRow; AnIndex, AnIndentCount: Integer): TdxReportItemViewParams; virtual;
    { Rows }
    procedure DoInitializeCategoryRow(ARow: TcxCategoryRow; AnItem: TdxReportCellImage); virtual;
    procedure DoInitializeRowReportRow(ARow: TcxCustomRow; AReportRow: TdxReportCell); virtual;
    procedure DoInitializeRowHeader(ARow: TcxCustomRow; AnItem: TdxReportCellImage; AnIndex: Integer); virtual;
    procedure DoInitializeRowValue(ARow: TcxCustomRow; AnItem: TAbstractdxReportCellData; AnIndex, ARecordIndex: Integer); virtual;
    procedure DoReportLinkInitializeRowHeader(ARow: TcxCustomRow; AnItem: TdxReportCellImage; AnIndex: Integer); virtual;
    procedure DoReportLinkInitializeRowValue(ARow: TcxCustomRow; AnItem: TAbstractdxReportCellData; AnIndex, ARecordIndex: Integer); virtual;
    function GetCategoryRowCellSides(ARow: TcxCustomRow): TdxCellSides; virtual;
    function GetCategoryRowClass(ARow: TcxCustomRow): TdxReportCellImageClass; virtual;
    function GetCategoryRowViewParams(ARow: TcxCustomRow): TdxReportItemViewParams; virtual;
    function GetCellValue(ARow: TcxCustomRow; AProperties: TcxCustomEditProperties; AnIndex, ARecordIndex: Integer): TcxEditValue; virtual;
    function GetRowHeaderCellSides(ARow: TcxCustomRow; AnIndex: Integer): TdxCellSides; virtual;
    function GetRowHeaderClass(ARow: TcxCustomRow): TdxReportCellImageClass; virtual;
    function GetRowHeaderViewParams(ARow: TcxCustomRow; AnIndex: Integer): TdxReportItemViewParams; virtual;
    function GetRowValueCellSides(ARow: TcxCustomRow; AnIndex, ARecordIndex: Integer): TdxCellSides; virtual;
    function GetRowValueClass(ARow: TcxCustomRow; AIndex, ARecordIndex: Integer): TdxReportCellDataClass; virtual;
    function GetRowValueViewParams(ARow: TcxCustomRow; AIndex, ARecordIndex: Integer): TdxReportItemViewParams; virtual;
    { Value and Header Separators }
    procedure DoInitializeRowHeaderSeparator(ARow: TcxCustomMultiEditorRow; AnItem: TdxReportCellString; AnIndex: Integer); virtual;
    procedure DoInitializeRowValueSeparator(ARow: TcxCustomMultiEditorRow; AnItem: TdxReportCellString; AnIndex, ARecordIndex: Integer); virtual;
    procedure DoReportLinkInitializeRowHeaderSeparator(ARow: TcxCustomMultiEditorRow; AnItem: TdxReportCellString; AnIndex: Integer); virtual;
    procedure DoReportLinkInitializeRowValueSeparator(ARow: TcxCustomMultiEditorRow; AnItem: TdxReportCellString; AnIndex, ARecordIndex: Integer); virtual;
    function GetRowHeaderSeparatorCellSides(ARow: TcxCustomMultiEditorRow): TdxCellSides;
    function GetRowHeaderSeparatorClass(ARow: TcxCustomMultiEditorRow): TdxReportCellStringClass; virtual;
    function GetRowHeaderSeparatorViewParams(ARow: TcxCustomMultiEditorRow; AnIndex: Integer): TdxReportItemViewParams; virtual;
    function GetRowValueSeparatorCellSides(ARow: TcxCustomMultiEditorRow): TdxCellSides;
    function GetRowValueSeparatorClass(ARow: TcxCustomMultiEditorRow): TdxReportCellStringClass; virtual;
    function GetRowValueSeparatorViewParams(ARow: TcxCustomMultiEditorRow; AnIndex, ARecordIndex: Integer): TdxReportItemViewParams; virtual;
    { Wrap Separators}
    procedure DoInitializeWrapSeparatorItem(AnItem: TdxReportCellString; AWrapIndex: Integer); virtual;
    procedure DoInitializeWrapSeparatorRow(ARow: TdxReportCell); virtual;
    function GetWrapSeparatorClass: TdxReportCellStringClass; virtual;

    property AutoWidth: Boolean read GetAutoWidth;
    property ExpandButtonColors[Row: TcxCustomRow]: TColor read GetExpandButtonColor;
    property ExpandButtonSize: Integer read GetExpandButtonSize;
    property GridLinesColor: TColor read GetGridLinesColor;
    property HeaderMinWidth: Integer read GetHeaderMinWidth;
    property HeaderWidth: Integer read GetHeaderWidth write FHeaderWidth;
    property InterRecordsSpace: Integer read GetInterRecordsSpace write SetInterRecordsSpace;
    property OptionsView: TcxVerticalGridReportLinkOptionsView read GetOptionsView;
    property RecordCounts[Index: Integer]: Integer read GetRecordCount;
    property RecordMinWidths[RecordIndex: Integer]: Integer read GetRecordMinWidth;
    property RecordWidths[RecordIndex: Integer]: Integer read GetRecordWidth write SetRecordWidth;
    property RowCount: Integer read GetRowCount;
    property RowHeights[Row: TcxCustomRow]: Integer read GetRowHeight write SetRowHeight;
    property Rows[Index: Integer]: TcxCustomRow read GetRow;
    property ShowBorders: Boolean read GetShowBorders;
    property ShowHeaders: Boolean read GetShowHeaders;
    property ShowHorzGridLines: Boolean read GetShowHorzGridLines;
    property ShowVertGridLines: Boolean read GetShowVertGridLines;
    property TransparentColor: TColor read FTransparentColor write FTransparentColor default clWindow;
    property ViewMaxWidth: Integer read GetViewMaxWidth;
    property WrapCount: Integer read GetWrapCount;
    property WrapSeparatorHeight: Integer read GetWrapSeparatorHeight;
  end;

  { Options }

  TcxVerticalGridReportLinkOptionsExpanding = class(TdxCustomReportLinkOptionsExpanding)
  private
    FAutoExpandRows: Boolean;
    function GetReportLink: TcxCustomVerticalGridReportLink;
    procedure SetAutoExpandRows(Value: Boolean);
  protected
    function DesignerTabIndex: Integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
    property ReportLink: TcxCustomVerticalGridReportLink read GetReportLink;
  published
    property AutoExpandRows: Boolean read FAutoExpandRows write SetAutoExpandRows default False;
  end;

  TcxVerticalGridReportLinkOptionsFormatting = class(TdxCustomReportLinkOptionsFormatting)
  private
    function GetReportLink: TcxCustomVerticalGridReportLink;
  protected
    function DesignerTabIndex: Integer; override;
  public
    property ReportLink: TcxCustomVerticalGridReportLink read GetReportLink;
  end;

  TcxVerticalGridReportLinkOptionsPagination = class(TdxCustomReportLinkOptionsPagination)
  private
    FByRows: Boolean;
    FByWrapping: Boolean;
    FOneWrappingPerPage: Boolean;
    function GetReportLink: TcxCustomVerticalGridReportLink;
    procedure SetByRows(Value: Boolean);
    procedure SetByWrapping(Value: Boolean);
    procedure SetOneWrappingPerPage(Value: Boolean);
  protected
    function DesignerTabIndex: Integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
    property ReportLink: TcxCustomVerticalGridReportLink read GetReportLink;
  published
    property ByRows: Boolean read FByRows write SetByRows default True;
    property ByWrapping: Boolean read FByWrapping write SetByWrapping default True;
    property OneWrappingPerPage: Boolean read FOneWrappingPerPage write SetOneWrappingPerPage default False;
  end;

  TcxVerticalGridReportLinkOptionsRefinements = class(TdxCustomReportLinkOptionsRefinements)
  private
    function GetReportLink: TcxCustomVerticalGridReportLink;
    function GetSuppressBackgroundBitmaps: Boolean;
    procedure SetSuppressBackgroundBitmaps(Value: Boolean);
  protected
    function DesignerTabIndex: Integer; override;
  public
    property ReportLink: TcxCustomVerticalGridReportLink read GetReportLink;
  published
    // for backward compatibility only -> now is moved to OptionsFormatting
    property SuppressBackgroundBitmaps: Boolean read GetSuppressBackgroundBitmaps write SetSuppressBackgroundBitmaps stored False;
  end;

  TcxVerticalGridReportLinkOptionsSize = class(TdxCustomReportLinkOptionsSize)
  private
    FBestFit: Boolean;
    FKeepSameRecordWidths: Boolean;
    FWrapRecords: Boolean;
    FWrapSeparatorHeight: Integer;
    function GetReportLink: TcxCustomVerticalGridReportLink;
    procedure SetBestFit(Value: Boolean);
    procedure SetKeepSameRecordWidths(Value: Boolean);
    procedure SetWrapRecords(Value: Boolean);
    procedure SetWrapSeparatorHeight(Value: Integer);
  protected
    function DesignerTabIndex: Integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
    property ReportLink: TcxCustomVerticalGridReportLink read GetReportLink;
  published
    property AutoWidth;
    property BestFit: Boolean read FBestFit write SetBestFit default False;
    property KeepSameRecordWidths: Boolean read FKeepSameRecordWidths write SetKeepSameRecordWidths default True;
    property WrapRecords: Boolean read FWrapRecords write SetWrapRecords default True;
    property WrapSeparatorHeight: Integer read FWrapSeparatorHeight write SetWrapSeparatorHeight default 20;
  end;

  TcxVerticalGridReportLinkPrintMode = (vpmLoadedRecords, vpmAllRecords);

  TcxVerticalGridReportLinkOptionsView = class(TdxCustomReportLinkOptionsView)
  private
    FBorders: Boolean;
    FExpandButtons: Boolean;
    FHeaders: Boolean;
    FMode: TcxVerticalGridReportLinkPrintMode;
    function GetReportLink: TcxCustomVerticalGridReportLink;
    procedure SetBorders(Value: Boolean);
    procedure SetExpandButtons(Value: Boolean);
    procedure SetHeaders(Value: Boolean);
    procedure SetMode(Value: TcxVerticalGridReportLinkPrintMode);
  protected
    function DesignerTabIndex: Integer; override;
    function GetActualMode: TcxVerticalGridReportLinkPrintMode; virtual;
    property Mode: TcxVerticalGridReportLinkPrintMode read FMode write SetMode default vpmLoadedRecords;
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
    property ActualMode: TcxVerticalGridReportLinkPrintMode read GetActualMode;
    property ReportLink: TcxCustomVerticalGridReportLink read GetReportLink;
  published
    property Borders: Boolean read FBorders write SetBorders default True;
    property ExpandButtons: Boolean read FExpandButtons write SetExpandButtons default True;
    property Headers: Boolean read FHeaders write SetHeaders default True;
  end;

  { Styles }

  TcxVerticalGridReportLinkStylesClass = class of TcxVerticalGridReportLinkStyles;

  TcxVerticalGridReportLinkStyles = class(TdxCustomReportLinkStyles)
  private
    function GetReportLink: TcxCustomVerticalGridReportLink;
  protected
    function DesignerTabIndex: Integer; override;

    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
    class function GetStyleCaption(AnIndex: Integer): string; override;
    function GetStyleIndexByCaption(const Caption: string): Integer; override;
  public
    procedure Assign(Source: TPersistent); override;

    procedure GetCategoryParams(ARow: TcxCustomRow; out AParams: TcxViewParams); virtual;
    procedure GetContentParams(ARow: TcxCustomRow; AnIndex, ARecordIndex: Integer; out AParams: TcxViewParams); virtual;
    procedure GetHeaderParams(ARow: TcxCustomRow; out AParams: TcxViewParams); virtual;
    procedure GetIndentParams(ARow: TcxCustomRow; out AParams: TcxViewParams); virtual;

    property ReportLink: TcxCustomVerticalGridReportLink read GetReportLink;
  published
    property Category: TcxStyle Index vspsVGridCategory read GetValue write SetValue;
    property Content: TcxStyle Index vspsVGridContent read GetValue write SetValue;
    property Header: TcxStyle Index vspsVGridHeader read GetValue write SetValue;
    property StyleSheet;
  end;

  TcxVerticalGridReportLinkStyleSheet = class(TdxCustomReportLinkStyleSheet)
  private
    function GetStylesValue: TcxVerticalGridReportLinkStyles;
    procedure SetStylesValue(Value: TcxVerticalGridReportLinkStyles);
  public
    class function GetStylesClass: TcxCustomStylesClass; override;
  published
    property Styles: TcxVerticalGridReportLinkStyles read GetStylesValue write SetStylesValue;
  end;

  { HostInfo }

  TcxVerticalGridHostInfo = class
  private
    FParent: TdxReportCell;
  public
    Origin: TPoint;
    procedure Initialize(AParent: TdxReportCell);
    property Parent: TdxReportCell read FParent;
  end;

  { TcxCustomVerticalGridReportLink }

  { CustomDraw Events }

  TcxVerticalGridReportLinkCustomDrawRowHeaderCellEvent = procedure(Sender: TcxCustomVerticalGridReportLink;
    ACanvas: TCanvas; ARow: TcxCustomRow; ACellIndex: Integer;
    AnItem: TdxReportCellImage; var ADone: Boolean) of object;

  TcxVerticalGridReportLinkCustomDrawRowHeaderSeparatorCellEvent = procedure(Sender: TcxCustomVerticalGridReportLink;
    ACanvas: TCanvas; ARow: TcxCustomRow; AnIndex: Integer;
    AnItem: TdxReportCellString; var ADone: Boolean) of object;

  TcxVerticalGridReportLinkCustomDrawRowIndentCellEvent = procedure(Sender: TcxCustomVerticalGridReportLink;
    ACanvas: TCanvas; ARow: TcxCustomRow; AnIndex: Integer;
    AnItem: TdxReportCellExpandButton; var ADone: Boolean) of object;

  TcxVerticalGridReportLinkCustomDrawRowValueCellEvent = procedure(Sender: TcxCustomVerticalGridReportLink;
    ACanvas: TCanvas; ARow: TcxCustomRow; ACellIndex, ARecordIndex: Integer;
    AnItem: TAbstractdxReportCellData; var ADone: Boolean) of object;

  TcxVerticalGridReportLinkCustomDrawRowValueSeparatorCellEvent = procedure(Sender: TcxCustomVerticalGridReportLink;
    ACanvas: TCanvas; ARow: TcxCustomRow; ACellIndex, ARecordIndex: Integer;
    AnItem: TdxReportCellString; var ADone: Boolean) of object;

  { Initialiazation Events }

  TcxVerticalGridReportLinkInitializeRowHeaderCellEvent = procedure(Sender: TcxCustomVerticalGridReportLink;
    ARow: TcxCustomRow; ACellIndex: Integer; AnItem: TdxReportCellImage) of object;

  TcxVerticalGridReportLinkInitializeRowHeaderSeparatorCellEvent = procedure(Sender: TcxCustomVerticalGridReportLink;
    ARow: TcxCustomRow; AnIndex: Integer; AnItem: TdxReportCellString) of object;

  TcxVerticalGridReportLinkInitializeRowIndentCellEvent = procedure(Sender: TcxCustomVerticalGridReportLink;
    ARow: TcxCustomRow; AnIndex: Integer; AnItem: TdxReportCellExpandButton) of object;

  TcxVerticalGridReportLinkInitializeRowValueCellEvent = procedure(Sender: TcxCustomVerticalGridReportLink;
    ARow: TcxCustomRow; ACellIndex, ARecordIndex: Integer;
    AnItem: TAbstractdxReportCellData) of object;

  TcxVerticalGridReportLinkInitializeRowValueSeparatorCellEvent = procedure(Sender: TcxCustomVerticalGridReportLink;
    ARow: TcxCustomRow; ACellIndex, ARecordIndex: Integer;
    AnItem: TdxReportCellString) of object;

  TcxCustomVerticalGridReportLink = class(TdxCustomcxControlReportLink)
  private
    FBuilder: TcxCustomVerticalGridReportLinkBuilder;
    FCategoryFont: TFont;
    FDelimitersHardHorz: TList;
    FDelimitersHardVert: TList;
    FHostInfo: TcxVerticalGridHostInfo;
    FOnCustomDrawRowHeaderCell: TcxVerticalGridReportLinkCustomDrawRowHeaderCellEvent;
    FOnCustomDrawRowHeaderSeparatorCell: TcxVerticalGridReportLinkCustomDrawRowHeaderSeparatorCellEvent;
    FOnCustomDrawRowIndentCell: TcxVerticalGridReportLinkCustomDrawRowIndentCellEvent;
    FOnCustomDrawRowValueCell: TcxVerticalGridReportLinkCustomDrawRowValueCellEvent;
    FOnCustomDrawRowValueSeparatorCell: TcxVerticalGridReportLinkCustomDrawRowValueSeparatorCellEvent;
    FOnInitializeRowHeaderCell: TcxVerticalGridReportLinkInitializeRowHeaderCellEvent;
    FOnInitializeRowHeaderSeparatorCell: TcxVerticalGridReportLinkInitializeRowHeaderSeparatorCellEvent;
    FOnInitializeRowIndentCell: TcxVerticalGridReportLinkInitializeRowIndentCellEvent;
    FOnInitializeRowValueCell: TcxVerticalGridReportLinkInitializeRowValueCellEvent;
    FOnInitializeRowValueSeparatorCell: TcxVerticalGridReportLinkInitializeRowValueSeparatorCellEvent;
    function GetActiveStyles: TcxVerticalGridReportLinkStyles;
    function GetDesignWindow: TcxfmVerticalGridReportLinkDesignWindow;
    function GetOptionsExpanding: TcxVerticalGridReportLinkOptionsExpanding;
    function GetOptionsFormatting: TcxVerticalGridReportLinkOptionsFormatting;
    function GetOptionsPagination: TcxVerticalGridReportLinkOptionsPagination;
    function GetOptionsRefinements: TcxVerticalGridReportLinkOptionsRefinements;
    function GetOptionsSize: TcxVerticalGridReportLinkOptionsSize;
    function GetOptionsView: TcxVerticalGridReportLinkOptionsView;
    function GetStyles: TcxVerticalGridReportLinkStyles;
    function GetVerticalGrid: TcxCustomVerticalGrid;
    procedure SetCategoryFont(Value: TFont);
    procedure SetOnCustomDrawRowHeaderCell(Value: TcxVerticalGridReportLinkCustomDrawRowHeaderCellEvent);
    procedure SetOnCustomDrawRowHeaderSeparatorCell(Value: TcxVerticalGridReportLinkCustomDrawRowHeaderSeparatorCellEvent);
    procedure SetOnCustomDrawRowIndentCell(Value: TcxVerticalGridReportLinkCustomDrawRowIndentCellEvent);
    procedure SetOnCustomDrawRowValueCell(Value: TcxVerticalGridReportLinkCustomDrawRowValueCellEvent);
    procedure SetOnCustomDrawRowValueSeparatorCell(Value: TcxVerticalGridReportLinkCustomDrawRowValueSeparatorCellEvent);
    procedure SetOptionsExpanding(Value: TcxVerticalGridReportLinkOptionsExpanding);
    procedure SetOptionsFormatting(Value: TcxVerticalGridReportLinkOptionsFormatting);
    procedure SetOptionsPagination(Value: TcxVerticalGridReportLinkOptionsPagination);
    procedure SetOptionsRefinements(Value: TcxVerticalGridReportLinkOptionsRefinements);
    procedure SetOptionsSize(Value: TcxVerticalGridReportLinkOptionsSize);
    procedure SetOptionsView(Value: TcxVerticalGridReportLinkOptionsView);
    procedure SetStyles(Value: TcxVerticalGridReportLinkStyles);
  protected
    procedure ConstructReport(AReportCells: TdxReportCells); override;
    procedure ConvertCoords; override;
    procedure CustomDraw(AItem: TAbstractdxReportCellData; ACanvas: TCanvas;
      ABoundsRect, AClientRect: TRect; var ADone: Boolean); override;
    procedure FontChanged(Sender: TObject); override;
    function GetBreakPagesByHardDelimiters: Boolean; override;
    procedure GetImageLists(AProc: TdxPSGetImageListProc); override;
    function GetRebuildOnPageParamsChange(AUpdateCodes: TdxPrinterPageUpdateCodes): Boolean; override;
    function GetUseHardVertDelimiters: Boolean; override;
    procedure InternalRestoreFromOriginal; override;
    function IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean; override;
    procedure MakeHardDelimiters(AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList); override;

    procedure DoCustomDrawRowHeaderCell(ACanvas: TCanvas; ARow: TcxCustomRow;
      AnIndex: Integer; AnItem: TdxReportCellImage; var ADone: Boolean); dynamic;
    procedure DoCustomDrawRowHeaderSeparatorCell(ACanvas: TCanvas; ARow: TcxCustomRow;
      AnIndex: Integer; AnItem: TdxReportCellString; var ADone: Boolean); dynamic;
    procedure DoCustomDrawRowIndentCell(ACanvas: TCanvas; ARow: TcxCustomRow;
      AnIndex: Integer; AnItem: TdxReportCellExpandButton; var ADone: Boolean); dynamic;
    procedure DoCustomDrawRowValueCell(ACanvas: TCanvas; ARow: TcxCustomRow;
      ACellIndex, ARecordIndex: Integer; AnItem: TAbstractdxReportCellData; var ADone: Boolean); dynamic;
    procedure DoCustomDrawRowValueSeparatorCell(ACanvas: TCanvas; ARow: TcxCustomRow;
      ACellIndex, ARecordIndex: Integer; AnItem: TdxReportCellString; var ADone: Boolean); dynamic;
    procedure DoInitializeRowHeaderCell(ARow: TcxCustomRow; ACellIndex: Integer;
      AnItem: TdxReportCellImage); dynamic;
    procedure DoInitializeRowHeaderSeparatorCell(ARow: TcxCustomRow; AnIndex: Integer;
      AnItem: TdxReportCellString); dynamic;
    procedure DoInitializeRowIndentCell(ARow: TcxCustomRow; AnIndex: Integer;
      AnItem: TdxReportCellExpandButton); dynamic;
    procedure DoInitializeRowValueCell(ARow: TcxCustomRow; ACellIndex, ARecordIndex: Integer;
      AnItem: TAbstractdxReportCellData); dynamic;
    procedure DoInitializeRowValueSeparatorCell(ARow: TcxCustomRow;
      ACellIndex, ARecordIndex: Integer; AnItem: TdxReportCellString); dynamic;

    function GetOptionsExpandingClass: TdxCustomReportLinkOptionsExpandingClass; override;
    function GetOptionsFormattingClass: TdxCustomReportLinkOptionsFormattingClass; override;
    function GetOptionsPaginationClass: TdxCustomReportLinkOptionsPaginationClass; override;
    function GetOptionsRefinementsClass: TdxCustomReportLinkOptionsRefinementsClass; override;
    function GetOptionsSizeClass: TdxCustomReportLinkOptionsSizeClass; override;
    function GetOptionsViewClass: TdxCustomReportLinkOptionsViewClass; override;

    function GetAreNativeStylesAvailable: Boolean; override;
    function GetStylesClass: TdxCustomReportLinkStylesClass; override;
    function GetStyleSheetClass: TdxCustomReportLinkStyleSheetClass; override;
    function GetStyleSheetPrototype: TdxCustomReportLinkStyleSheet; override;
    procedure PrepareConstruct; override;
    procedure UnprepareConstruct; override;

    procedure AddHorizontalHardDelimiter(ADelimiter: Integer);
    procedure AddVerticalHardDelimiter(ADelimiter: Integer); overload;
    procedure AddVerticalHardDelimiter(ADelimiter: TdxReportCell); overload;

    // Builder
    function CreateBuilder: TcxCustomVerticalGridReportLinkBuilder; virtual;
    class function GetBuilderClass: TcxCustomVerticalGridReportLinkBuilderClass; virtual;

    // CustomDraw
    procedure ExtractCustomDrawCodeDataValues(ACode: Integer;
      var AnAttribute: TcxVerticalGridAttributeID; var ACellIndex, ARecordIndex: Integer);
    function GetItemCustomDrawInfo(AnItem: TdxReportVisualItem;
       out ADrawInfo: TcxVerticalGridCellCustomDrawInfo): TcxVerticalGridAttributeID; virtual;
    function IsCustomDrawn(AnAttributeID: TcxVerticalGridAttributeID): Boolean; virtual;
    function MakeCustomDrawCodeData(AnAttribute: TcxVerticalGridAttributeID;
      ACellIndex, ARecordIndex: Integer): Integer;

    // DesignWindow
    procedure InitializePrintModeControl(AControl: TcxComboBox); virtual;
    procedure UpdateStatePrintModeControl(AControl: TcxComboBox; ALayoutItem: TdxLayoutItem); virtual;

    property ActiveStyles: TcxVerticalGridReportLinkStyles read GetActiveStyles;
    property Builder: TcxCustomVerticalGridReportLinkBuilder read FBuilder;
    property CategoryFont: TFont read FCategoryFont write SetCategoryFont;
    property DelimitersHardHorz: TList read FDelimitersHardHorz;
    property DelimitersHardVert: TList read FDelimitersHardVert;
    property HostInfo: TcxVerticalGridHostInfo read FHostInfo;
    property VerticalGrid: TcxCustomVerticalGrid read GetVerticalGrid;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DesignWindow: TcxfmVerticalGridReportLinkDesignWindow read GetDesignWindow;
  published
    property Color;
    property Font;
    property OptionsExpanding: TcxVerticalGridReportLinkOptionsExpanding read GetOptionsExpanding write SetOptionsExpanding;
    property OptionsFormatting: TcxVerticalGridReportLinkOptionsFormatting read GetOptionsFormatting write SetOptionsFormatting;
    property OptionsPagination: TcxVerticalGridReportLinkOptionsPagination read GetOptionsPagination write SetOptionsPagination;
    property OptionsRefinements: TcxVerticalGridReportLinkOptionsRefinements read GetOptionsRefinements write SetOptionsRefinements;
    property OptionsSize: TcxVerticalGridReportLinkOptionsSize read GetOptionsSize write SetOptionsSize;
    property OptionsView: TcxVerticalGridReportLinkOptionsView read GetOptionsView write SetOptionsView;
    property ScaleFonts;
    property StyleRepository;
    property Styles: TcxVerticalGridReportLinkStyles read GetStyles write SetStyles;
    property SupportedCustomDraw;

    property OnCustomDrawRowHeaderCell: TcxVerticalGridReportLinkCustomDrawRowHeaderCellEvent
      read FOnCustomDrawRowHeaderCell write SetOnCustomDrawRowHeaderCell;
    property OnCustomDrawRowHeaderSeparatorCell: TcxVerticalGridReportLinkCustomDrawRowHeaderSeparatorCellEvent
      read FOnCustomDrawRowHeaderSeparatorCell write SetOnCustomDrawRowHeaderSeparatorCell;
    property OnCustomDrawRowIndentCell: TcxVerticalGridReportLinkCustomDrawRowIndentCellEvent
      read FOnCustomDrawRowIndentCell write SetOnCustomDrawRowIndentCell;
    property OnCustomDrawRowValueCell: TcxVerticalGridReportLinkCustomDrawRowValueCellEvent
      read FOnCustomDrawRowValueCell write SetOnCustomDrawRowValueCell;
    property OnCustomDrawRowValueSeparatorCell: TcxVerticalGridReportLinkCustomDrawRowValueSeparatorCellEvent
      read FOnCustomDrawRowValueSeparatorCell write SetOnCustomDrawRowValueSeparatorCell;
    property OnInitializeRowHeaderCell: TcxVerticalGridReportLinkInitializeRowHeaderCellEvent
      read FOnInitializeRowHeaderCell write FOnInitializeRowHeaderCell;
    property OnInitializeRowHeaderSeparatorCell: TcxVerticalGridReportLinkInitializeRowHeaderSeparatorCellEvent
      read FOnInitializeRowHeaderSeparatorCell write FOnInitializeRowHeaderSeparatorCell;
    property OnInitializeRowIndentCell: TcxVerticalGridReportLinkInitializeRowIndentCellEvent
      read FOnInitializeRowIndentCell write FOnInitializeRowIndentCell;
    property OnInitializeRowValueCell: TcxVerticalGridReportLinkInitializeRowValueCellEvent
      read FOnInitializeRowValueCell write FOnInitializeRowValueCell;
    property OnInitializeRowValueSeparatorCell: TcxVerticalGridReportLinkInitializeRowValueSeparatorCellEvent
      read FOnInitializeRowValueSeparatorCell write FOnInitializeRowValueSeparatorCell;
  end;

  TcxUnboundVerticalGridReportLinkBuilder = class(TcxCustomVerticalGridReportLinkBuilder)
  public
    class function AdapterClass: TcxCustomVerticalGridAdapterClass; override;
  end;

  TcxUnboundVerticalGridAdapter = class(TcxCustomVerticalGridAdapter)
  private
    function GetVerticalGrid: TcxUnboundVerticalGrid;
  protected
    function GetLayoutStyle: TcxvgLayoutStyle; override;
    property VerticalGrid: TcxUnboundVerticalGrid read GetVerticalGrid;
  end;

  TcxUnboundVerticalGridReportLink = class(TcxCustomVerticalGridReportLink)
  protected
    class function GetBuilderClass: TcxCustomVerticalGridReportLinkBuilderClass; override;
  end;

  TcxVerticalGridReportLink = class(TcxUnboundVerticalGridReportLink)
  private
    function GetVerticalGrid: TcxVerticalGrid;
  public
    property VerticalGrid: TcxVerticalGrid read GetVerticalGrid;
  end;

  { RTTI Inspector }

  TcxRTTIInspectorReportLink = class(TcxUnboundVerticalGridReportLink)
  private
    function GetRTTIInspector: TcxRTTIInspector;
  public
    property RTTIInspector: TcxRTTIInspector read GetRTTIInspector;
  end;

  { VirtualVerticalGrid }

  TcxVirtualVerticalGridReportLinkBuilder = class(TcxCustomVerticalGridReportLinkBuilder)
  public
    class function AdapterClass: TcxCustomVerticalGridAdapterClass; override;
    class function FormatterClass: TcxCustomVerticalGridReportLinkFormatterClass; override;
  end;

  TcxVirtualVerticalGridAdapter = class(TcxCustomVerticalGridAdapter)
  private
    function GetOptionsView: TcxvgMultiRecordsOptionsView;
    function GetVerticalGrid: TcxVirtualVerticalGrid;
  protected
    function GetInterRecordsSpace: Integer; override;
    function GetLayoutStyle: TcxvgLayoutStyle; override;
  public
    property OptionsView: TcxvgMultiRecordsOptionsView read GetOptionsView;
    property VerticalGrid: TcxVirtualVerticalGrid read GetVerticalGrid;
  end;

  TcxVirtualVerticalGridReportLinkFormatter = class(TcxCustomVerticalGridReportLinkFormatter)
  protected
    function GetFirstInternalRecordIndex: Integer; override;
    function GetInternalRecordCount: Integer; override;
  end;

  TcxVirtualVerticalGridReportLink = class;

  TcxVirtualVerticalGridReportLinkOptionsView = class(TcxVerticalGridReportLinkOptionsView)
  private
    function GetReportLink: TcxVirtualVerticalGridReportLink;
  protected
    function GetActualMode: TcxVerticalGridReportLinkPrintMode; override;
  public
    property ReportLink: TcxVirtualVerticalGridReportLink read GetReportLink;
  published
    property Mode;
  end;

  TcxVirtualVerticalGridReportLink = class(TcxCustomVerticalGridReportLink)
  private
    function GetOptionsView: TcxVirtualVerticalGridReportLinkOptionsView;
    function GetVirtualVerticalGrid: TcxVirtualVerticalGrid;
    procedure SetOptionsView(Value: TcxVirtualVerticalGridReportLinkOptionsView);
  protected
    procedure InternalRestoreFromOriginal; override;

    class function GetBuilderClass: TcxCustomVerticalGridReportLinkBuilderClass; override;
    function GetOptionsViewClass: TdxCustomReportLinkOptionsViewClass; override;

    procedure InitializePrintModeControl(AControl: TcxComboBox); override;
    procedure UpdateStatePrintModeControl(AControl: TcxComboBox; ALayoutItem: TdxLayoutItem); override;
  public
    property VirtualVerticalGrid: TcxVirtualVerticalGrid read GetVirtualVerticalGrid;
  published
    property OptionsView: TcxVirtualVerticalGridReportLinkOptionsView read GetOptionsView write SetOptionsView;
  end;

  { DBVericalGrid }

  TcxDBVerticalGridReportLinkBuilder = class(TcxVirtualVerticalGridReportLinkBuilder)
  public
    class function FormatterClass: TcxCustomVerticalGridReportLinkFormatterClass; override;
  end;

  TcxDBVerticalGridReportLinkFormatter = class(TcxVirtualVerticalGridReportLinkFormatter)
  private
    function GetDataController: TcxDBVerticalGridDataController;
  protected
    function GetAreRecordsNeededLoading: Boolean; override;
    function GetAreRecordsNeededUnloading: Boolean; override;
    procedure LoadRecords; override;
    procedure UnloadRecords; override;
  public
    property DataController: TcxDBVerticalGridDataController read GetDataController;
  end;

  TcxDBVerticalGridReportLink = class(TcxVirtualVerticalGridReportLink)
  private
    function GetDBVerticalGrid: TcxDBVerticalGrid;
  protected
    class function GetBuilderClass: TcxCustomVerticalGridReportLinkBuilderClass; override;
  public
    property DBVerticalGrid: TcxDBVerticalGrid read GetDBVerticalGrid;
  end;

  { TcxfmVerticalGridReportLinkDesignWindow }

  TcxfmVerticalGridReportLinkDesignWindow = class(TdxfmCustomcxControlReportLinkDesignWindow)
    btnStyleBackgroundBitmap: TcxButton;
    btnStyleBackgroundBitmapClear: TcxButton;
    btnStyleColor: TcxButton;
    btnStyleFont: TcxButton;
    btnStyleRestoreDefaults: TcxButton;
    btnStyleSheetCopy: TcxButton;
    btnStyleSheetDelete: TcxButton;
    btnStyleSheetNew: TcxButton;
    btnStyleSheetRename: TcxButton;
    btnStylesSaveAs: TcxButton;
    cbxLookAndFeel: TcxComboBox;
    cbxPrintMode: TcxComboBox;
    cbxStyleSheets: TcxComboBox;
    chbxAutoWidth: TcxCheckBox;
    chbxBestFit: TcxCheckBox;
    chbxDisplayGraphicsAsText: TcxCheckBox;
    chbxDisplayTrackBarsAsText: TcxCheckBox;
    chbxExpandRows: TcxCheckBox;
    chbxFlatCheckMarks: TcxCheckBox;
    chbxKeepSameRecordWidths: TcxCheckBox;
    chbxOneWrappingPerPage: TcxCheckBox;
    chbxPaginateByRows: TcxCheckBox;
    chbxPaginateByWrapping: TcxCheckBox;
    chbxShowBorders: TcxCheckBox;
    chbxShowExpandButtons: TcxCheckBox;
    chbxShowHeaders: TcxCheckBox;
    chbxSuppressBackgroundBitmaps: TcxCheckBox;
    chbxTransparentGraphics: TcxCheckBox;
    chbxUseNativeStyles: TcxCheckBox;
    chbxWrapRecords: TcxCheckBox;
    cxStyleRepository1: TcxStyleRepository;
    dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup11: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup12: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup13: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup14: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup15: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup16: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup;
    dxLayoutGroup1: TdxLayoutGroup;
    pcMain: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem13: TdxLayoutItem;
    dxLayoutItem14: TdxLayoutItem;
    dxLayoutItem15: TdxLayoutItem;
    dxLayoutItem16: TdxLayoutItem;
    dxLayoutItem17: TdxLayoutItem;
    dxLayoutItem18: TdxLayoutItem;
    dxLayoutItem19: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem20: TdxLayoutItem;
    dxLayoutItem21: TdxLayoutItem;
    dxLayoutItem22: TdxLayoutItem;
    dxLayoutItem23: TdxLayoutItem;
    dxLayoutItem24: TdxLayoutItem;
    dxLayoutItem25: TdxLayoutItem;
    dxLayoutItem26: TdxLayoutItem;
    dxLayoutItem27: TdxLayoutItem;
    dxLayoutItem28: TdxLayoutItem;
    dxLayoutItem29: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem30: TdxLayoutItem;
    dxLayoutItem31: TdxLayoutItem;
    dxLayoutItem32: TdxLayoutItem;
    dxLayoutItem33: TdxLayoutItem;
    dxLayoutItem34: TdxLayoutItem;
    dxLayoutItem35: TdxLayoutItem;
    dxLayoutItem36: TdxLayoutItem;
    dxLayoutItem37: TdxLayoutItem;
    dxLayoutItem38: TdxLayoutItem;
    dxLayoutItem39: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem40: TdxLayoutItem;
    dxLayoutItem41: TdxLayoutItem;
    dxLayoutItem42: TdxLayoutItem;
    dxLayoutItem43: TdxLayoutItem;
    bvlStylesHost: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    bvlMultipleRecords: TdxLayoutSeparatorItem;
    ilStylesPopup: TcxImageList;
    imgExpanding: TcxImage;
    imgGridSize: TcxImage;
    imgLookAndFeel: TcxImage;
    imgPagination: TcxImage;
    imgRefinements: TcxImage;
    imgShow: TcxImage;
    lblExpanding: TcxLabel;
    lblLookAndFeel: TcxLabel;
    lblPagination: TcxLabel;
    lblPreviewWindow: TdxLayoutItem;
    lblPrintMode: TdxLayoutItem;
    lblRefinements: TcxLabel;
    lblShow: TcxLabel;
    lblSize: TcxLabel;
    lblStyleSheets: TcxLabel;
    lblUseNativeStyles: TcxLabel;
    miLine2: TMenuItem;
    miLine3: TMenuItem;
    miLine4: TMenuItem;
    milLine: TMenuItem;
    miStyleBackgroundBitmap: TMenuItem;
    miStyleBackgroundBitmapClear: TMenuItem;
    miStyleColor: TMenuItem;
    miStyleFont: TMenuItem;
    miStyleRestoreDefaults: TMenuItem;
    miStylesSaveAs: TMenuItem;
    miStylesSelectAll: TMenuItem;
    pmStyles: TPopupMenu;
    pnlPreview: TPanel;
    PreviewVGrid: TcxVerticalGrid;
    rowEngine: TcxEditorRow;
    rowLuxurySedan: TcxCategoryRow;
    rowManufacturer: TcxEditorRow;
    rowModel: TcxEditorRow;
    rowPicture: TcxEditorRow;
    rowTires: TcxEditorRow;
    rowTransmission: TcxEditorRow;
    styleCategory: TcxStyle;
    styleContent: TcxStyle;
    styleHeader: TcxStyle;
    tshBehaviors: TdxLayoutGroup;
    tshFormatting: TdxLayoutGroup;
    tshStyles: TdxLayoutGroup;
    tshView: TdxLayoutGroup;

    procedure btnStyleSheetCopyClick(Sender: TObject);
    procedure btnStyleSheetDeleteClick(Sender: TObject);
    procedure btnStyleSheetNewClick(Sender: TObject);
    procedure btnStyleSheetRenameClick(Sender: TObject);
    procedure cbxPrintModeChange(Sender: TObject);
    procedure cbxStyleSheetsClick(Sender: TObject);
    procedure cbxStyleSheetsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbxStyleSheetsPropertiesDrawItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
    procedure lblUseNativeStylesClick(Sender: TObject);
    procedure lbxStylesClick(Sender: TObject);
    procedure LookAndFeelChange(Sender: TObject);
    procedure miStylesSelectAllClick(Sender: TObject);
    procedure OptionsExpandingClick(Sender: TObject);
    procedure OptionsFormattingClick(Sender: TObject);
    procedure OptionsPaginationClick(Sender: TObject);
    procedure OptionsRefinementsClick(Sender: TObject);
    procedure OptionsSizeClick(Sender: TObject);
    procedure OptionsViewClick(Sender: TObject);
    procedure pmStylesPopup(Sender: TObject);
    procedure PreviewVGridDrawValue(Sender: TObject; ACanvas: TcxCanvas; APainter: TcxvgPainter; AValueInfo: TcxRowValueInfo; var Done: Boolean);
    procedure StyleBackgroundBitmapClearClick(Sender: TObject);
    procedure StyleBackgroundBitmapClick(Sender: TObject);
    procedure StyleColorClick(Sender: TObject);
    procedure StyleFontClick(Sender: TObject);
    procedure StyleRestoreDefaultsClick(Sender: TObject);
    procedure StylesSaveAsClick(Sender: TObject);
  private
    lbxStyles: TdxStylesListBox;

    function GetActiveStyle: TcxStyle;
    function GetComponent: TcxCustomVerticalGrid;
    function GetHasSelectedStyles: Boolean;
    function GetHasSelectedStylesWithAssignedBitmap: Boolean;
    function GetReportLink: TcxCustomVerticalGridReportLink;

    function CanSelectAllStyles: Boolean;
    procedure CreateControls;
    procedure CustomDrawTextRect(ACanvas: TcxCanvas; AViewInfo: TcxRowValueInfo; const AText: string);
    procedure InitializePreviewVerticalGridStyles;
    procedure LoadDataIntoPreviewVerticalGrid;
    procedure RecreateStylesListBox;
    procedure RestoreSelectedStyles(AList: TList);
    procedure SaveSelectedStyles(AList: TList);
    procedure SetActivePage;
    procedure UpdatePreviewVerticalGridStyles(const ACaption: string; AStyle: TcxStyle);

    procedure SetOptionsExpandingByIndex(Index: Integer; Value: Boolean);
    procedure SetOptionsFormattingByIndex(Index: Integer; Value: Boolean);
    procedure SetOptionsPaginationByIndex(Index: Integer; Value: Boolean);
    procedure SetOptionsRefinementsByIndex(Index: Integer; Value: Boolean);
    procedure SetOptionsSizeByIndex(Index: Integer; Value: Boolean);
    procedure SetOptionsViewByIndex(Index: Integer; Value: Boolean);
  protected
    procedure DoInitialize; override;
    function GetPreviewHost: TCustomPanel; override;
    procedure LoadGroupsIcons; override;
    procedure LoadStrings; override;
    procedure UpdateControlsState; override;
    procedure UpdatePreview; override;

    function GetDesignerTabIndex: Integer; override;
    procedure SetDesignerTabIndex(Value: Integer); override;

    procedure DoActiveStyleSheetChanged; override;
    procedure DoFormActivated(AnActive: Boolean); override;
    procedure DoRefreshStylesList; override;
    procedure DoStyleChanged(const ACaption: string; AStyle: TcxStyle); override;
    procedure DoStylesChanged(AStrings: TStrings; ARecreate: Boolean); override;

    procedure GetSelectedStyleNames(AStrings: TStrings); override;
    procedure GetStyleSheetNames(out AStrings: TStrings); override;
    procedure GetStyleNames(out AStrings: TStrings); override;

    property ActiveStyle: TcxStyle read GetActiveStyle;
    property Component: TcxCustomVerticalGrid read GetComponent;
    property HasSelectedStyles: Boolean read GetHasSelectedStyles;
    property HasSelectedStylesWithAssignedBitmap: Boolean read GetHasSelectedStylesWithAssignedBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ReportLink: TcxCustomVerticalGridReportLink read GetReportLink;
  end;

function PreviewImage: TPicture;
function PreviewImageAsString: AnsiString;

implementation

{$R *.dfm}

{ 652 x 432 - DesignWindow }
{.3 - chbxTransparentRichEdits}

uses
  Types, Variants, Math, Forms, Registry,
  cxGeometry, cxDataUtils, cxDataStorage, dxPSRes, dxPSImgs, dxPSUtl,dxBkgnd, dxPSPopupMan, dxPSdxSpreadSheetLnk,
  dxPSdxSpreadSheetLnkCore;

const
  DefaultExpandButtonSize = 11;
  DefaultIndentWidth = 20;
  DefaultRowMinHeight = 20;
  ExpandButtonInteriorColor: TColor = clWindow;

  // ------------------------------------------------------------------ //
  //  Cell.Data =  0000.0000.0000.0000.0000.0000.0000.0000 = DWORD, 32b //
  //              |    |         |                        |             //
  //              |    |         |                        |             //
  //              |ID,4| Index,8b|    RecordIndex,20b     |             //
  // ------------------------------------------------------------------ //
  //                ^       ^              ^
  //
  //                |       |               ----
  //                |        ----------------
  //                 ---------------------
  //
  //
  CodeAttributeMask     = $0000000F; // --
  CodeAttributeOffset   = $0000001C; //
  CodeIndexMask         = $000000FF; //------
  CodeIndexOffset       = $00000014; //
  CodeRecordIndexMask   = $000FFFFF; //---------
  CodeRecordIndexOffset = $00000000;

var
  FBMW760Li: TPicture;
  FDefaultdxPScxVerticalGridLinkStyleSheet: TcxVerticalGridReportLinkStyleSheet;

type
  TcxVerticalGridRowHelperFactory = class(TdxCustomClassMaps)
  private
    function GetHelperClass(Row: TcxCustomRow): TcxVerticalGridCustomRowHelperClass;
  public
    class function Instance: TcxVerticalGridRowHelperFactory; reintroduce; overload;
    property HelperClasses[Row: TcxCustomRow]: TcxVerticalGridCustomRowHelperClass read GetHelperClass; default;
  end;

  TcxDataControllerConditionalFormattingProviderAccess = class(TcxDataControllerConditionalFormattingProvider);
  TcxCustomMultiEditorRowAccess = class(TcxCustomMultiEditorRow);
  TcxCustomRowAccess = class(TcxCustomRow);
  TcxCustomVerticalGridAccess = class(TcxCustomVerticalGrid);
  TcxUnboundVerticalGridAccess = class(TcxUnboundVerticalGrid);
  TcxvgCustomViewInfoAccess = class(TcxvgCustomViewInfo);

{ Helpers }

{ CustomMultiEditorRow Helpers }

function CustomMultiEditorRow_GetProperties(AInstance: TcxCustomMultiEditorRow): TcxMultiEditorRowProperties;
begin
  Result := TcxCustomMultiEditorRowAccess(AInstance).Properties;
end;

{ CustomRow Helpers }

function CustomRow_GetProperties(AInstance: TcxCustomRow): TcxCustomRowProperties;
begin
  Result := TcxCustomRowAccess(AInstance).FProperties;
end;

function CustomRow_IsHeightAssigned(AInstance: TcxCustomRow): Boolean;
begin
  Result := TcxCustomRowAccess(AInstance).IsHeightAssigned;
end;

{ CustomVerticalGrid Helpers }

function CustomVerticalGrid_GetCategoryFont(AInstance: TcxCustomVerticalGrid): TFont;
begin
  Result := TcxCustomVerticalGridAccess(AInstance).CategoryFont;
end;

function CustomVerticalGrid_GetDataController(AInstance: TcxCustomVerticalGrid): TcxCustomDataController;
begin
  Result := TcxCustomVerticalGridAccess(AInstance).DataController;
end;

function CustomVerticalGrid_GetRecordCount(AInstance: TcxCustomVerticalGrid): Integer;
begin
  Result := TcxCustomVerticalGridAccess(AInstance).RecordCount;
end;

{ UnboundVerticalGrid Helpers }

function UnboundVerticalGrid_GetLayoutStyle(AInstance: TcxUnboundVerticalGrid): TcxvgUnboundLayoutStyle;
begin
  Result := TcxUnboundVerticalGridAccess(AInstance).LayoutStyle;
end;

{ Utilities }

function PreviewImage: TPicture;
begin
  if FBMW760Li = nil then
  begin
    FBMW760Li := TPicture.Create;
    dxLoadBitmapFromResource(FBMW760Li.Bitmap, IDB_DXPSCARBMW760LI);
  end;
  Result := FBMW760Li;
end;

function PreviewImageAsString: AnsiString;
begin
  cxImage.SavePicture(PreviewImage, Result);
end;

function DefaultdxPScxVerticalGridLinkStyleSheet: TcxVerticalGridReportLinkStyleSheet;

  function CreateStyle(AColor: TColor; AFontColor: TColor): TcxStyle;
  begin
    Result := TcxStyle.Create(DefaultdxPScxVerticalGridLinkStyleSheet);
    with Result do
    begin
      Color := AColor;
      Font.Name := dxPSCore.dxPSDefaultFontName;
      Font.Color := AFontColor;
    end;
  end;

begin
  if FDefaultdxPScxVerticalGridLinkStyleSheet = nil then
  begin
    FDefaultdxPScxVerticalGridLinkStyleSheet := TcxVerticalGridReportLinkStyleSheet.Create(nil);
    with FDefaultdxPScxVerticalGridLinkStyleSheet.Styles as TcxVerticalGridReportLinkStyles do
    begin
      Category := CreateStyle(dxPSCore.dxDefaultFixedColor, dxPSCore.dxPSDefaultFontColor);
      Content := CreateStyle(dxPSCore.dxDefaultContentColor, dxPSCore.dxPSDefaultFontColor);
      Header := CreateStyle(dxPSCore.dxDefaultFixedColor, dxPSCore.dxPSDefaultFontColor);
    end;
  end;
  Result := FDefaultdxPScxVerticalGridLinkStyleSheet;
end;

{ PlaceList Helpers }

procedure ClearPlaceList(AList: TList);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    TObject(AList[I]).Free;
  AList.Clear;
end;

function CreatePlaceList(ACount: Integer): TList;
var
  I: Integer;
begin
  Result := TList.Create;
  Result.Capacity := ACount;
  for I := 0 to ACount - 1 do
    Result.Add(TcxVerticalGridRowElementPlace.Create);
end;

{ TcxVerticalGridRowHelperFactory }

function dxVerticalGridRowHelperFactory: TcxVerticalGridRowHelperFactory;
begin
  Result := TcxVerticalGridRowHelperFactory.Instance;
end;

class function TcxVerticalGridRowHelperFactory.Instance: TcxVerticalGridRowHelperFactory;
begin
  Result := inherited Instance as TcxVerticalGridRowHelperFactory;
end;

function TcxVerticalGridRowHelperFactory.GetHelperClass(Row: TcxCustomRow): TcxVerticalGridCustomRowHelperClass;
begin
  Result := TcxVerticalGridCustomRowHelperClass(PairClasses[Row.ClassType]);
end;

{ CLR Accessors }


{ TcxVerticalGridRowElementPlace }

procedure TcxVerticalGridRowElementPlace.Clear;
begin
  Offset := 0;
  Width := 0;
end;

{ TcxVerticalGridCustomRowPlace }

constructor TcxVerticalGridCustomRowPlace.Create(AFormatter: TcxCustomVerticalGridReportLinkFormatter;
  ARow: TcxCustomRow; AWrapIndex: Integer);
begin
  inherited Create;
  FFormatter := AFormatter;
  FRow := ARow;
  FWrapIndex := AWrapIndex;
end;

procedure TcxVerticalGridCustomRowPlace.AddDelimiters(AReportLink: TcxCustomVerticalGridReportLink);
begin
end;

procedure TcxVerticalGridCustomRowPlace.Calculate;
begin
end;

function TcxVerticalGridCustomRowPlace.CalculatedHeaderWidth: Integer;
begin
  Result := CalculatedMinWidth;
end;

function TcxVerticalGridCustomRowPlace.CalculatedMinHeight: Integer;
begin
  Result := Formatter.GetImageHeight(Row, 0);
  if Result = 0 then
    Result := DefaultRowMinHeight;
end;

function TcxVerticalGridCustomRowPlace.CalculatedMinWidth: Integer;
begin
  Result := IndentArea;
end;

function TcxVerticalGridCustomRowPlace.CalculatedRecordWidth(ARecordIndex: Integer): Integer;
begin
  Result := 0;
end;

function TcxVerticalGridCustomRowPlace.CalculatedTextPatternHeight(AFont: TFont): Integer;
begin
  Result := 2 + cxTextHeight(AFont) + 2;
end;

function TcxVerticalGridCustomRowPlace.CalculatedTextWidth(
  const AText: string; AFont: TFont): Integer;
begin
  Result := 3 + cxTextWidth(AFont, AText) + 3;
end;

procedure TcxVerticalGridCustomRowPlace.Clear;
begin
end;

function TcxVerticalGridCustomRowPlace.GetHeaderAvailableWidth: Integer;
begin
  Result := Formatter.HeaderAvailableWidths[Row];
end;

function TcxVerticalGridCustomRowPlace.GetValueAvailableWidth(RecordIndex: Integer): Integer;
begin
  Result := Formatter.RecordWidths[RecordIndex];
end;

function TcxVerticalGridCustomRowPlace.Row: TcxCustomRow;
begin
  Result := FRow;
end;

function TcxVerticalGridCustomRowPlace.RowHelper: TcxVerticalGridCustomRowHelper;
begin
  Result := Formatter.RowHelpers[Row];
end;

function TcxVerticalGridCustomRowPlace.GetScreenCanvas: TdxPSReportRenderCustomCanvas;
begin
  Result := Formatter.ScreenCanvas;
end;

function TcxVerticalGridCustomRowPlace.GetHasHeader: Boolean;
begin
  Result := Formatter.ShowHeaders;
end;

function TcxVerticalGridCustomRowPlace.GetIndentArea: Integer;
begin
  Result := Formatter.IndentAreas[Row];
end;

function TcxVerticalGridCustomRowPlace.GetInterRecordsSpace: Integer;
begin
  Result := Formatter.InterRecordsSpace;
end;

function TcxVerticalGridCustomRowPlace.GetItemCount: Integer;
begin
  Result := RowHelper.ItemCount;
end;

function TcxVerticalGridCustomRowPlace.GetRecordCount: Integer;
begin
  Result := Formatter.RecordCounts[WrapIndex];
end;

function TcxVerticalGridCustomRowPlace.GetRecordWidth(RecordIndex: Integer): Integer;
begin
  Result := Formatter.RecordWidths[RecordIndex];
end;

function TcxVerticalGridCustomRowPlace.GetStartRecordIndex: Integer;
begin
  Result := Formatter.WrappedRecordStartIndexes[WrapIndex];
end;

function TcxVerticalGridCustomRowPlace.GetStopRecordIndex: Integer;
begin
  Result := Formatter.WrappedRecordStopIndexes[WrapIndex];
end;

{ TcxVerticalGridCategoryRowPlace }

constructor TcxVerticalGridCategoryRowPlace.Create(AFormatter: TcxCustomVerticalGridReportLinkFormatter;
  ARow: TcxCustomRow; AWrapIndex: Integer);
begin
  inherited;
  FPlace := TcxVerticalGridRowElementPlace.Create;
end;

destructor TcxVerticalGridCategoryRowPlace.Destroy;
begin
  FreeAndNil(FPlace);
  inherited;
end;

procedure TcxVerticalGridCategoryRowPlace.Calculate;
begin
  inherited;
  FPlace.Offset := IndentArea;
  FPlace.Width := Formatter.ViewWidths[WrapIndex] - IndentArea;
end;

function TcxVerticalGridCategoryRowPlace.CalculatedHeaderWidth: Integer;
begin
  Result := IndentArea;
end;

function TcxVerticalGridCategoryRowPlace.CalculatedMinHeight: Integer;
begin
  Result := inherited CalculatedMinHeight;
  ScreenCanvas.SaveState;
  try
    Result := Max(Result, CalculatedTextPatternHeight(Font));
  finally
    ScreenCanvas.RestoreState;
  end;
end;

function TcxVerticalGridCategoryRowPlace.CalculatedMinWidth: Integer;
begin
  Result := inherited CalculatedMinWidth;
  ScreenCanvas.SaveState;
  try
    Inc(Result, CalculatedTextWidth(Caption, Font));
    Inc(Result, Formatter.GetImageWidth(Row, 0));
  finally
    ScreenCanvas.RestoreState;
  end;
end;

function TcxVerticalGridCategoryRowPlace.Row: TcxCategoryRow;
begin
  Result := inherited Row as TcxCategoryRow;
end;

function TcxVerticalGridCategoryRowPlace.RowHelper: TcxVerticalGridCategoryRowHelper;
begin
  Result := inherited RowHelper as TcxVerticalGridCategoryRowHelper;
end;

function TcxVerticalGridCategoryRowPlace.GetCaption: string;
begin
  Result := RowHelper.Caption;
end;

function TcxVerticalGridCategoryRowPlace.GetFont: TFont;
begin
  Result := Formatter.GetCategoryRowFont(Row);
end;

function TcxVerticalGridCategoryRowPlace.GetOffset: Integer;
begin
  Result := FPlace.Offset;
end;

function TcxVerticalGridCategoryRowPlace.GetWidth: Integer;
begin
  Result := FPlace.Width;
end;

{ TcxVerticalGridCustomEditorRowPlace }

constructor TcxVerticalGridCustomEditorRowPlace.Create(AFormatter: TcxCustomVerticalGridReportLinkFormatter;
  ARow: TcxCustomRow; AWrapIndex: Integer);
begin
  inherited;
  FHeaderPlace := TcxVerticalGridRowElementPlace.Create;
  CreateValuePlaces;
end;

destructor TcxVerticalGridCustomEditorRowPlace.Destroy;
begin
  FreeAndNilValuePlaces;
  FreeAndNil(FHeaderPlace);
  inherited;
end;

procedure TcxVerticalGridCustomEditorRowPlace.AddDelimiters(AReportLink: TcxCustomVerticalGridReportLink);
var
  I: Integer;
begin
  inherited;
  if HasHeader then
  begin
    AReportLink.AddHorizontalDelimiter(HeaderPlace.Offset);
    AReportLink.AddHorizontalDelimiter(HeaderPlace.Offset + HeaderPlace.Width);
  end;
  for I := StartRecordIndex to StartRecordIndex + ValuePlaceCount - 1 do
  begin
    AReportLink.AddHorizontalDelimiter(ValuePlaces[I].Offset);
    AReportLink.AddHorizontalDelimiter(ValuePlaces[I].Offset + ValuePlaces[I].Width);
  end;
end;

procedure TcxVerticalGridCustomEditorRowPlace.Calculate;
begin
  inherited;
  CalculateHeaderPlace;
  CalculateValuePlaces;
end;

function TcxVerticalGridCustomEditorRowPlace.CalculatedHeaderWidth: Integer;
begin
  Result := inherited CalculatedHeaderWidth;
  ScreenCanvas.SaveState;
  try
    Inc(Result, CalculatedTextWidth(RowHelper.HeaderCaptions[0], HeaderFont));
    Inc(Result, Formatter.GetImageWidth(Row, 0));
    if Result <> 0 then
      Inc(Result, 1 + 1); // Borders
  finally
    ScreenCanvas.RestoreState;
  end;
end;

function TcxVerticalGridCustomEditorRowPlace.CalculatedMinHeight: Integer;
var
  I, HeaderMinHeight, RowMinHeight: Integer;
begin
  Result := inherited CalculatedMinHeight;
  RowMinHeight := 0;
  HeaderMinHeight := CalculatedTextPatternHeight(HeaderFont);
  for I := 0 to RecordCount - 1 do
    RowMinHeight := Max(RowMinHeight, CalculatedTextPatternHeight(ValueFonts[I]));
  Result := Max(Result, Max(HeaderMinHeight, RowMinHeight));
end;

function TcxVerticalGridCustomEditorRowPlace.CalculatedRecordWidth(ARecordIndex: Integer): Integer;
begin
  if DoesItemParticipateInBestFitCalculation(ARecordIndex) then
  begin
    Result := MeasureWidth(ARecordIndex);
    if Result <> 0 then
      Inc(Result, 1 + 1); // Borders
  end
  else
    Result := 0;
end;

procedure TcxVerticalGridCustomEditorRowPlace.CalculateHeaderPlace;
begin
  HeaderPlace.Offset := IndentArea;
  if HasHeader then
    HeaderPlace.Width := HeaderAvailableWidth;
end;

procedure TcxVerticalGridCustomEditorRowPlace.CalculateValuePlaces;
var
  CurrentOffset, I: Integer;
begin
  CurrentOffset := ValuesOffset;
  for I := StartRecordIndex to StopRecordIndex do
  begin
    with ValuePlaces[I] do
    begin
      Offset := CurrentOffset;
      Width := RecordWidths[I];
    end;
    Inc(CurrentOffset, ValuePlaces[I].Width + InterRecordsSpace);
  end;
end;

procedure TcxVerticalGridCustomEditorRowPlace.Clear;
begin
  inherited;
  HeaderPlace.Clear;
  ClearValuePlaces;
end;

procedure TcxVerticalGridCustomEditorRowPlace.ClearValuePlaces;
begin
  ClearPlaceList(FValuePlaces);
end;

procedure TcxVerticalGridCustomEditorRowPlace.CreateValuePlaces;
begin
  FValuePlaces := CreatePlaceList(RecordCount);
end;

procedure TcxVerticalGridCustomEditorRowPlace.FreeAndNilValuePlaces;
begin
  ClearValuePlaces;
  FreeAndNil(FValuePlaces);
end;

function TcxVerticalGridCustomEditorRowPlace.DoesItemParticipateInBestFitCalculation(ARecordIndex: Integer): Boolean;
begin
   Result := dxPScxCommon.dxPSDataMaps.DoesItemParticipateInBestFitCalculation(Properties[ARecordIndex]);
end;

function TcxVerticalGridCustomEditorRowPlace.MeasureWidth(ARecordIndex: Integer): Integer;
begin
  Result := dxPScxCommon.dxPSDataMaps.MeasureWidth(Properties[ARecordIndex],
    Values[ARecordIndex], Formatter, ValueFonts[ARecordIndex], ARecordIndex);
end;

function TcxVerticalGridCustomEditorRowPlace.Row: TcxCustomEditorRow;
begin
  Result := inherited Row as TcxCustomEditorRow;
end;

function TcxVerticalGridCustomEditorRowPlace.RowHelper: TcxVerticalGridCustomEditorRowHelper;
begin
  Result := inherited RowHelper as TcxVerticalGridCustomEditorRowHelper;
end;

function TcxVerticalGridCustomEditorRowPlace.GetHeaderFont: TFont;
begin
  Result := Formatter.GetRowHeaderFont(Row, 0);
end;

function TcxVerticalGridCustomEditorRowPlace.GetProperty(RecordIndex: Integer): TcxCustomEditProperties;
begin
  Result := RowHelper.EditProperties[0, RecordIndex];
end;

function TcxVerticalGridCustomEditorRowPlace.GetValueFont(RecordIndex: Integer): TFont;
begin
  Result := Formatter.GetRowValueFont(Row, 0, RecordIndex);
end;

function TcxVerticalGridCustomEditorRowPlace.GetValuePlace(RecordIndex: Integer): TcxVerticalGridRowElementPlace;
begin
  Result := FValuePlaces[RecordIndex - StartRecordIndex];
end;

function TcxVerticalGridCustomEditorRowPlace.GetValuePlaceCount: Integer;
begin
  Result := FValuePlaces.Count;
end;

function TcxVerticalGridCustomEditorRowPlace.GetValue(RecordIndex: Integer): TcxEditValue;
begin
  Result := RowHelper.Values[0, RecordIndex];
end;

function TcxVerticalGridCustomEditorRowPlace.GetValuesOffset: Integer;
begin
  Result := HeaderPlace.Offset + HeaderPlace.Width;
end;

{ TcxVerticalGridCustomMultiEditorRowPlace }

constructor TcxVerticalGridCustomMultiEditorRowPlace.Create(AFormatter: TcxCustomVerticalGridReportLinkFormatter;
  ARow: TcxCustomRow; AWrapIndex: Integer);
begin
  inherited;
  CreatePlaces;
end;

destructor TcxVerticalGridCustomMultiEditorRowPlace.Destroy;
begin
  FreeAndNilPlaces;
  inherited;
end;

procedure TcxVerticalGridCustomMultiEditorRowPlace.AddDelimiters(AReportLink: TcxCustomVerticalGridReportLink);
begin
  inherited;
  AReportLink.AddHorizontalDelimiter(ValuesOffset);
end;

procedure TcxVerticalGridCustomMultiEditorRowPlace.Calculate;
begin
  inherited;
  if HasHeaderSeparators then CalculateHeaderSeparatorWidths;
  if HasValueSeparators then CalculateValueSeparatorWidths;
  CalculateHeaderPlaces;
  CalculateValuePlaces;
  if HasHeaderSeparators then CalculateHeaderSeparatorPlaces;
  if HasValueSeparators then CalculateValueSeparatorPlaces;
end;

function TcxVerticalGridCustomMultiEditorRowPlace.CalculatedHeaderWidth: Integer;
var
  I: Integer;
begin
  Result := inherited CalculatedHeaderWidth;

  for I := 0 to ItemCount - 1 do
  begin
    Inc(Result, Formatter.GetImageWidth(Row, I));
    Inc(Result, CalculatedTextWidth(RowHelper.HeaderCaptions[I], HeaderFonts[I]));
  end;

  if HasHeaderSeparators then
  begin
    CalculateHeaderSeparatorWidths;
    Inc(Result, HeaderSeparatorsArea);
  end;
end;

function TcxVerticalGridCustomMultiEditorRowPlace.CalculatedMinHeight: Integer;
var
  I, J: Integer;
  RowHeaderMinHeight, ValueSeparatorMinHeight: Integer;
  RowMinHeight, ImageHeight: Integer;
begin
  RowHeaderMinHeight := 0;
  ImageHeight := 0;

  for I := 0 to ItemCount - 1 do
  begin
    RowHeaderMinHeight := Max(RowHeaderMinHeight,
      CalculatedTextPatternHeight(HeaderFonts[I]));
    ImageHeight := Max(ImageHeight, Formatter.GetImageHeight(Row, I));
  end;

  RowMinHeight := 0;
  ValueSeparatorMinHeight := 0;
  for I := 0 to RecordCount - 1 do
    for J := 0 to ItemCount - 1 do
    begin
      RowMinHeight := Max(RowMinHeight, CalculatedTextPatternHeight(ValueFonts[J, I]));
      ValueSeparatorMinHeight := Max(ValueSeparatorMinHeight,
        CalculatedTextPatternHeight(ValueSeparatorsFonts[J, I]));
    end;

  Result := Max(RowHeaderMinHeight, Max(RowMinHeight, DefaultRowMinHeight));
  Result := Max(Result, Max(ImageHeight, ValueSeparatorMinHeight));
end;

function TcxVerticalGridCustomMultiEditorRowPlace.CalculatedRecordWidth(ARecordIndex: Integer): Integer;
var
  I, W: Integer;
begin
  Result := 0;

  for I := 0 to ItemCount - 1 do
    if DoesItemParticipateInBestFitCalculation(I, ARecordIndex) then
    begin
      W := MeasureWidth(I, ARecordIndex);
      if W <> 0 then  Inc(W, 1 + 1); // borders
      Inc(Result, W);
    end;
  if HasValueSeparators then
  begin
    CalculateValueSeparatorWidths;
    Inc(Result, ValueSeparatorsAreas[ARecordIndex]);
  end;

  if Result <> 0 then
    Inc(Result, 1 + 1); // Borders
end;

procedure TcxVerticalGridCustomMultiEditorRowPlace.CalculateHeaderPartWidths;
var
  AutoWidthObject: TcxAutoWidthObject;
  I: Integer;
begin
  AutoWidthObject := TcxAutoWidthObject.Create(RowHelper.ItemCount);
  try
    CalculatePartWidths(AutoWidthObject, HeaderAvailableWidth);
    for I := 0 to AutoWidthObject.Count - 1 do
      HeaderPlaces[I].Width := AutoWidthObject[I].AutoWidth;
  finally
    AutoWidthObject.Free;
  end;
end;

procedure TcxVerticalGridCustomMultiEditorRowPlace.CalculateHeaderPlaces;
var
  CurrentOffset, I: Integer;
begin
  if HasHeader then CalculateHeaderPartWidths;

  CurrentOffset := IndentArea;
  for I := 0 to ItemCount - 1 do
  begin
    HeaderPlaces[I].Offset := CurrentOffset;
    Inc(CurrentOffset, HeaderPlaces[I].Width);
    if HasHeaderSeparators and (I < ItemCount - 1) then
      Inc(CurrentOffset, HeaderSeparatorPlaces[I].Width);
  end;
end;

procedure TcxVerticalGridCustomMultiEditorRowPlace.CalculateHeaderSeparatorPlaces;
var
  CurrentOffset, I: Integer;
begin
  CurrentOffset := IndentArea;
  for I := 0 to HeaderSeparatorCount - 1 do
  begin
    Inc(CurrentOffset, HeaderPlaces[I].Width);
    HeaderSeparatorPlaces[I].Offset := CurrentOffset;
    Inc(CurrentOffset, HeaderSeparatorPlaces[I].Width);
  end;
end;

procedure TcxVerticalGridCustomMultiEditorRowPlace.CalculateHeaderSeparatorWidths;
var
  I: Integer;
begin
  ScreenCanvas.SaveState;
  try
    for I := 0 to HeaderSeparatorCount - 1 do
    begin
      HeaderSeparatorPlaces[I].Width := CalculatedTextWidth(
        RowHelper.HeaderSeparatorTexts[I], HeaderSeparatorsFonts[I]);
    end;
  finally
    ScreenCanvas.RestoreState;
  end;
end;

procedure TcxVerticalGridCustomMultiEditorRowPlace.CalculatePartWidths(
  AnAutoWidthObject: TcxAutoWidthObject; AnAvailableWidth: Integer);

  procedure AssignAutoWidthItem(AnItem: TcxAutoWidthItem; AProperties: TcxEditorRowItemProperties);
  begin
    AnItem.MinWidth := 0;
    AnItem.Width := AProperties.Width;
  end;

var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
    AssignAutoWidthItem(AnAutoWidthObject.AddItem, RowHelper.Properties(I));
  AnAutoWidthObject.AvailableWidth := AnAvailableWidth;
  AnAutoWidthObject.Calculate;
end;

procedure TcxVerticalGridCustomMultiEditorRowPlace.CalculateValuePartWidths;

  procedure CalculateValueRecordPartWidths(ARecordIndex: Integer);
  var
    AutoWidthObject: TcxAutoWidthObject;
    I: Integer;
  begin
    AutoWidthObject := TcxAutoWidthObject.Create(RowHelper.ItemCount);
    try
      CalculatePartWidths(AutoWidthObject, ValueAvailableWidths[ARecordIndex]);
      for I := 0 to AutoWidthObject.Count - 1 do
        ValuePlaces[I, ARecordIndex].Width := AutoWidthObject[I].AutoWidth;
    finally
      AutoWidthObject.Free;
    end;
  end;

var
  I: Integer;
begin
  for I := StartRecordIndex to StopRecordIndex do
    CalculateValueRecordPartWidths(I);
end;

procedure TcxVerticalGridCustomMultiEditorRowPlace.CalculateValuePlaces;
var
  CurrentOffset, I, J: Integer;
begin
  CalculateValuePartWidths;

  CurrentOffset := ValuesOffset;
  for I := StartRecordIndex to StopRecordIndex do
  begin
    for J := 0 to ItemCount - 1 do
    begin
      ValuePlaces[J, I].Offset := CurrentOffset;
      Inc(CurrentOffset, ValuePlaces[J, I].Width);
      if HasValueSeparators and (J < ItemCount - 1) then
        Inc(CurrentOffset, ValueSeparatorPlaces[J, I].Width);
    end;
    Inc(CurrentOffset, InterRecordsSpace);
  end;
end;

procedure TcxVerticalGridCustomMultiEditorRowPlace.CalculateValueSeparatorPlaces;
var
  CurrentOffset, I, J: Integer;
begin
  CurrentOffset := ValuesOffset;
  for I := StartRecordIndex to StopRecordIndex do
  begin
    Inc(CurrentOffset, ValuePlaces[0, I].Width);
    for J := 0 to ValueSeparatorCount - 1 do
    begin
      ValueSeparatorPlaces[J, I].Offset := CurrentOffset;
      Inc(CurrentOffset, ValuePlaces[J + 1, I].Width + ValueSeparatorPlaces[J, I].Width);
    end;
    Inc(CurrentOffset, InterRecordsSpace);
  end;
end;

procedure TcxVerticalGridCustomMultiEditorRowPlace.CalculateValueSeparatorWidths;
var
  I, J: Integer;
begin
  ScreenCanvas.SaveState;
  try
    for I := StartRecordIndex to StopRecordIndex do
      for J := 0 to ValueSeparatorCount - 1 do
      begin
        ValueSeparatorPlaces[J, I].Width := CalculatedTextWidth(
          RowHelper.ValueSeparatorTexts[J], ValueSeparatorsFonts[J, I]);
      end;
  finally
    ScreenCanvas.RestoreState;
  end;
end;

procedure TcxVerticalGridCustomMultiEditorRowPlace.Clear;
begin
  inherited;
  ClearPlaces;
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetHeaderAvailableWidth: Integer;
begin
  Result := inherited GetHeaderAvailableWidth - HeaderSeparatorsArea;
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetValueAvailableWidth(RecordIndex: Integer): Integer;
begin
  Result := inherited GetValueAvailableWidth(RecordIndex) - ValueSeparatorsAreas[RecordIndex];
end;

procedure TcxVerticalGridCustomMultiEditorRowPlace.ClearPlaces;
begin
  ClearPlaceList(FHeaderPlaces);
  ClearPlaceList(FHeaderSeparatorPlaces);
  ClearPlaceList(FValuePlaces);
  ClearPlaceList(FValueSeparatorPlaces);
end;

procedure TcxVerticalGridCustomMultiEditorRowPlace.CreatePlaces;
begin
  FHeaderPlaces := CreatePlaceList(ItemCount);
  FHeaderSeparatorPlaces := CreatePlaceList(HeaderSeparatorCount);
  FValuePlaces := CreatePlaceList(RecordCount * ItemCount);
  FValueSeparatorPlaces := CreatePlaceList(RecordCount * ValueSeparatorCount);
end;

procedure TcxVerticalGridCustomMultiEditorRowPlace.FreeAndNilPlaces;
begin
  ClearPlaces;
  FreeAndNil(FHeaderPlaces);
  FreeAndNil(FHeaderSeparatorPlaces);
  FreeAndNil(FValuePlaces);
  FreeAndNil(FValueSeparatorPlaces);
end;

function TcxVerticalGridCustomMultiEditorRowPlace.DoesItemParticipateInBestFitCalculation(ACellIndex,
  ARecordIndex: Integer): Boolean;
begin
  Result := dxPScxCommon.dxPSDataMaps.DoesItemParticipateInBestFitCalculation(Properties[ACellIndex, ARecordIndex]);
end;

function TcxVerticalGridCustomMultiEditorRowPlace.MeasureWidth(ACellIndex,
  ARecordIndex: Integer): Integer;
begin
  Result := dxPScxCommon.dxPSDataMaps.MeasureWidth(Properties[ACellIndex, ARecordIndex],
    Values[ACellIndex, ARecordIndex], Formatter, ValueFonts[ACellIndex, ARecordIndex], ARecordIndex);
end;

function TcxVerticalGridCustomMultiEditorRowPlace.Row: TcxCustomMultiEditorRow;
begin
  Result := inherited Row as TcxCustomMultiEditorRow;
end;

function TcxVerticalGridCustomMultiEditorRowPlace.RowHelper: TcxVerticalGridCustomMultiEditorRowHelper;
begin
  Result := inherited RowHelper as TcxVerticalGridCustomMultiEditorRowHelper;
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetHasHeaderSeparators: Boolean;
begin
  Result := HasHeader and RowHelper.HasHeaderSeparators;
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetHasValueSeparators: Boolean;
begin
  Result := RowHelper.HasValueSeparators;
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetHeaderFont(CellIndex: Integer): TFont;
begin
  Result := Formatter.GetRowHeaderFont(Row, CellIndex);
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetHeaderPlace(CellIndex: Integer): TcxVerticalGridRowElementPlace;
begin
  Result := FHeaderPlaces[CellIndex];
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetHeaderPlaceCount: Integer;
begin
  Result := FHeaderPlaces.Count;
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetHeaderSeparatorCount: Integer;
begin
  Result := RowHelper.HeaderSeparatorCount;
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetHeaderSeparatorPlace(CellIndex: Integer): TcxVerticalGridRowElementPlace;
begin
  Result := FHeaderSeparatorPlaces[CellIndex];
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetHeaderSeparatorPlaceCount: Integer;
begin
  Result := FHeaderSeparatorPlaces.Count;
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetHeaderSeparatorsArea: Integer;
var
  I: Integer;
begin
  if HasHeaderSeparators then
  begin
    Result := 0;
    for I := 0 to HeaderSeparatorCount - 1 do
      Inc(Result, HeaderSeparatorPlaces[I].Width);
  end
  else
    Result := 0;
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetHeaderSeparatorsFont(CellIndex: Integer): TFont;
begin
  Result := Formatter.GetRowHeaderSeparatorsFont(Row, CellIndex);
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetProperty(CellIndex, RecordIndex: Integer): TcxCustomEditProperties;
begin
  Result := RowHelper.EditProperties[CellIndex, RecordIndex];
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetValue(CellIndex, RecordIndex: Integer): TcxEditValue;
begin
  Result := RowHelper.Values[CellIndex, RecordIndex];
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetValueFlatIndex(CellIndex, RecordIndex: Integer): Integer;
begin
  Result := RecordIndex * ItemCount + CellIndex;
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetValueFont(CellIndex, RecordIndex: Integer): TFont;
begin
  Result := Formatter.GetRowValueFont(Row, CellIndex, RecordIndex);
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetValuePlace(CellIndex, RecordIndex: Integer): TcxVerticalGridRowElementPlace;
begin
  Result := FValuePlaces[ValueFlatIndexes[CellIndex, RecordIndex - StartRecordIndex]];
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetValuePlaceCount: Integer;
begin
  Result := FValuePlaces.Count;
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetValueSeparatorsArea(RecordIndex: Integer): Integer;
var
  I: Integer;
begin
  if HasValueSeparators then
  begin
    Result := 0;
    for I := 0 to RowHelper.ValueSeparatorCount - 1 do
      Inc(Result, ValueSeparatorPlaces[I, RecordIndex].Width);
  end
  else
    Result := 0;
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetValueSeparatorCount: Integer;
begin
  Result := RowHelper.ValueSeparatorCount;
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetValueSeparatorFlatIndex(CellIndex, RecordIndex: Integer): Integer;
begin
  Result := RecordIndex * ValueSeparatorCount + CellIndex;
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetValueSeparatorPlace(CellIndex, RecordIndex: Integer): TcxVerticalGridRowElementPlace;
begin
  Result := FValueSeparatorPlaces[ValueSeparatorFlatIndexes[CellIndex, RecordIndex - StartRecordIndex]];
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetValueSeparatorPlaceCount: Integer;
begin
  Result := FValueSeparatorPlaces.Count;
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetValueSeparatorsFont(CellIndex, RecordIndex: Integer): TFont;
begin
  Result := Formatter.GetRowValueSeparatorsFont(Row, CellIndex, RecordIndex);
end;

function TcxVerticalGridCustomMultiEditorRowPlace.GetValuesOffset: Integer;
begin
  with HeaderPlaces[RowHelper.ItemCount - 1] do
    Result := Offset + Width;
end;

{ TcxVerticalGridReportLinkCustomElementProducer }

constructor TcxVerticalGridReportLinkCustomElementProducer.Create(ABuilder: TcxCustomVerticalGridReportLinkBuilder);
begin
  inherited Create;
  FBuilder := ABuilder;
end;

function TcxVerticalGridReportLinkCustomElementProducer.Adapter: TcxCustomVerticalGridAdapter;
begin
  Result := Builder.Adapter;
end;

function TcxVerticalGridReportLinkCustomElementProducer.Builder: TcxCustomVerticalGridReportLinkBuilder;
begin
  Result := FBuilder;
end;

function TcxVerticalGridReportLinkCustomElementProducer.Formatter: TcxCustomVerticalGridReportLinkFormatter;
begin
  Result := Builder.Formatter;
end;

function TcxVerticalGridReportLinkCustomElementProducer.Produce(AHostInfo: TcxVerticalGridHostInfo;
  AWrapIndex: Integer): TdxReportCell;
begin
  FWrapIndex := AWrapIndex;
  CalculateRowHeight;
  CreateRowHost(AHostInfo);
  CreateRow;
  Inc(AHostInfo.Origin.Y, RowHeight);
  Result := Host;
end;

procedure TcxVerticalGridReportLinkCustomElementProducer.CalculateRowHeight;
begin
  FRowHeight := Formatter.WrapSeparatorHeight;
end;

procedure TcxVerticalGridReportLinkCustomElementProducer.CreateRow;
begin
  FRow := TdxReportCell.Create(Host);
  FRow.BoundsRect := Bounds(0, 0, RowWidth, RowHeight);
  InitializeRow;
end;

procedure TcxVerticalGridReportLinkCustomElementProducer.CreateRowHost(AHostInfo: TcxVerticalGridHostInfo);
begin
  FHost := TdxReportCell.Create(AHostInfo.Parent);
  FHost.BoundsRect := Bounds(AHostInfo.Origin.X, AHostInfo.Origin.Y, RowWidth, RowHeight);
  Formatter.DoInitializeHost(Host);
end;

procedure TcxVerticalGridReportLinkCustomElementProducer.InitializeRow;
begin

end;

function TcxVerticalGridReportLinkCustomElementProducer.GetScreenCanvas: TdxPSReportRenderCustomCanvas;
begin
  Result := Formatter.ScreenCanvas;
end;

function TcxVerticalGridReportLinkCustomElementProducer.GetRowWidth: Integer;
begin
  Result := Formatter.ViewWidths[WrapIndex];
end;

{ TcxVerticalGridReportLinkWrapSeparatorProducer }

procedure TcxVerticalGridReportLinkWrapSeparatorProducer.CreateRow;
begin
  inherited;
  CreateSeparator(Row);
end;

procedure TcxVerticalGridReportLinkWrapSeparatorProducer.CreateSeparator(AParent: TdxReportCell);
begin
  FSeparator := Formatter.GetWrapSeparatorClass.Create(AParent);
  FSeparator.BoundsRect := SeparatorBounds;
  InitializeSeparator;
end;

procedure TcxVerticalGridReportLinkWrapSeparatorProducer.InitializeRow;
begin
  Formatter.DoInitializeWrapSeparatorRow(Row);
end;

procedure TcxVerticalGridReportLinkWrapSeparatorProducer.InitializeSeparator;
begin
  Formatter.DoInitializeWrapSeparatorItem(Separator, WrapIndex);
end;

function TcxVerticalGridReportLinkWrapSeparatorProducer.GetSeparatorBounds: TRect;
begin
  Result := Row.BoundsRect;
  OffsetRect(Result, -Result.Left, -Result.Top);
end;

{ TcxVerticalGridReportLinkCustomRowProducer }

constructor TcxVerticalGridReportLinkCustomRowProducer.Create(ABuilder: TcxCustomVerticalGridReportLinkBuilder);
begin
  inherited;
  FIndents := TList.Create;
end;

destructor TcxVerticalGridReportLinkCustomRowProducer.Destroy;
begin
  FreeAndNil(FIndents);
  inherited;
end;

function TcxVerticalGridReportLinkCustomRowProducer.GridRow: TcxCustomRow;
begin
  Result := FGridRow;
end;

function TcxVerticalGridReportLinkCustomRowProducer.GridRowHelper: TcxVerticalGridCustomRowHelper;
begin
  Result := Formatter.RowHelpers[GridRow];
end;

function TcxVerticalGridReportLinkCustomRowProducer.GridRowPlace: TcxVerticalGridCustomRowPlace;
begin
  Result := Formatter.RowPlacesByRow[GridRow, WrapIndex];
end;

function TcxVerticalGridReportLinkCustomRowProducer.Produce(AHostInfo: TcxVerticalGridHostInfo;
  AGridRow: TcxCustomRow; AWrapIndex: Integer): TdxReportCell;
begin
  ClearItems;

  FWrapIndex := AWrapIndex;
  FGridRow := AGridRow;
  try
    CalculateRowHeight;

    CreateRowHost(AHostInfo);
    CreateRow;
    if AutoHeight then
    begin
      CalculateRowAutoHeight;
      FixupRowHeight;
    end;

    Inc(AHostInfo.Origin.Y, RowHeight);
    Result := Host;
  finally
    FGridRow := nil;
  end;
end;

procedure TcxVerticalGridReportLinkCustomRowProducer.CalculateRowAutoHeight;
var
  MaxRowHeight, I, AItemHeight: Integer;
  Item: TAbstractdxReportCellData;
begin
  MaxRowHeight := 0;
  for I := 0 to Row.DataItemCount - 1 do
  begin
    Item := Row.DataItems[I];
    if DoesItemParticipateInRowAutoHeightCalculation(Item) then
    begin
      AItemHeight := Item.MeasureContentHeight(ScreenCanvas);
      MaxRowHeight := Max(MaxRowHeight, AItemHeight);
    end;
  end;
  RowHeight := MaxRowHeight;
end;

procedure TcxVerticalGridReportLinkCustomRowProducer.CalculateRowHeight;
begin
  FRowHeight := Formatter.RowHeights[GridRow];
  Inc(FRowHeight, 1 + 1);
end;

procedure TcxVerticalGridReportLinkCustomRowProducer.ClearItems;
begin
  FIndents.Clear;
end;

procedure TcxVerticalGridReportLinkCustomRowProducer.CreateRow;
begin
  inherited;
  if HasIndents then CreateRowIndents(Row);
end;

function TcxVerticalGridReportLinkCustomRowProducer.CreateRowIndent(AParent: TdxReportCell): TdxReportCellExpandButton;
begin
  Result := Formatter.GetRowIndentClass.Create(AParent);
  FIndents.Add(Result);
end;

procedure TcxVerticalGridReportLinkCustomRowProducer.CreateRowIndents(AParent: TdxReportCell);
var
  I: Integer;
  Indent: TdxReportCellExpandButton;
begin
  for I := 0 to IndentCount - 1 do
  begin
    Indent := CreateRowIndent(AParent);
    Indent.BoundsRect := IndentBounds[I];
    InitializeRowIndent(Indent, I);
  end;
end;

function TcxVerticalGridReportLinkCustomRowProducer.DoesItemParticipateInRowAutoHeightCalculation(AnItem: TAbstractdxReportCellData): Boolean;
begin
  Result := True;
end;

procedure TcxVerticalGridReportLinkCustomRowProducer.FixupRowDataHeight;
var
  I: Integer;
begin
  for I := 0 to Row.DataItemCount - 1 do
    Row.DataItems[I].Height := RowHeight;
end;

procedure TcxVerticalGridReportLinkCustomRowProducer.FixupRowHeight;
begin
  FixupRowItselfHeight;
  FixupRowDataHeight;
end;

procedure TcxVerticalGridReportLinkCustomRowProducer.FixupRowItselfHeight;
begin
  Host.Height := RowHeight;
  Row.Height := RowHeight;
end;

procedure TcxVerticalGridReportLinkCustomRowProducer.InitializeRow;
begin
  Formatter.DoInitializeRowReportRow(GridRow, Row);
end;

procedure TcxVerticalGridReportLinkCustomRowProducer.InitializeRowIndent(AnItem: TdxReportCellExpandButton;
  AnIndex: Integer);
begin
  Formatter.DoInitializeRowIndent(GridRow, AnItem, AnIndex, IndentCount);
  Formatter.DoReportLinkInitializeRowIndent(GridRow, AnItem, AnIndex);
end;

function TcxVerticalGridReportLinkCustomRowProducer.GetAutoHeight: Boolean;
begin
  Result := Adapter.CellAutoHeight and GridRow.Options.CanAutoHeight and
    not CustomRow_IsHeightAssigned(GridRow);
end;

function TcxVerticalGridReportLinkCustomRowProducer.GetLineHeight: Integer;
begin
  if not CustomRow_IsHeightAssigned(GridRow) then
    Result := GridRow.Height
  else
    Result := Row.Height;
end;

function TcxVerticalGridReportLinkCustomRowProducer.GetHasIndents: Boolean;
begin
  Result := IndentCount <> 0;
end;

function TcxVerticalGridReportLinkCustomRowProducer.GetIndent(Index: Integer): TdxReportCellExpandButton;
begin
  Result := TdxReportCellExpandButton(FIndents[Index]);
end;

function TcxVerticalGridReportLinkCustomRowProducer.GetIndentBounds(Index: Integer): TRect;
begin
  Result := Bounds(IndentWidth * Index, 0, IndentWidth, RowHeight);
end;

function TcxVerticalGridReportLinkCustomRowProducer.GetIndentCount: Integer;
begin
  if Formatter.ShowHeaders then
    Result := Adapter.IndentCounts[GridRow]
  else
    Result := 0;
end;

function TcxVerticalGridReportLinkCustomRowProducer.GetIndentWidth: Integer;
begin
  Result := Adapter.IndentWidth;
end;

function TcxVerticalGridReportLinkCustomRowProducer.GetItemCount: Integer;
begin
  Result := GridRowHelper.ItemCount;
end;

function TcxVerticalGridReportLinkCustomRowProducer.GetRecordCount: Integer;
begin
  Result := Formatter.RecordCounts[WrapIndex];
end;

function TcxVerticalGridReportLinkCustomRowProducer.GetStartRecordIndex: Integer;
begin
  Result := Formatter.WrappedRecordStartIndexes[WrapIndex];
end;

function TcxVerticalGridReportLinkCustomRowProducer.GetStopRecordIndex: Integer;
begin
  Result := Formatter.WrappedRecordStopIndexes[WrapIndex];
end;

{ TcxVerticalGridReportLinkCategoryRowProducer }

function TcxVerticalGridReportLinkCategoryRowProducer.GridRow: TcxCategoryRow;
begin
  Result := inherited GridRow as TcxCategoryRow;
end;

function TcxVerticalGridReportLinkCategoryRowProducer.GridRowHelper: TcxVerticalGridCategoryRowHelper;
begin
  Result := inherited GridRowHelper as TcxVerticalGridCategoryRowHelper;
end;

function TcxVerticalGridReportLinkCategoryRowProducer.GridRowPlace: TcxVerticalGridCategoryRowPlace;
begin
  Result := inherited GridRowPlace as TcxVerticalGridCategoryRowPlace;
end;

procedure TcxVerticalGridReportLinkCategoryRowProducer.CreateCategoryRow(AParent: TdxReportCell);
var
  Item: TdxReportCellImage;
begin
  Item := Formatter.GetCategoryRowClass(GridRow).Create(Row);
  Item.BoundsRect := CategoryBounds;
  Formatter.DoInitializeCategoryRow(GridRow, Item);
end;

procedure TcxVerticalGridReportLinkCategoryRowProducer.CreateRow;
begin
  inherited;
  CreateCategoryRow(Row);
end;

function TcxVerticalGridReportLinkCategoryRowProducer.GetCategoryBounds: TRect;
begin
  Result := Bounds(GridRowPlace.Offset, 0, GridRowPlace.Width, RowHeight);
end;

{ TcxVerticalGridReportLinkCustomEditableRowProducer }

constructor TcxVerticalGridReportLinkCustomEditableRowProducer.Create(ABuilder: TcxCustomVerticalGridReportLinkBuilder);
begin
  inherited;
  FHeaders := TList.Create;
  FValues := TList.Create;
end;

destructor TcxVerticalGridReportLinkCustomEditableRowProducer.Destroy;
begin
  FreeAndNil(FValues);
  FreeAndNil(FHeaders);
  inherited;
end;

procedure TcxVerticalGridReportLinkCustomEditableRowProducer.ClearItems;
begin
  inherited;
  FHeaders.Clear;
  FValues.Clear;
end;

function TcxVerticalGridReportLinkCustomEditableRowProducer.CreateHeaderItem(AParent: TdxReportCell;
  ACellIndex: Integer): TdxReportCellImage;
begin
  Result := Formatter.GetRowHeaderClass(GridRow).Create(AParent);
  FHeaders.Add(Result);
end;

function TcxVerticalGridReportLinkCustomEditableRowProducer.CreateValueItem(AParent: TdxReportCell;
  ACellIndex, ARecordIndex: Integer): TAbstractdxReportCellData;
begin
  Result := Formatter.GetRowValueClass(GridRow, ACellIndex, ARecordIndex).Create(AParent);
  FValues.Add(Result);
end;

procedure TcxVerticalGridReportLinkCustomEditableRowProducer.DoInitializeHeader(AnItem: TdxReportCellImage;
  AnIndex: Integer);
begin
  Formatter.DoInitializeRowHeader(GridRow, AnItem, AnIndex);
  Formatter.DoReportLinkInitializeRowHeader(GridRow, AnItem, AnIndex);
end;

procedure TcxVerticalGridReportLinkCustomEditableRowProducer.DoInitializeValue(AnItem: TAbstractdxReportCellData;
  ACellIndex, ARecordIndex: Integer);
begin
  Formatter.DoInitializeRowValue(GridRow, AnItem, ACellIndex, ARecordIndex);
  Formatter.DoReportLinkInitializeRowValue(GridRow, AnItem, ACellIndex, ARecordIndex);
end;

function TcxVerticalGridReportLinkCustomEditableRowProducer.DoesItemParticipateInRowAutoHeightCalculation(
  AnItem: TAbstractdxReportCellData): Boolean;
begin
  Result := True;
end;

function TcxVerticalGridReportLinkCustomEditableRowProducer.GetHeader(Index: Integer): TdxReportCellImage;
begin
  Result := TdxReportCellImage(FHeaders[Index]);
end;

function TcxVerticalGridReportLinkCustomEditableRowProducer.GetHeaderCount: Integer;
begin
  Result := FHeaders.Count;
end;

function TcxVerticalGridReportLinkCustomEditableRowProducer.GetValue(Index: Integer): TAbstractdxReportCellData;
begin
  Result := TAbstractdxReportCellData(FValues[Index]);
end;

function TcxVerticalGridReportLinkCustomEditableRowProducer.GetValueCount: Integer;
begin
  Result := FValues.Count;
end;

{ TcxVerticalGridReportLinkCustomEditorRowProducer }

function TcxVerticalGridReportLinkCustomEditorRowProducer.GridRow: TcxCustomEditorRow;
begin
  Result := inherited GridRow as TcxCustomEditorRow;
end;

function TcxVerticalGridReportLinkCustomEditorRowProducer.GridRowHelper: TcxVerticalGridCustomEditorRowHelper;
begin
  Result := inherited GridRowHelper as TcxVerticalGridCustomEditorRowHelper;
end;

function TcxVerticalGridReportLinkCustomEditorRowProducer.GridRowPlace: TcxVerticalGridCustomEditorRowPlace;
begin
  Result := inherited GridRowPlace as TcxVerticalGridCustomEditorRowPlace;
end;

procedure TcxVerticalGridReportLinkCustomEditorRowProducer.CreateRow;
begin
  inherited;
  if HasHeader then CreateHeader(Row);
  CreateValues(Row);
end;

procedure TcxVerticalGridReportLinkCustomEditorRowProducer.CreateHeader(AParent: TdxReportCell);
var
  Header: TdxReportCellImage;
begin
  Header := CreateHeaderItem(AParent, 0);
  Header.BoundsRect := HeaderBounds;
  DoInitializeHeader(Header, 0);
end;

procedure TcxVerticalGridReportLinkCustomEditorRowProducer.CreateValues(AParent: TdxReportCell);
var
  I: Integer;
  Value: TAbstractdxReportCellData;
begin
  for I := StartRecordIndex to StopRecordIndex do
  begin
    Value := CreateValueItem(AParent, 0, I);
    Value.BoundsRect := ValueBounds[I];
    DoInitializeValue(Value, 0, I);
  end;
end;

function TcxVerticalGridReportLinkCustomEditorRowProducer.GetHasHeader: Boolean;
begin
  Result := Formatter.ShowHeaders;
end;

function TcxVerticalGridReportLinkCustomEditorRowProducer.GetHeader: TdxReportCellImage;
begin
  Result := Headers[0];
end;

function TcxVerticalGridReportLinkCustomEditorRowProducer.GetHeaderBounds: TRect;
begin
  with GridRowPlace.HeaderPlace do
    Result := Bounds(Offset, 0, Width, RowHeight);
end;

function TcxVerticalGridReportLinkCustomEditorRowProducer.GetValueBounds(Index: Integer): TRect;
begin
  with GridRowPlace do
    Result := Bounds(ValuePlaces[Index].Offset, 0, ValuePlaces[Index].Width, RowHeight);
end;

{ TcxVerticalGridReportLinkCustomMultiEditorRowProducer }

constructor TcxVerticalGridReportLinkCustomMultiEditorRowProducer.Create(ABuilder: TcxCustomVerticalGridReportLinkBuilder);
begin
  inherited;
  FHeaderSeparators := TList.Create;
  FValueSeparators := TList.Create;
end;

destructor TcxVerticalGridReportLinkCustomMultiEditorRowProducer.Destroy;
begin
  FreeAndNil(FValueSeparators);
  FreeAndNil(FHeaderSeparators);
  inherited;
end;

function TcxVerticalGridReportLinkCustomMultiEditorRowProducer.GridRow: TcxCustomMultiEditorRow;
begin
  Result := inherited GridRow as TcxCustomMultiEditorRow;
end;

function TcxVerticalGridReportLinkCustomMultiEditorRowProducer.GridRowHelper: TcxVerticalGridCustomMultiEditorRowHelper;
begin
  Result := inherited GridRowHelper as TcxVerticalGridCustomMultiEditorRowHelper;
end;

function TcxVerticalGridReportLinkCustomMultiEditorRowProducer.GridRowPlace: TcxVerticalGridCustomMultiEditorRowPlace;
begin
  Result := inherited GridRowPlace as TcxVerticalGridCustomMultiEditorRowPlace;
end;

procedure TcxVerticalGridReportLinkCustomMultiEditorRowProducer.ClearItems;
begin
  inherited;
  FHeaderSeparators.Clear;
  FValueSeparators.Clear;
end;

procedure TcxVerticalGridReportLinkCustomMultiEditorRowProducer.CreateRow;
begin
  inherited;
  CreateHeaders(Row);
  CreateValues(Row);
  if HasHeaderSeparators then CreateHeaderSeparators(Row);
  if HasValueSeparators then CreateValueSeparators(Row);
end;

procedure TcxVerticalGridReportLinkCustomMultiEditorRowProducer.CreateHeaders(AParent: TdxReportCell);
var
  I: Integer;
  Header: TdxReportCellImage;
begin
  for I := 0 to GridRowHelper.ItemCount - 1 do
  begin
    Header := CreateHeaderItem(AParent, I);
    Header.BoundsRect := HeaderBounds[I];
    DoInitializeHeader(Header, I);
  end;
end;

function TcxVerticalGridReportLinkCustomMultiEditorRowProducer.CreateHeaderSeparator(AParent: TdxReportCell): TdxReportCellString;
begin
  Result := Formatter.GetRowHeaderSeparatorClass(GridRow).Create(AParent);
  FValueSeparators.Add(Result);
end;

procedure TcxVerticalGridReportLinkCustomMultiEditorRowProducer.CreateHeaderSeparators(AParent: TdxReportCell);
var
  I: Integer;
  Separator: TdxReportCellString;
begin
  for I := 0 to HeaderSeparatorCount - 1 do
  begin
    Separator := CreateHeaderSeparator(AParent);
    Separator.BoundsRect := HeaderSeparatorBounds[I];
    Separator.Text := HeaderSeparatorTexts[I];
    DoInitializeHeaderSeparator(Separator, I);
  end;
end;

procedure TcxVerticalGridReportLinkCustomMultiEditorRowProducer.CreateValues(AParent: TdxReportCell);
var
  I, J: Integer;
  Item: TAbstractdxReportCellData;
begin
  for I := StartRecordIndex to StopRecordIndex do
    for J := 0 to ItemCount - 1 do
    begin
      Item := CreateValueItem(AParent, J, I);
      Item.BoundsRect := ValueBounds[J, I];
      DoInitializeValue(Item, J, I);
    end;
end;

function TcxVerticalGridReportLinkCustomMultiEditorRowProducer.CreateValueSeparator(AParent: TdxReportCell): TdxReportCellString;
begin
  Result := Formatter.GetRowValueSeparatorClass(GridRow).Create(AParent);
  FValueSeparators.Add(Result);
end;

procedure TcxVerticalGridReportLinkCustomMultiEditorRowProducer.CreateValueSeparators(AParent: TdxReportCell);
var
  I, J: Integer;
  Separator: TdxReportCellString;
begin
  for I := StartRecordIndex to StopRecordIndex do
    for J := 0 to ValueSeparatorCount - 1 do
    begin
      Separator := CreateValueSeparator(AParent);
      Separator.BoundsRect := ValueSeparatorBounds[J, I];
      Separator.Text := ValueSeparatorTexts[J];
      DoInitializeValueSeparator(Separator, J, I);
    end;
end;

procedure TcxVerticalGridReportLinkCustomMultiEditorRowProducer.DoInitializeHeaderSeparator(AnItem: TdxReportCellString;
  AnIndex: Integer);
begin
  Formatter.DoInitializeRowHeaderSeparator(GridRow, AnItem, AnIndex);
  Formatter.DoReportLinkInitializeRowHeaderSeparator(GridRow, AnItem, AnIndex);
end;

procedure TcxVerticalGridReportLinkCustomMultiEditorRowProducer.DoInitializeValueSeparator(AnItem: TdxReportCellString;
  AnIndex, ARecordIndex: Integer);
begin
  Formatter.DoInitializeRowValueSeparator(GridRow, AnItem, AnIndex, ARecordIndex);
  Formatter.DoReportLinkInitializeRowValueSeparator(GridRow, AnItem, AnIndex, ARecordIndex);
end;

function TcxVerticalGridReportLinkCustomMultiEditorRowProducer.GetHasHeaderSeparators: Boolean;
begin
  Result := GridRowHelper.HasHeaderSeparators;
end;

function TcxVerticalGridReportLinkCustomMultiEditorRowProducer.GetHasValueSeparators: Boolean;
begin
  Result := GridRowHelper.HasValueSeparators;
end;

function TcxVerticalGridReportLinkCustomMultiEditorRowProducer.GetHeaderBounds(Index: Integer): TRect;
begin
  with GridRowPlace.HeaderPlaces[Index] do
    Result := Bounds(Offset, 0, Width, RowHeight);
end;

function TcxVerticalGridReportLinkCustomMultiEditorRowProducer.GetHeaderSeparator(Index: Integer): TdxReportCellString;
begin
  Result := TdxReportCellString(FHeaderSeparators[Index]);
end;

function TcxVerticalGridReportLinkCustomMultiEditorRowProducer.GetHeaderSeparatorBounds(Index: Integer): TRect;
begin
  with GridRowPlace do
    Result := Bounds(HeaderSeparatorPlaces[Index].Offset, 0, HeaderSeparatorPlaces[Index].Width, RowHeight);
end;

function TcxVerticalGridReportLinkCustomMultiEditorRowProducer.GetHeaderSeparatorCount: Integer;
begin
  Result := GridRowHelper.HeaderSeparatorCount;
end;

function TcxVerticalGridReportLinkCustomMultiEditorRowProducer.GetHeaderSeparatorText(Index: Integer): string;
begin
  Result := GridRowHelper.HeaderSeparatorTexts[Index];
end;

function TcxVerticalGridReportLinkCustomMultiEditorRowProducer.GetValue(CellIndex, RecordIndex: Integer): TAbstractdxReportCellData;
begin
  Result := FValues[RecordIndex * GridRowHelper.ItemCount + CellIndex];
end;

function TcxVerticalGridReportLinkCustomMultiEditorRowProducer.GetValueBounds(CellIndex, RecordIndex: Integer): TRect;
begin
  with GridRowPlace do
    Result := Bounds(ValuePlaces[CellIndex, RecordIndex].Offset, 0, ValuePlaces[CellIndex, RecordIndex].Width, RowHeight);
end;

function TcxVerticalGridReportLinkCustomMultiEditorRowProducer.GetValueSeparator(Index: Integer): TdxReportCellString;
begin
  Result := TdxReportCellString(FValueSeparators[Index]);
end;

function TcxVerticalGridReportLinkCustomMultiEditorRowProducer.GetValueSeparatorBounds(Index, RecordIndex: Integer): TRect;
begin
  with GridRowPlace do
    Result := Bounds(ValueSeparatorPlaces[Index, RecordIndex].Offset, 0, ValueSeparatorPlaces[Index, RecordIndex].Width, RowHeight);
end;

function TcxVerticalGridReportLinkCustomMultiEditorRowProducer.GetValueSeparatorCount: Integer;
begin
  Result := GridRowHelper.ValueSeparatorCount;
end;

function TcxVerticalGridReportLinkCustomMultiEditorRowProducer.GetValueSeparatorText(Index: Integer): string;
begin
  Result := GridRowHelper.ValueSeparatorTexts[Index];
end;

{ TcxVerticalGridCustomRowHelper }

constructor TcxVerticalGridCustomRowHelper.Create(AnAdapter: TcxCustomVerticalGridAdapter);
begin
  inherited Create;
  FAdapter := AnAdapter;
end;

procedure TcxVerticalGridCustomRowHelper.Initialize(ARow: TcxCustomRow);
begin
  FRow := ARow;
end;

class function TcxVerticalGridCustomRowHelper.PairClass: TClass;
begin
  Result := RowClass;
end;

class function TcxVerticalGridCustomRowHelper.ProducerClass: TcxVerticalGridReportLinkCustomRowProducerClass;
begin
  Result := TcxVerticalGridReportLinkCustomRowProducer;
end;

class procedure TcxVerticalGridCustomRowHelper.Register;
begin
  dxVerticalGridRowHelperFactory.Register(Self);
end;

class procedure TcxVerticalGridCustomRowHelper.Unregister;
begin
  dxVerticalGridRowHelperFactory.Unregister(Self);
end;

function TcxVerticalGridCustomRowHelper.Properties(Index: Integer): TcxCustomRowProperties;
begin
  Result := GetRowProperties(Index);
end;

function TcxVerticalGridCustomRowHelper.Row: TcxCustomRow;
begin
  Result := FRow;
end;

class function TcxVerticalGridCustomRowHelper.RowClass: TcxCustomRowClass;
begin
  Result := TcxCustomRow;
end;

class function TcxVerticalGridCustomRowHelper.RowPlaceClass: TcxVerticalGridCustomRowPlaceClass;
begin
  Result := TcxVerticalGridCustomRowPlace;
end;

function TcxVerticalGridCustomRowHelper.Adapter: TcxCustomVerticalGridAdapter;
begin
  Result := FAdapter;
end;

function TcxVerticalGridCustomRowHelper.GetEditProperties(Index, RecordIndex: Integer): TcxCustomEditProperties;
begin
  Result := nil;
end;

function TcxVerticalGridCustomRowHelper.GetHasHeaderImage(Index: Integer): Boolean;
begin
  Result := (Adapter.Images <> nil) and (ImageIndexes[Index] > -1) and (ImageIndexes[Index] < Adapter.Images.Count);
end;

function TcxVerticalGridCustomRowHelper.GetHasHeaderSeparators: Boolean;
begin
  Result := False;
end;

function TcxVerticalGridCustomRowHelper.GetHasValueSeparators: Boolean;
begin
  Result := False;
end;

function TcxVerticalGridCustomRowHelper.GetHeaderAlignmentHorz(Index: Integer): TcxTextAlignX;
begin
  Result := taLeft;
end;

function TcxVerticalGridCustomRowHelper.GetHeaderAlignmentVert(Index: Integer): TcxTextAlignY;
begin
  Result := taCenterY;
end;

function TcxVerticalGridCustomRowHelper.GetHeaderCaption(Index: Integer): string;
begin
  Result := '';
end;

function TcxVerticalGridCustomRowHelper.GetImageIndex(Index: Integer): Integer;
begin
  Result := -1;
end;

function TcxVerticalGridCustomRowHelper.GetIndentCount: Integer;
begin
  Result := Row.Level + 1;
end;

function TcxVerticalGridCustomRowHelper.GetItemCount: Integer;
begin
  Result := 1;
end;

function TcxVerticalGridCustomRowHelper.GetRecordMinWidth: Integer;
begin
  Result := DefaultIndentWidth;
end;

function TcxVerticalGridCustomRowHelper.GetRowProperties(Index: Integer): TcxCustomRowProperties;
begin
  Result := CustomRow_GetProperties(Row);
end;

function TcxVerticalGridCustomRowHelper.GetDisplayText(Index, RecordIndex: Integer): string;
begin
  Result := '';
end;

function TcxVerticalGridCustomRowHelper.GetSeparatorsAlignmentVert: TcxAlignmentVert;
begin
  Result := vaCenter;
end;

function TcxVerticalGridCustomRowHelper.GetValue(Index, RecordIndex: Integer): TcxEditValue;
begin
  Result := '';
end;

{ TcxVerticalGridCategoryRowHelper }

class function TcxVerticalGridCategoryRowHelper.ProducerClass: TcxVerticalGridReportLinkCustomRowProducerClass;
begin
  Result := TcxVerticalGridReportLinkCategoryRowProducer;
end;

function TcxVerticalGridCategoryRowHelper.Properties(Index: Integer): TcxCaptionRowProperties;
begin
  Result := Row.Properties;
end;

function TcxVerticalGridCategoryRowHelper.Row: TcxCategoryRow;
begin
  Result := inherited Row as TcxCategoryRow;
end;

class function TcxVerticalGridCategoryRowHelper.RowClass: TcxCustomRowClass;
begin
  Result := TcxCategoryRow;
end;

class function TcxVerticalGridCategoryRowHelper.RowPlaceClass: TcxVerticalGridCustomRowPlaceClass;
begin
  Result := TcxVerticalGridCategoryRowPlace;
end;

function TcxVerticalGridCategoryRowHelper.GetCaption: string;
begin
  Result := HeaderCaptions[0];
end;

function TcxVerticalGridCategoryRowHelper.GetHeaderAlignmentHorz(Index: Integer): TcxTextAlignX;
begin
  Result := TextAlignXMap[Properties(Index).HeaderAlignmentHorz];
end;

function TcxVerticalGridCategoryRowHelper.GetHeaderAlignmentVert(Index: Integer): TcxTextAlignY;
begin
  Result := TextAlignYMap[Properties(Index).HeaderAlignmentVert];
end;

function TcxVerticalGridCategoryRowHelper.GetHeaderCaption(Index: Integer): string;
begin
  Result := Properties(Index).Caption;
end;

function TcxVerticalGridCategoryRowHelper.GetImageIndex(Index: Integer): Integer;
begin
  Result := Properties(Index).ImageIndex;
end;

{ TcxVerticalGridCustomEditorRowHelper }

class function TcxVerticalGridCustomEditorRowHelper.ProducerClass: TcxVerticalGridReportLinkCustomRowProducerClass;
begin
  Result := TcxVerticalGridReportLinkCustomEditorRowProducer;
end;

function TcxVerticalGridCustomEditorRowHelper.Properties(Index: Integer): TcxCustomEditorRowProperties;
begin
  Result := TcxCustomEditorRowProperties(inherited Properties(Index));
end;

function TcxVerticalGridCustomEditorRowHelper.Row: TcxCustomEditorRow;
begin
  Result := inherited Row as TcxCustomEditorRow;
end;

class function TcxVerticalGridCustomEditorRowHelper.RowClass: TcxCustomRowClass;
begin
  Result := TcxCustomEditorRow;
end;

class function TcxVerticalGridCustomEditorRowHelper.RowPlaceClass: TcxVerticalGridCustomRowPlaceClass;
begin
  Result := TcxVerticalGridCustomEditorRowPlace;
end;

function TcxVerticalGridCustomEditorRowHelper.GetEditProperties(Index, RecordIndex: Integer): TcxCustomEditProperties;
begin
  Result := TcxCustomEditorRowProperties(Properties(Index)).DisplayEditProperties[RecordIndex];
end;

function TcxVerticalGridCustomEditorRowHelper.GetHeaderAlignmentHorz(Index: Integer): TcxTextAlignX;
begin
  Result := TextAlignXMap[Properties(Index).HeaderAlignmentHorz];
end;

function TcxVerticalGridCustomEditorRowHelper.GetHeaderAlignmentVert(Index: Integer): TcxTextAlignY;
begin
  Result := TextAlignYMap[Properties(Index).HeaderAlignmentVert];
end;

function TcxVerticalGridCustomEditorRowHelper.GetHeaderCaption(Index: Integer): string;
begin
  Result := Properties(Index).Caption;
end;

function TcxVerticalGridCustomEditorRowHelper.GetImageIndex(Index: Integer): Integer;
begin
  Result := Properties(Index).ImageIndex;
end;

function TcxVerticalGridCustomEditorRowHelper.GetDisplayText(Index, RecordIndex: Integer): string;
begin
  Result := Properties(Index).DisplayTexts[RecordIndex];
end;

function TcxVerticalGridCustomEditorRowHelper.GetRecordMinWidth: Integer;
begin
  Result := Adapter.ValueMinWidth;
end;

function TcxVerticalGridCustomEditorRowHelper.GetValue(Index, RecordIndex: Integer): TcxEditValue;
begin
  Result := Properties(Index).Values[RecordIndex];
end;

{ TcxVerticalGridEditorRowHelper }

function TcxVerticalGridEditorRowHelper.Properties(Index: Integer): TcxEditorRowProperties;
begin
  Result := Row.Properties;
end;

function TcxVerticalGridEditorRowHelper.Row: TcxEditorRow;
begin
  Result := inherited Row as TcxEditorRow;
end;

class function TcxVerticalGridEditorRowHelper.RowClass: TcxCustomRowClass;
begin
  Result := TcxEditorRow;
end;

{ TcxVerticalGridDBEditorRowHelper }

function TcxVerticalGridDBEditorRowHelper.Properties(Index: Integer): TcxDBEditorRowProperties;
begin
  Result := Row.Properties;
end;

function TcxVerticalGridDBEditorRowHelper.Row: TcxDBEditorRow;
begin
  Result := inherited Row as TcxDBEditorRow;
end;

class function TcxVerticalGridDBEditorRowHelper.RowClass: TcxCustomRowClass;
begin
  Result := TcxDBEditorRow;
end;

{ TcxVerticalGridCustomMultiEditorRowHelper }

class function TcxVerticalGridCustomMultiEditorRowHelper.ProducerClass: TcxVerticalGridReportLinkCustomRowProducerClass;
begin
  Result := TcxVerticalGridReportLinkCustomMultiEditorRowProducer;
end;

function TcxVerticalGridCustomMultiEditorRowHelper.Properties(Index: Integer): TcxEditorRowItemProperties;
begin
  Result := TcxEditorRowItemProperties(GetRowProperties(Index));
end;

function TcxVerticalGridCustomMultiEditorRowHelper.RootProperties: TcxMultiEditorRowProperties;
begin
  Result := CustomMultiEditorRow_GetProperties(Row);
end;

function TcxVerticalGridCustomMultiEditorRowHelper.Row: TcxCustomMultiEditorRow;
begin
  Result := inherited Row as TcxCustomMultiEditorRow;
end;

class function TcxVerticalGridCustomMultiEditorRowHelper.RowClass: TcxCustomRowClass;
begin
  Result := TcxCustomMultiEditorRow;
end;

class function TcxVerticalGridCustomMultiEditorRowHelper.RowPlaceClass: TcxVerticalGridCustomRowPlaceClass;
begin
  Result := TcxVerticalGridCustomMultiEditorRowPlace;
end;

function TcxVerticalGridCustomMultiEditorRowHelper.GetEditProperties(Index, RecordIndex: Integer): TcxCustomEditProperties;
begin
  Result := TcxCustomEditorRowProperties(Properties(Index)).DisplayEditProperties[RecordIndex];
end;

function TcxVerticalGridCustomMultiEditorRowHelper.GetHasHeaderSeparators: Boolean;
begin
  Result := RootProperties.SeparatorKind = skString;
end;

function TcxVerticalGridCustomMultiEditorRowHelper.GetHasValueSeparators: Boolean;
begin
  Result := RootProperties.SeparatorKind = skString;
end;

function TcxVerticalGridCustomMultiEditorRowHelper.GetHeaderAlignmentHorz(Index: Integer): TcxTextAlignX;
begin
  Result := TextAlignXMap[Properties(Index).HeaderAlignmentHorz];
end;

function TcxVerticalGridCustomMultiEditorRowHelper.GetHeaderAlignmentVert(Index: Integer): TcxTextAlignY;
begin
  Result := TextAlignYMap[Properties(Index).HeaderAlignmentVert];
end;

function TcxVerticalGridCustomMultiEditorRowHelper.GetHeaderCaption(Index: Integer): string;
begin
  Result := Properties(Index).Caption;
end;

function TcxVerticalGridCustomMultiEditorRowHelper.GetImageIndex(Index: Integer): Integer;
begin
  Result := Properties(Index).ImageIndex;
end;

function TcxVerticalGridCustomMultiEditorRowHelper.GetItemCount: Integer;
begin
  Result := RootProperties.Editors.Count;
end;

function TcxVerticalGridCustomMultiEditorRowHelper.GetRecordMinWidth: Integer;
begin
  Result := Adapter.ValueMinWidth * ItemCount;
end;

function TcxVerticalGridCustomMultiEditorRowHelper.GetRowProperties(Index: Integer): TcxCustomRowProperties;
begin
  Result := RootProperties.Editors[Index];
end;

function TcxVerticalGridCustomMultiEditorRowHelper.GetDisplayText(Index, RecordIndex: Integer): string;
begin
  Result := Properties(Index).DisplayTexts[RecordIndex];
end;

function TcxVerticalGridCustomMultiEditorRowHelper.GetHeaderSeparatorCount: Integer;
begin
  if HasValueSeparators then
    Result := ItemCount - 1
  else
    Result := 0;
end;

function TcxVerticalGridCustomMultiEditorRowHelper.GetHeaderSeparatorText(Index: Integer): string;
begin
  Result := RootProperties.SeparatorString;
end;

function TcxVerticalGridCustomMultiEditorRowHelper.GetSeparatorsAlignmentVert: TcxAlignmentVert;
begin
  Result := RootProperties.SeparatorAlignmentVert;
end;

function TcxVerticalGridCustomMultiEditorRowHelper.GetValue(Index, RecordIndex: Integer): TcxEditValue;
begin
  Result := Properties(Index).Values[RecordIndex];
end;

function TcxVerticalGridCustomMultiEditorRowHelper.GetValueSeparatorCount: Integer;
begin
  if HasValueSeparators then
    Result := ItemCount - 1
  else
    Result := 0;
end;

function TcxVerticalGridCustomMultiEditorRowHelper.GetValueSeparatorText(Index: Integer): string;
begin
  Result := RootProperties.SeparatorString;
end;

{ TcxVerticalGridRowHelperCache }

constructor TcxVerticalGridRowHelperCache.Create(AnAdapter: TcxCustomVerticalGridAdapter);
begin
  inherited Create;
  FAdapter := AnAdapter;
end;

function TcxVerticalGridRowHelperCache.IndexOf(Row: TcxCustomRow): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].RowClass = Row.ClassType then Exit;

  Result := Add(dxVerticalGridRowHelperFactory.HelperClasses[Row].Create(Adapter));
end;

function TcxVerticalGridRowHelperCache.GetHelper(Row: TcxCustomRow): TcxVerticalGridCustomRowHelper;
begin
  Result := Items[IndexOf(Row)];
  Result.Initialize(Row);
end;

function TcxVerticalGridRowHelperCache.GetItem(Index: Integer): TcxVerticalGridCustomRowHelper;
begin
  Result := TcxVerticalGridCustomRowHelper(inherited Items[Index]);
end;

{ TcxVerticalGridReportLinkProducerCache }

constructor TcxVerticalGridReportLinkProducerCache.Create(ABuilder: TcxCustomVerticalGridReportLinkBuilder);
begin
  inherited Create;
  FBuilder := ABuilder;
end;

function TcxVerticalGridReportLinkProducerCache.IndexOf(AProducerClass: TcxVerticalGridReportLinkCustomRowProducerClass): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].ClassType = AProducerClass then Exit;

  Result := Add(AProducerClass.Create(Builder));
end;

function TcxVerticalGridReportLinkProducerCache.GetProducer(ProducerClass: TcxVerticalGridReportLinkCustomRowProducerClass): TcxVerticalGridReportLinkCustomRowProducer;
begin
  Result := Items[IndexOf(ProducerClass)];
end;

function TcxVerticalGridReportLinkProducerCache.GetItem(Index: Integer): TcxVerticalGridReportLinkCustomRowProducer;
begin
  Result := TcxVerticalGridReportLinkCustomRowProducer(inherited Items[Index]);
end;

{ TcxCustomVerticalGridReportLinkBuilder }

constructor TcxCustomVerticalGridReportLinkBuilder.Create(AReportLink: TcxCustomVerticalGridReportLink);
begin
  inherited Create;
  FReportLink := AReportLink;
  FAdapter := AdapterClass.Create(Self);
  FFormatter := FormatterClass.Create(Self);
  FProducerCache := TcxVerticalGridReportLinkProducerCache.Create(Self);
  FReportRows := TList.Create;
end;

destructor TcxCustomVerticalGridReportLinkBuilder.Destroy;
begin
  FreeAndNil(FWrapSeparatorProducer);
  FreeAndNil(FReportRows);
  FreeAndNil(FProducerCache);
  FreeAndNil(FFormatter);
  FreeAndNil(FAdapter);
  inherited;
end;

procedure TcxCustomVerticalGridReportLinkBuilder.Build;
begin
  BeforeBuilding;
  try
    DoBuild;
  finally
    AfterBuilding;
  end;
end;

procedure TcxCustomVerticalGridReportLinkBuilder.Progress(const APercentDone: Double);
begin
  ReportLink.DoProgress(APercentDone);
end;

function TcxCustomVerticalGridReportLinkBuilder.Adapter: TcxCustomVerticalGridAdapter;
begin
  Result := FAdapter;
end;

class function TcxCustomVerticalGridReportLinkBuilder.AdapterClass: TcxCustomVerticalGridAdapterClass;
begin
  Result := TcxCustomVerticalGridAdapter;
end;

function TcxCustomVerticalGridReportLinkBuilder.Formatter: TcxCustomVerticalGridReportLinkFormatter;
begin
  Result := FFormatter;
end;

class function TcxCustomVerticalGridReportLinkBuilder.FormatterClass: TcxCustomVerticalGridReportLinkFormatterClass;
begin
  Result := TcxCustomVerticalGridReportLinkFormatter;
end;

procedure TcxCustomVerticalGridReportLinkBuilder.AddReportRow(ARow: TdxReportCell);
begin
  if ARow.Parent = ReportLink.ReportCells.Cells then
    FReportRows.Add(ARow);
end;

procedure TcxCustomVerticalGridReportLinkBuilder.AfterBuilding;
begin
  Formatter.AfterBuilding;
  Adapter.AfterBuilding;
end;

procedure TcxCustomVerticalGridReportLinkBuilder.BeforeBuilding;
begin
  Adapter.BeforeBuilding;
  Formatter.BeforeBuilding;
end;

procedure TcxCustomVerticalGridReportLinkBuilder.CreateRow(ARow: TcxCustomRow; AWrapIndex: Integer);
var
  ReportRow: TdxReportCell;
begin
  ReportRow := GetRowProducer(ARow).Produce(ReportLink.HostInfo, ARow, AWrapIndex);
  AddReportRow(ReportRow);
end;

procedure TcxCustomVerticalGridReportLinkBuilder.CreateRows;
var
  I, J: Integer;
  Row: TcxCustomRow;
begin
  for I := 0 to Formatter.WrapCount - 1 do
  begin
    for J := 0 to Formatter.RowCount - 1 do
    begin
      Row := Formatter.Rows[J];
      CreateRow(Row, I);
      Progress(100 * (I * Formatter.RowCount + J) / (Formatter.WrapCount * Formatter.RowCount));
      if IsAborted then Break;
    end;
    if IsAborted then Break;
    if AreWrapSeparatorsNeeded and (I < Formatter.WrapCount - 1) then
      CreateWrapSeparator(I);
  end;
end;

procedure TcxCustomVerticalGridReportLinkBuilder.CreateWrapSeparator(AWrapIndex: Integer);
var
  ReportRow: TdxReportCell;
begin
  ReportRow := GetWrapSeparatorProducer(AWrapIndex).Produce(ReportLink.HostInfo, AWrapIndex);
  AddReportRow(ReportRow);
end;

function TcxCustomVerticalGridReportLinkBuilder.GetRowProducer(ARow: TcxCustomRow): TcxVerticalGridReportLinkCustomRowProducer;
begin
  Result := ProducerCache[GetRowProducerClass(ARow)] as TcxVerticalGridReportLinkCustomRowProducer;
end;

function TcxCustomVerticalGridReportLinkBuilder.GetRowProducerClass(ARow: TcxCustomRow): TcxVerticalGridReportLinkCustomRowProducerClass;
begin
  Result := Formatter.RowHelpers[ARow].ProducerClass;
end;

function TcxCustomVerticalGridReportLinkBuilder.GetWrapSeparatorProducer(AWrapIndex: Integer): TcxVerticalGridReportLinkWrapSeparatorProducer;
begin
  if FWrapSeparatorProducer = nil then
    FWrapSeparatorProducer := GetWrapSeparatorProducerClass.Create(Self);
  Result := FWrapSeparatorProducer;
end;

function TcxCustomVerticalGridReportLinkBuilder.GetWrapSeparatorProducerClass: TcxVerticalGridReportLinkWrapSeparatorProducerClass;
begin
  Result := TcxVerticalGridReportLinkWrapSeparatorProducer;
end;

function TcxCustomVerticalGridReportLinkBuilder.AreWrapSeparatorsNeeded: Boolean;
begin
  with ReportLink.OptionsPagination do
    Result := ByRows or (ByWrapping and not OneWrappingPerPage);
end;

procedure TcxCustomVerticalGridReportLinkBuilder.DoBuild;
begin
  CreateRows;
end;

function TcxCustomVerticalGridReportLinkBuilder.IsAborted: Boolean;
begin
  Result := ReportLink.AbortBuilding;
end;

function TcxCustomVerticalGridReportLinkBuilder.GetAvailableSiteWidth: Integer;
var
  AIntf: IdxReportLinkController;
  AScaleFactor: Integer;
begin
  if ReportLink.IsAggregated and Supports(TObject(ReportLink.Controller), IdxReportLinkController, AIntf) then
    Result := cxRectWidth(AIntf.GetControlSiteBounds(ReportLink.VerticalGrid))
  else
    Result := cxRectWidth(ReportLink.RealPrinterPage.PaintRectPixels);

  if Formatter.AutoWidth then
    AScaleFactor := ReportLink.RealPrinterPage.ScaleFactor
  else
    AScaleFactor := ReportLink.GetRealScaleFactor;

  Result := MulDiv(Result - 1, 100, AScaleFactor);
end;

function TcxCustomVerticalGridReportLinkBuilder.GetAvailableWidth: Integer;
begin
  if Formatter.AutoWidth then
    Result := GetAvailableSiteWidth
  else
    Result := cxRectWidth(VerticalGrid.ClientBounds);
end;

function TcxCustomVerticalGridReportLinkBuilder.GetHost: TdxReportCell;
begin
  Result := ReportLink.ReportCells.Cells;
end;

function TcxCustomVerticalGridReportLinkBuilder.GetReportCells: TdxReportCells;
begin
  Result := ReportLink.ReportCells;
end;

function TcxCustomVerticalGridReportLinkBuilder.GetReportRow(Index: Integer): TdxReportCell;
begin
  Result := TdxReportCell(FReportRows[Index]);
end;

function TcxCustomVerticalGridReportLinkBuilder.GetReportRowCount: Integer;
begin
  Result := FReportRows.Count;
end;

function TcxCustomVerticalGridReportLinkBuilder.GetVerticalGrid: TcxCustomVerticalGrid;
begin
  Result := ReportLink.VerticalGrid;
end;

{ TcxVerticalGridReportLinkBuilderHandler }

constructor TcxVerticalGridReportLinkBuilderHandler.Create(ABuilder: TcxCustomVerticalGridReportLinkBuilder);
begin
  inherited Create;
  FBuilder := ABuilder;
end;

function TcxVerticalGridReportLinkBuilderHandler.Builder: TcxCustomVerticalGridReportLinkBuilder;
begin
  Result := FBuilder;
end;

function TcxVerticalGridReportLinkBuilderHandler.GetReportLink: TcxCustomVerticalGridReportLink;
begin
  Result := Builder.ReportLink;
end;

{ TcxCustomVerticalGridAdapter }

constructor TcxCustomVerticalGridAdapter.Create(ABuilder: TcxCustomVerticalGridReportLinkBuilder);
var
  AConditionalFormatting: IcxDataControllerConditionalFormattingProviderOwner;
begin
  inherited;
  FHelperCache := TcxVerticalGridRowHelperCache.Create(Self);
  if Supports(VerticalGrid, IcxDataControllerConditionalFormattingProviderOwner, AConditionalFormatting) then
    FConditionalFormattingProvider := AConditionalFormatting.GetConditionalFormattingProvider;
end;

destructor TcxCustomVerticalGridAdapter.Destroy;
begin
  FreeAndNil(FHelperCache);
  inherited;
end;

procedure TcxCustomVerticalGridAdapter.AfterBuilding;
begin
end;

procedure TcxCustomVerticalGridAdapter.BeforeBuilding;
begin
end;

function TcxCustomVerticalGridAdapter.GetInterRecordsSpace: Integer;
begin
  Result := 0;
end;

function TcxCustomVerticalGridAdapter.GetProperties(ARow: TcxCustomRow;
  AnIndex, ARecordIndex: Integer): TcxCustomEditProperties;
begin
  Result := Helpers[ARow].GetEditProperties(AnIndex, ARecordIndex);
end;

function TcxCustomVerticalGridAdapter.GetPropertiesClass(ARow: TcxCustomRow;
  AnIndex, ARecordIndex: Integer): TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomEditPropertiesClass(GetProperties(ARow, AnIndex, ARecordIndex).ClassType);
end;

function TcxCustomVerticalGridAdapter.GetCategoryViewParams(ARow: TcxCustomRow): TcxViewParams;
begin
  Result := Styles.GetCategoryParams(ARow);
  // v3.2
  if Result.Color = clWindow then
    Result.Color := dxPSCore.dxDefaultContentColor;
end;

function TcxCustomVerticalGridAdapter.GetContentViewParams(ARow: TcxCustomRow;
  AnIndex, ARecordIndex: Integer): TcxViewParams;
var
  Properties: TcxCustomEditorRowProperties;
begin
  Properties := TcxCustomEditorRowProperties(Helpers[ARow].GetRowProperties(AnIndex));
  Result := Styles.GetContentParams(Properties, False, ARecordIndex);
  // v3.2
  if Result.Color = clWindow then
    Result.Color := dxPSCore.dxDefaultContentColor;
end;

function TcxCustomVerticalGridAdapter.GetHeaderViewParams(ARow: TcxCustomRow): TcxViewParams;
begin
  Result := Styles.GetHeaderParams(ARow);
  // v3.2
  if Result.Color = clWindow then
    Result.Color := dxPSCore.dxDefaultContentColor;
end;

function TcxCustomVerticalGridAdapter.GetImageViewParams(ARow: TcxCustomRow): TcxViewParams;
begin
  Result := GetHeaderViewParams(ARow);
end;

function TcxCustomVerticalGridAdapter.GetIndentViewParams(ARow: TcxCustomRow): TcxViewParams;
begin
  if ARow is TcxCategoryRow then
    Result := GetCategoryViewParams(ARow)
  else
    Result := GetHeaderViewParams(ARow);
end;

function TcxCustomVerticalGridAdapter.TryGetAdvancedStyle(
  ARow: TcxCustomRow; ACellIndex, ARecordIndex: Integer; out AStyle: TdxSpreadSheetCellDisplayStyle): Boolean;
var
  ACell: TPoint;
  AProvider: TcxDataControllerConditionalFormattingProviderAccess;
begin
  Result := False;
  AProvider := TcxDataControllerConditionalFormattingProviderAccess(ConditionalFormattingProvider);
  if AProvider <> nil then
  begin
    ACell.X := AProvider.FindFieldByItem(TcxCustomRowAccess(ARow).GetEditContainer(ACellIndex));
    ACell.Y := ARecordIndex;
    Result := AProvider.TryGetStyle(ACell, AStyle);
  end;
end;

function TcxCustomVerticalGridAdapter.GetBackgroundBitmap(Index: Integer): TBitmap;
begin
  Result := Styles.GetBitmap(Index);
end;

function TcxCustomVerticalGridAdapter.GetRowIndentBackgroundBitmapIndex(ARow: TcxCustomRow; ALevel: Integer): Integer;
const
  BackgroundBitmapIndexes: array[Boolean] of Integer = (vgs_Header, vgs_Category);
begin
  Result := BackgroundBitmapIndexes[GetRowParentAtLevel(ARow, ALevel) is TcxCategoryRow];
end;

function TcxCustomVerticalGridAdapter.HasBackgroundBitmap(Index: Integer): Boolean;
begin
  Result := GetBackgroundBitmap(Index) <> nil;
end;

procedure TcxCustomVerticalGridAdapter.FullExpand;
begin
  VerticalGrid.FullExpand;
end;

function TcxCustomVerticalGridAdapter.GetImageHeight(ARow: TcxCustomRow; AnIndex: Integer): Integer;
begin
  if HasRowImage(ARow, AnIndex) then
    Result := 1 + Images.Height + 1
  else
    Result := 0;
end;

function TcxCustomVerticalGridAdapter.GetImageWidth(ARow: TcxCustomRow; AnIndex: Integer): Integer;
begin
  if HasRowImage(ARow, AnIndex) then
    Result := 1 + Images.Width + 1
  else
    Result := 0;
end;

function TcxCustomVerticalGridAdapter.GetRowParentAtLevel(ARow: TcxCustomRow;
  ALevel: Integer): TcxCustomRow;
begin
  Result := ARow;
  while ALevel > 0 do
  begin
    Result := Result.Parent;
    Dec(ALevel);
  end;
end;

function TcxCustomVerticalGridAdapter.HasCategoryRowAsParentAtLevel(ARow: TcxCustomRow;
  ALevel: Integer; ACheckFollow: Boolean): Boolean;
begin
  while ALevel > 0 do
  begin
    ARow := ARow.Parent;
    if ACheckFollow and (ARow is TcxCategoryRow) then
      Break;
    Dec(ALevel);
  end;
  Result := ARow is TcxCategoryRow;
end;

function TcxCustomVerticalGridAdapter.HasRowImage(ARow: TcxCustomRow; AnIndex: Integer): Boolean;
var
  Helper: TcxVerticalGridCustomRowHelper;
begin
  Result := Images <> nil;
  if Result and (ARow <> nil) and not MakeSpaceForEmptyImage then
  begin
    Helper := Helpers[ARow];
    Result := (Helper.ImageIndexes[AnIndex] > -1) and (Helper.ImageIndexes[AnIndex] < Images.Count);
  end;
end;

function TcxCustomVerticalGridAdapter.IsRowLastChildAtLevel(ARow: TcxCustomRow; ALevel: Integer): Boolean;
begin
  while (ALevel > 0) and ARow.IsLastVisible do
  begin
    ARow := ARow.Parent;
    Dec(ALevel);
  end;
  Result := ARow.IsLastVisible;
end;

function TcxCustomVerticalGridAdapter.IsRowVisible(ARow: TcxCustomRow): Boolean;
begin
  Result := VerticalGrid.IsRowVisible(ARow);
end;

function TcxCustomVerticalGridAdapter.GetCellAutoHeight: Boolean;
begin
  Result := OptionsView.CellAutoHeight;
end;

function TcxCustomVerticalGridAdapter.GetCellEndEllipsis: Boolean;
begin
  Result := OptionsView.CellEndEllipsis;
end;

function TcxCustomVerticalGridAdapter.GetCellMultiline: Boolean;
begin
  Result := OptionsView.CellAutoHeight;
end;

function TcxCustomVerticalGridAdapter.GetDataController: TcxCustomDataController;
begin
  Result := CustomVerticalGrid_GetDataController(VerticalGrid);
end;

function TcxCustomVerticalGridAdapter.GetDefaultRowHeight: Integer;
begin
  Result := OptionsView.RowHeight;
end;

function TcxCustomVerticalGridAdapter.GetFirstRecordIndex: Integer;
begin
  if VerticalGrid.ViewInfo <> nil then
    Result := VerticalGrid.ViewInfo.FirstVisibleRecordIndex
  else
    Result := 0;
end;

function TcxCustomVerticalGridAdapter.GetGridLinesColor: TColor;
begin
  Result := OptionsView.GridLineColor;
end;

function TcxCustomVerticalGridAdapter.GetHeaderAvailableWidth(Row: TcxCustomRow): Integer;
begin
  Result := HeaderWidth - IndentAreas[Row];
end;

function TcxCustomVerticalGridAdapter.GetHeaderMinWidth: Integer;
begin
  Result := OptionsView.RowHeaderMinWidth;
end;

function TcxCustomVerticalGridAdapter.GetHeaderWidth: Integer;
begin
  Result := OptionsView.RowHeaderWidth;
end;

function TcxCustomVerticalGridAdapter.GetHelper(Row: TcxCustomRow): TcxVerticalGridCustomRowHelper;
begin
  Result := FHelperCache[Row];
end;

function TcxCustomVerticalGridAdapter.GetImages: TCustomImageList;
begin
  Result := VerticalGrid.Images;
end;

function TcxCustomVerticalGridAdapter.GetIndentArea(Row: TcxCustomRow): Integer;
begin
  Result := IndentCounts[Row] * IndentWidth;
end;

function TcxCustomVerticalGridAdapter.GetIndentCount(Row: TcxCustomRow): Integer;
begin
  Result := Helpers[Row].IndentCount;
end;

function TcxCustomVerticalGridAdapter.GetIndentWidth: Integer;
begin
  Result := GetImageWidth(nil, 0);
  if Result = 0 then
    Result := DefaultIndentWidth;
end;

function TcxCustomVerticalGridAdapter.GetMakeSpaceForEmptyImage: Boolean;
begin
  Result := OptionsView.ShowEmptyRowImage;
end;

function TcxCustomVerticalGridAdapter.GetOptionsView: TcxvgOptionsView;
begin
  Result := VerticalGrid.OptionsView;
end;

function TcxCustomVerticalGridAdapter.GetPaintStyle: TcxvgPaintStyle;
begin
  Result := OptionsView.PaintStyle;
end;

function TcxCustomVerticalGridAdapter.GetRecordCount: Integer;
begin
  Result := CustomVerticalGrid_GetRecordCount(VerticalGrid);
end;

function TcxCustomVerticalGridAdapter.GetRecordMinWidth(RecordIndex: Integer): Integer;
var
  I, V: Integer;
  Row: TcxCustomRow;
begin
  Result := 0;
  for I := 0 to RowCount - 1 do
  begin
    Row := Rows[I];
    if IsRowVisible(Row) then
    begin
      V := Helpers[Row].RecordMinWidth;
      if Result < V then Result := V;
    end;
  end;
end;

function TcxCustomVerticalGridAdapter.GetRecordWidth(RecordCount: Integer): Integer;
begin
  Result := TcxvgCustomViewInfoAccess(VerticalGrid.ViewInfo).ViewValueWidth + 1; //autoheight correction
end;

function TcxCustomVerticalGridAdapter.GetRow(Index: Integer): TcxCustomRow;
begin
  Result := VerticalGrid.Rows[Index];
end;

function TcxCustomVerticalGridAdapter.GetRowCount: Integer;
begin
  Result := VerticalGrid.Rows.Count;
end;

function TcxCustomVerticalGridAdapter.GetRowHeight(Row: TcxCustomRow): Integer;
begin
  Result := VerticalGrid.ViewInfo.CalcRowHeight(Row);
end;

function TcxCustomVerticalGridAdapter.GetShowHeaders: Boolean;
begin
  Result := OptionsView.ShowHeaders;
end;

function TcxCustomVerticalGridAdapter.GetShowHorzGridLines: Boolean;
begin
  Result := OptionsView.GridLines in [vglBoth, vglHorizontal];
end;

function TcxCustomVerticalGridAdapter.GetShowVertGridLines: Boolean;
begin
  Result := OptionsView.GridLines in [vglBoth, vglVertical];
end;

function TcxCustomVerticalGridAdapter.GetStyles: TcxVerticalGridStyles;
begin
  Result := VerticalGrid.Styles;
end;

function TcxCustomVerticalGridAdapter.GetValueMinWidth: Integer;
begin
  Result := OptionsView.ValueMinWidth;
end;

function TcxCustomVerticalGridAdapter.GetVerticalGrid: TcxCustomVerticalGrid;
begin
  Result := Builder.VerticalGrid;
end;

{ TcxCustomVerticalGridReportLinkFormatter }

constructor TcxCustomVerticalGridReportLinkFormatter.Create(ABuilder: TcxCustomVerticalGridReportLinkBuilder);
begin
  inherited;
  FFont := TFont.Create;
  FHeaderWidth := -1;
  FInterRecordsSpace := -1;
  FLookAndFeelItems := TList.Create;
  FRecordWidths := TList.Create;
  FRowHeights := TList.Create;
  FRowPlaces := TList.Create;
  FRows := TList.Create;
  FTransparentColor := dxPSCore.dxDefaultContentColor;
  FViewWidths := TList.Create;
  FWrappedRecordStartIndexes := TList.Create;
end;

destructor TcxCustomVerticalGridReportLinkFormatter.Destroy;
begin
  FreeAndNilRowPlaces;
  FreeAndNil(FViewWidths);
  FreeAndNil(FWrappedRecordStartIndexes);
  FreeAndNil(FRows);
  FreeAndNil(FRowHeights);
  FreeAndNil(FRecordWidths);
  FreeAndNil(FLookAndFeelItems);
  FreeAndNil(FFont);
  inherited;
end;

function TcxCustomVerticalGridReportLinkFormatter.Adapter: TcxCustomVerticalGridAdapter;
begin
  Result := Builder.Adapter;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.DoInitializeHost(AHost: TdxReportCell);
begin
  AHost.CellSides := [];
  AHost.Transparent := True;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.DoInitializeRowImage(ARow: TcxCustomRow;
  AnItem: TdxReportCellGraphic; AnIndex: Integer);
begin
  SetViewParams(AnItem, GetRowImageViewParams(ARow, AnIndex));
  if HasBackgroundBitmap(vgs_Content) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(vgs_Content);

  AnItem.DrawMode := gdmCenter;
  AnItem.ImageIndex := RowHelpers[ARow].ImageIndexes[AnIndex];
  AnItem.ImageList := Adapter.Images;
  AnItem.ImageTransparent := True;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowImageCellSides(ARow: TcxCustomRow): TdxCellSides;
begin
  Result := csTopBottom;
  Result := FixupHorzCellSides(ARow, Result);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowImageClass: TdxReportCellGraphicClass;
begin
  Result := TdxReportCellGraphic;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowImageViewParams(ARow: TcxCustomRow;
  AnIndex: Integer): TdxReportItemViewParams;
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
    ReportLink.Styles.GetHeaderParams(ARow, Result.NativeParams)
  else
    Result.NativeParams := Adapter.GetImageViewParams(ARow);

  Result.CellSides := GetRowImageCellSides(ARow);
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); // ReportLink.FixedTransparent;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.DoInitializeRowIndent(ARow: TcxCustomRow;
  AnItem: TdxReportCellExpandButton; AnIndex, AnIndentCount: Integer);
var
  BackgroundBitmapIndex: Integer;
begin
  SetViewParams(AnItem, GetRowIndentViewParams(ARow, AnIndex, AnIndentCount));
  BackgroundBitmapIndex := Adapter.GetRowIndentBackgroundBitmapIndex(ARow, AnIndentCount - AnIndex - 1);
  if HasBackgroundBitmap(BackgroundBitmapIndex) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(BackgroundBitmapIndex);

  AnItem.Data := MakeCustomDrawCodeData(cxVerticalGridRowIndentID, AnIndex, 0);
  AnItem.ShowButton := (AnIndex = AnIndentCount - 1) and OptionsView.ExpandButtons and ARow.HasChildren;
  if AnItem.ShowButton then
    DoInitializeExpandButton(ARow, AnItem, AnIndex, AnIndentCount);
end;

procedure TcxCustomVerticalGridReportLinkFormatter.DoInitializeExpandButton(
  ARow: TcxCustomRow; AnItem: TdxReportCellExpandButton; AnIndex, AnIndentCount: Integer);
begin
  AnItem.ButtonBorder3D := ReportLink.Effects3D;
  AnItem.ButtonBorder3DSoft := ReportLink.Soft3D;
  AnItem.ButtonExpanded := ARow.Expanded;
  AnItem.ButtonInteriorColor := ExpandButtonColors[ARow];
  AnItem.ButtonSize := ExpandButtonSize;
  AnItem.ButtonTransparent := False;//IsColorTransparent(ButtonInteriorColor);
end;

procedure TcxCustomVerticalGridReportLinkFormatter.DoReportLinkInitializeRowIndent(ARow: TcxCustomRow;
  AnItem: TdxReportCellExpandButton; AnIndex: Integer);
begin
  ReportLink.DoInitializeRowIndentCell(ARow, AnIndex, AnItem);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowIndentCellSides(ARow: TcxCustomRow;
  AnIndex, AnIndentCount: Integer): TdxCellSides;
var
  Level: Integer;
  NextRow: TcxCustomRow;
begin
  if Adapter.PaintStyle = psdotNet then
  begin
    Result := [];
    Level := AnIndentCount - AnIndex - 1;

    if (ARow is TcxCategoryRow) and ((AnIndex = AnIndentCount - 1) or
      ((AnIndex = AnIndentCount - 2) and not Adapter.HasCategoryRowAsParentAtLevel(ARow, Level, False))) then
      Include(Result, csTop);
    if not (ARow is TcxCategoryRow) and not Adapter.HasCategoryRowAsParentAtLevel(ARow, Level, True) then
      Result := Result + csTopBottom;
    if not (ARow is TcxCategoryRow) and Adapter.IsRowLastChildAtLevel(ARow, Level) and
      Adapter.HasCategoryRowAsParentAtLevel(ARow, Level, False) then
      Include(Result, csBottom);
    if IsLastRow(ARow) then
      Include(Result, csBottom);

    if (AnIndex = 0) or ((ARow is TcxCategoryRow) and (AnIndex = AnIndentCount - 1)) then
      Include(Result, csLeft);
    if (AnIndex <> AnIndentCount - 1) and Adapter.HasCategoryRowAsParentAtLevel(ARow, Level, False) then
      Result := Result + csLeftRight;
    if AnIndex = AnIndentCount - 1 then
    begin
      if (ARow is TcxCategoryRow) and ARow.HasChildren then
      begin
        NextRow := GetNextRow(ARow);
        if (NextRow <> nil) and (NextRow.Level > ARow.Level) then
          Result := Result - [csBottom, csRight];
      end;
      if not (ARow is TcxCategoryRow) then
        Include(Result, csLeft);
    end;
  end
  else
  begin
    Result := csTopBottom;
    if AnIndex = 0 then Include(Result, csLeft);
  end;

  Result := FixupHorzCellSides(ARow, Result);
  if not ShowVertGridLines then
  begin
    Exclude(Result, csRight);
    if AnIndex <> 0 then Exclude(Result, csLeft);
  end;
  if (AnIndex = 0) and not ShowBorders then
    Exclude(Result, csLeft);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowIndentClass: TdxReportCellExpandButtonClass;
begin
  Result := TdxReportCellExpandButton;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowIndentViewParams(ARow: TcxCustomRow;
  AnIndex, AnIndentCount: Integer): TdxReportItemViewParams;
var
  ParentRow: TcxCustomRow;
begin
  ParentRow := Adapter.GetRowParentAtLevel(ARow, AnIndentCount - AnIndex - 1);
  if ReportLink.OptionsFormatting.UseNativeStyles then
    ReportLink.Styles.GetIndentParams(ParentRow, Result.NativeParams)
  else
    Result.NativeParams := Adapter.GetIndentViewParams(ParentRow);

  Result.CellSides := GetRowIndentCellSides(ARow, AnIndex, AnIndentCount);
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); //ReportLink.FixedTransparent
end;

procedure TcxCustomVerticalGridReportLinkFormatter.DoInitializeCategoryRow(ARow: TcxCategoryRow;
  AnItem: TdxReportCellImage);
var
  AImageItem: TdxReportCellImage;
  Helper: TcxVerticalGridCustomRowHelper;
begin
  SetViewParams(AnItem, GetCategoryRowViewParams(ARow));
  if HasBackgroundBitmap(vgs_Category) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(vgs_Category);
  Helper := RowHelpers[ARow];
  AImageItem := AnItem as TdxReportCellImage;
  AImageItem.Data := MakeCustomDrawCodeData(cxVerticalGridCategoryRowID, 0, 0);
  AImageItem.EndEllipsis := Adapter.CellEndEllipsis and not ReportLink.OptionsSize.BestFit;
  if Helper.HasHeaderImages[0] then
  begin
    AImageItem.ImageIndex := Helper.ImageIndexes[0];
    AImageItem.ImageList := Adapter.Images;
    AImageItem.ImageLayout := ilImageCenterLeft;
  end;
  AImageItem.Multiline := False;
  AImageItem.Text := Helper.HeaderCaptions[0];
  AImageItem.TextAlignX := Helper.HeaderAlignmentHorzs[0];
  AImageItem.TextAlignY := Helper.HeaderAlignmentVerts[0];
end;

procedure TcxCustomVerticalGridReportLinkFormatter.DoInitializeRowReportRow(ARow: TcxCustomRow;
 AReportRow: TdxReportCell);
begin
  AReportRow.CellSides := [];
  AReportRow.Data := Integer(ARow);
  AReportRow.Transparent := True;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.DoInitializeRowHeader(ARow: TcxCustomRow;
  AnItem: TdxReportCellImage; AnIndex: Integer);
var
  AImageItem: TdxReportCellImage;
  Helper: TcxVerticalGridCustomRowHelper;
begin
  SetViewParams(AnItem, GetRowHeaderViewParams(ARow, AnIndex));
  if HasBackgroundBitmap(vgs_Header) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(vgs_Header);

  Helper := RowHelpers[ARow];
  AImageItem := AnItem as TdxReportCellImage;
  AImageItem.Data := MakeCustomDrawCodeData(cxVerticalGridRowHeaderID, AnIndex, 0);
  AImageItem.EndEllipsis := Adapter.CellEndEllipsis and not ReportLink.OptionsSize.BestFit;
  AImageItem.ImageIndex := Helper.ImageIndexes[AnIndex];
  AImageItem.ImageList := Adapter.Images;
  AImageItem.ImageLayout := ilImageCenterLeft;
  AImageItem.MakeSpaceForEmptyImage := Adapter.MakeSpaceForEmptyImage;
  AImageItem.Multiline := False;
  AImageItem.Text := Helper.HeaderCaptions[AnIndex];
  AImageItem.TextAlignX := Helper.HeaderAlignmentHorzs[AnIndex];
  AImageItem.TextAlignY := Helper.HeaderAlignmentVerts[AnIndex];
end;

procedure TcxCustomVerticalGridReportLinkFormatter.DoInitializeRowValue(ARow: TcxCustomRow;
  AnItem: TAbstractdxReportCellData; AnIndex, ARecordIndex: Integer);
var
  Properties: TcxCustomEditProperties;
  ViewParams: TdxReportItemViewParams;
  CellValue: TcxEditValue;
begin
  Properties := Adapter.GetProperties(ARow, AnIndex, ARecordIndex);
  ViewParams := GetRowValueViewParams(ARow, AnIndex, ARecordIndex);
  CellValue := GetCellValue(ARow, Properties, AnIndex, ARecordIndex);
  dxPScxCommon.dxPSDataMaps.InitializeItem(AnItem, Properties, CellValue, Self, ViewParams, False, ARecordIndex,
    TcxCustomRowAccess(ARow).GetEditContainer(AnIndex));
  dxPScxCommon.dxPSDataMaps.GetImageLists(Properties, ReportLink.AppendImageList);

  SetViewParams(AnItem, ViewParams);
  if HasBackgroundBitmap(vgs_Content) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(vgs_Content);

  AnItem.Data := MakeCustomDrawCodeData(cxVerticalGridRowValueID, AnIndex, ARecordIndex);
end;

procedure TcxCustomVerticalGridReportLinkFormatter.DoReportLinkInitializeRowHeader(ARow: TcxCustomRow;
  AnItem: TdxReportCellImage; AnIndex: Integer);
begin
  ReportLink.DoInitializeRowHeaderCell(ARow, AnIndex, AnItem);
end;

procedure TcxCustomVerticalGridReportLinkFormatter.DoReportLinkInitializeRowValue(ARow: TcxCustomRow;
  AnItem: TAbstractdxReportCellData; AnIndex, ARecordIndex: Integer);
begin
  ReportLink.DoInitializeRowValueCell(ARow, AnIndex, ARecordIndex, AnItem);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetCategoryRowCellSides(ARow: TcxCustomRow): TdxCellSides;
begin
  Result := [csTop, csRight, csBottom];
  if not ShowHeaders then
    Include(Result, csLeft);
  if not ShowBorders then
    Exclude(Result, csRight);
  Result := FixupHorzCellSides(ARow, Result);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetCategoryRowClass(ARow: TcxCustomRow): TdxReportCellImageClass;
begin
  Result := TdxReportCellImage;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetCategoryRowViewParams(ARow: TcxCustomRow): TdxReportItemViewParams;
begin
  FillChar(Result, 0, SizeOf(Result));
  if ReportLink.OptionsFormatting.UseNativeStyles then
    ReportLink.Styles.GetCategoryParams(ARow, Result.NativeParams)
  else
    Result.NativeParams := Adapter.GetCategoryViewParams(ARow);

  Result.CellSides := GetCategoryRowCellSides(ARow);
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); //ReportLink.Transparent;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetCellValue(ARow: TcxCustomRow;
  AProperties: TcxCustomEditProperties; AnIndex, ARecordIndex: Integer): TcxEditValue;
var
  Helper: TcxVerticalGridCustomRowHelper;
begin
  Helper := RowHelpers[ARow];
  if AProperties.GetEditValueSource(False) = evsValue then
    Result := Helper.Values[AnIndex, ARecordIndex]
  else
    Result := Helper.DisplayTexts[AnIndex, ARecordIndex];
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowHeaderCellSides(ARow: TcxCustomRow;
  AnIndex: Integer): TdxCellSides;
var
  Helper: TcxVerticalGridCustomRowHelper;
begin
  Helper := RowHelpers[ARow];

  Result := csAll;
  if Helper.HasHeaderSeparators and (Helper.ItemCount <> 1) then
  begin
    if AnIndex <> 0 then
      Exclude(Result, csLeft);
    if AnIndex <> Helper.ItemCount - 1 then
      Exclude(Result, csRight);
  end;
  if AnIndex = 0 then
    Exclude(Result, csLeft);
  if not ShowVertGridLines then
    Result := Result - csLeftRight;
  Result := FixupHorzCellSides(ARow, Result);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowHeaderClass(ARow: TcxCustomRow): TdxReportCellImageClass;
begin
  Result := TdxReportCellImage;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowHeaderViewParams(ARow: TcxCustomRow;
  AnIndex: Integer): TdxReportItemViewParams;
begin
  FillChar(Result, 0, SizeOf(Result));
  if ReportLink.OptionsFormatting.UseNativeStyles then
    ReportLink.Styles.GetHeaderParams(ARow, Result.NativeParams)
  else
    Result.NativeParams := Adapter.GetHeaderViewParams(ARow);

  Result.CellSides := GetRowHeaderCellSides(ARow, AnIndex);
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); //ReportLink.Transparent;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowValueCellSides(ARow: TcxCustomRow;
  AnIndex, ARecordIndex: Integer): TdxCellSides;
var
  Helper: TcxVerticalGridCustomRowHelper;
begin
  Helper := RowHelpers[ARow];

  Result := csAll;
  if Helper.HasValueSeparators and (Helper.ItemCount <> 1) then
  begin
    if AnIndex <> 0 then
      Exclude(Result, csLeft);
    if AnIndex <> Helper.ItemCount - 1 then
      Exclude(Result, csRight);
  end;
  if not ShowVertGridLines then
  begin
    Exclude(Result, csLeft);
    if AnIndex <> Helper.ItemCount - 1 then
      Exclude(Result, csRight);
  end;
  if not ShowBorders and (AnIndex = Helper.ItemCount - 1) then
    Exclude(Result, csRight);
  Result := FixupHorzCellSides(ARow, Result);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowValueClass(
  ARow: TcxCustomRow; AIndex, ARecordIndex: Integer): TdxReportCellDataClass;
begin
  Result := dxPScxCommon.dxPSDataMaps.ItemClass(
    Adapter.GetProperties(ARow, AIndex, ARecordIndex),
    GetRowValueViewParams(ARow, AIndex, ARecordIndex));
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowValueViewParams(
  ARow: TcxCustomRow; AIndex, ARecordIndex: Integer): TdxReportItemViewParams;
var
  AStyle: TdxSpreadSheetCellDisplayStyle;
begin
  FillChar(Result, 0, SizeOf(Result));
  if ReportLink.OptionsFormatting.UseNativeStyles then
    ReportLink.Styles.GetContentParams(ARow, AIndex, ARecordIndex, Result.NativeParams)
  else
    Result.NativeParams := Adapter.GetContentViewParams(ARow, AIndex, ARecordIndex);

  Result.CellSides := GetRowValueCellSides(ARow, AIndex, ARecordIndex);
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color);

  if Adapter.TryGetAdvancedStyle(ARow, AIndex, ARecordIndex, AStyle) and (AStyle <> nil) then
    Result.AdvancedViewParams := TdxSpreadSheetAdvancedViewParams.CreateFrom(AStyle);
end;

procedure TcxCustomVerticalGridReportLinkFormatter.DoInitializeRowHeaderSeparator(
  ARow: TcxCustomMultiEditorRow; AnItem: TdxReportCellString; AnIndex: Integer);
begin
  SetViewParams(AnItem, GetRowHeaderSeparatorViewParams(ARow, AnIndex));
  if HasBackgroundBitmap(vgs_Header) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(vgs_Header);

  with TdxReportCellString(AnItem) do
  begin
    Data := MakeCustomDrawCodeData(cxVerticalGridRowHeaderSeparatorID, AnIndex, 0);
    EndEllipsis := False;
    Multiline := False;
    TextAlignX := taLeft;
    TextAlignY := dxPScxCommon.TextAlignYMap[RowHelpers[ARow].SeparatorsAlignmentVert];
  end;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.DoInitializeRowValueSeparator(ARow: TcxCustomMultiEditorRow;
  AnItem: TdxReportCellString; AnIndex, ARecordIndex: Integer);
begin
  SetViewParams(AnItem, GetRowValueSeparatorViewParams(ARow, AnIndex, ARecordIndex));
  if HasBackgroundBitmap(vgs_Content) then
    AnItem.BackgroundBitmapIndex := GetBackgroundBitmapIndex(vgs_Content);

  with TdxReportCellString(AnItem) do
  begin
    Data := MakeCustomDrawCodeData(cxVerticalGridRowValueSeparatorID, AnIndex, ARecordIndex);
    EndEllipsis := False;
    Multiline := False;
    TextAlignX := taLeft;
    TextAlignY := dxPScxCommon.TextAlignYMap[RowHelpers[ARow].SeparatorsAlignmentVert];
  end;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.DoReportLinkInitializeRowHeaderSeparator(ARow: TcxCustomMultiEditorRow;
  AnItem: TdxReportCellString; AnIndex: Integer);
begin
  ReportLink.DoInitializeRowHeaderSeparatorCell(ARow, AnIndex, AnItem);
end;

procedure TcxCustomVerticalGridReportLinkFormatter.DoReportLinkInitializeRowValueSeparator(ARow: TcxCustomMultiEditorRow;
  AnItem: TdxReportCellString; AnIndex, ARecordIndex: Integer);
begin
  ReportLink.DoInitializeRowValueSeparatorCell(ARow, AnIndex, ARecordIndex, AnItem);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowHeaderSeparatorCellSides(ARow: TcxCustomMultiEditorRow): TdxCellSides;
begin
  Result := FixupHorzCellSides(ARow, csTopBottom);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowHeaderSeparatorClass(ARow: TcxCustomMultiEditorRow): TdxReportCellStringClass;
begin
  Result := TdxReportCellString;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowHeaderSeparatorViewParams(ARow: TcxCustomMultiEditorRow;
  AnIndex: Integer): TdxReportItemViewParams;
begin
  FillChar(Result, 0, SizeOf(Result));
  if ReportLink.OptionsFormatting.UseNativeStyles then
    ReportLink.Styles.GetHeaderParams(ARow, Result.NativeParams)
  else
    Result.NativeParams := Adapter.GetHeaderViewParams(ARow);

  Result.CellSides := GetRowHeaderSeparatorCellSides(ARow);
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); //ReportLink.Transparent;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowValueSeparatorCellSides(ARow: TcxCustomMultiEditorRow): TdxCellSides;
begin
  Result := FixupHorzCellSides(ARow, csTopBottom);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowValueSeparatorClass(ARow: TcxCustomMultiEditorRow): TdxReportCellStringClass;
begin
  Result := TdxReportCellString;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowValueSeparatorViewParams(ARow: TcxCustomMultiEditorRow;
  AnIndex, ARecordIndex: Integer): TdxReportItemViewParams;
begin
  FillChar(Result, 0, SizeOf(Result));
  if ReportLink.OptionsFormatting.UseNativeStyles then
    ReportLink.Styles.GetContentParams(ARow, AnIndex, ARecordIndex, Result.NativeParams)
  else
    Result.NativeParams := Adapter.GetContentViewParams(ARow, AnIndex, ARecordIndex);

  Result.CellSides := GetRowValueSeparatorCellSides(ARow);
  Result.FontStyle := [];
  Result.Transparent := IsColorTransparent(Result.NativeParams.Color); //ReportLink.Transparent;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.DoInitializeWrapSeparatorItem(AnItem: TdxReportCellString;
  AWrapIndex: Integer);
begin
  AnItem.CellSides := [];
  AnItem.Data := AWrapIndex;
  AnItem.Transparent := True;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.DoInitializeWrapSeparatorRow(ARow: TdxReportCell);
begin
  ARow.CellSides := [];
  ARow.Transparent := True;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetWrapSeparatorClass: TdxReportCellStringClass;
begin
  Result := TdxReportCellString;
end;

function TcxCustomVerticalGridReportLinkFormatter.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TcxCustomVerticalGridReportLinkFormatter._AddRef: Integer;
begin
  Result := 1;
end;

function TcxCustomVerticalGridReportLinkFormatter._Release: Integer;
begin
  Result := 1;
end;

function TcxCustomVerticalGridReportLinkFormatter.IdxPSCellParams_GetAutoHeight: Boolean;
begin
  Result := Adapter.CellAutoHeight;
end;

function TcxCustomVerticalGridReportLinkFormatter.IdxPSCellParams_GetCanvas: TdxPSReportRenderCustomCanvas;
begin
  Result := ScreenCanvas;
end;

function TcxCustomVerticalGridReportLinkFormatter.IdxPSCellParams_GetDisplayGraphicsAsText: Boolean;
begin
  Result := ReportLink.OptionsRefinements.DisplayGraphicsAsText;
end;

function TcxCustomVerticalGridReportLinkFormatter.IdxPSCellParams_GetDisplayTrackBarsAsText: Boolean;
begin
  Result := ReportLink.OptionsRefinements.DisplayTrackBarsAsText;
end;

function TcxCustomVerticalGridReportLinkFormatter.IdxPSCellParams_GetEndEllipsis: Boolean;
begin
  Result := Adapter.CellEndEllipsis and not ReportLink.OptionsSize.BestFit;
end;

function TcxCustomVerticalGridReportLinkFormatter.IdxPSCellParams_GetFlatCheckMarks: Boolean;
begin
  Result := ReportLink.OptionsRefinements.FlatCheckMarks;
end;

function TcxCustomVerticalGridReportLinkFormatter.IdxPSCellParams_GetGraphicsText: string;
begin
  Result := ReportLink.OptionsRefinements.GraphicsText;
end;

function TcxCustomVerticalGridReportLinkFormatter.IdxPSCellParams_GetMultiline: Boolean;
begin
  Result := Adapter.CellMultiline;
end;

function TcxCustomVerticalGridReportLinkFormatter.IdxPSCellParams_GetTransparentGraphics: Boolean;
begin
  Result := ReportLink.OptionsRefinements.TransparentGraphics;
end;

function TcxCustomVerticalGridReportLinkFormatter.IdxPSCellParams2_GetPreviewMarginLeft: Integer;
begin
  Result := 2;
end;

function TcxCustomVerticalGridReportLinkFormatter.IdxPSCellParams2_GetPreviewMarginRight: Integer;
begin
  Result := 2;
end;

function TcxCustomVerticalGridReportLinkFormatter.IdxPSCellParams2_GetPreviewMaxHeight: Integer;
begin
  Result := -1;
end;

function TcxCustomVerticalGridReportLinkFormatter.IdxPSCellParams2_GetPreviewMaxLineCount: Integer;
begin
  Result := -1;
end;

function TcxCustomVerticalGridReportLinkFormatter.IdxPSCellParams2_GetRichEditGraphicClass: TGraphicClass;
const
  GraphicClasses: array[Boolean] of TGraphicClass = (TMetafile, TBitmap);
begin
  Result := GraphicClasses[IdxPSCellParams2_GetRichEditTransparent];
end;

function TcxCustomVerticalGridReportLinkFormatter.IdxPSCellParams2_GetRichEditTransparent: Boolean;
begin
  Result := ReportLink.OptionsRefinements.TransparentRichEdits;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.AddDelimiters;
begin
  AddHorizontalDelimiters;
  AddVerticalDelimiters;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.AddHorizontalDelimiters;
var
  I, J: Integer;
begin
  for I := 0 to WrapCount - 1 do
    for J := 0 to RowCount - 1 do
      RowPlaces[I, J].AddDelimiters(ReportLink);
end;

procedure TcxCustomVerticalGridReportLinkFormatter.AddVerticalDelimiters;
var
  I: Integer;
  Row: TdxReportCell;
begin
  for I := 0 to Builder.ReportRowCount - 1 do
  begin
    Row := Builder.ReportRows[I];
    ReportLink.AddVerticalDelimiter(Row);
    if ReportLink.OptionsPagination.ByWrapping and IsLastRow(GetRowByReportRow(Row)) then
      ReportLink.AddVerticalHardDelimiter(Row);
  end;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.AfterBuilding;
begin
  if not Builder.IsAborted then
  begin
    FormatLookAndFeelItems;
    AddDelimiters;
    Builder.ReportCells.BorderColor := GridLinesColor;
  end;

  if AreRecordsNeededUnloading then
    UnloadRecords;
  FIsPrevGridMode := False;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.BeforeBuilding;
begin
  FIsPrevGridMode := DataController.IsGridMode;
  if AreRecordsNeededLoading then
    LoadRecords;
  if ReportLink.OptionsExpanding.AutoExpandRows then
    Adapter.FullExpand;
  CreateRowList;
  InitializeWrappedRecordIndexes;
  InitializeRecordWidths;
  InitializeRowHeights;
  Calculate;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetAreRecordsNeededLoading: Boolean;
begin
  Result := False;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetAreRecordsNeededUnloading: Boolean;
begin
  Result := False;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetFirstInternalRecordIndex: Integer;
begin
  Result := 0;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetInternalRecordCount: Integer;
begin
  Result := Adapter.RecordCount;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.LoadRecords;
begin
end;

procedure TcxCustomVerticalGridReportLinkFormatter.UnloadRecords;
begin
end;

procedure TcxCustomVerticalGridReportLinkFormatter.AddWrappedRecordStartIndex(Value: Integer);
begin
  FWrappedRecordStartIndexes.Add(TObject(Value));
end;

procedure TcxCustomVerticalGridReportLinkFormatter.Calculate;
begin
  CreateRowPlaces;

  if ReportLink.OptionsSize.BestFit and not ReportLink.OptionsSize.AutoWidth then
    CalculateBestFit;
  if ReportLink.OptionsSize.WrapRecords and (InternalRecordCount > 1) then
  begin
    CalculateRecordCounts;
    CreateRowPlaces;
  end;
  if ReportLink.OptionsSize.AutoWidth then
    CalculateRecordAutoWidths;

  CalculateViewWidths;
  CalculateRowHeights;
  CalculateRowPlaces;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.CalculateBestFit;
var
  ARecordMinWidth, ARecordMaxWidth: Integer;
  I, J: Integer;
begin
  ARecordMinWidth := CalculatedMinWidth;

  if ShowHeaders then
  begin
    for I := 0 to RowCount - 1 do
      HeaderWidth := Max(HeaderWidth, RowPlaces[0, I].CalculatedHeaderWidth);
    Dec(ARecordMinWidth, HeaderWidth);
  end;

  ARecordMaxWidth := 0;
  for I := WrappedRecordStartIndexes[0] to WrappedRecordStopIndexes[WrapCount - 1] do
  begin
    for J := 0 to RowCount - 1 do
      RecordWidths[I] := Max(RecordWidths[I], RowPlaces[0, J].CalculatedRecordWidth(I));
    RecordWidths[I] := Max(RecordWidths[I], ARecordMinWidth);
    ARecordMaxWidth := Max(RecordWidths[I], ARecordMaxWidth);
  end;

  if ReportLink.OptionsSize.KeepSameRecordWidths then
  begin
    for I := WrappedRecordStartIndexes[0] to WrappedRecordStopIndexes[WrapCount - 1] do
      RecordWidths[I] := ARecordMaxWidth;
  end;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.CalculateHeight(
  const AParams: TdxReportItemViewParams; var AHeight: Integer);
begin
  AHeight := Max(AHeight,
    Renderer.CalcTextPatternHeight(ScreenCanvas, AParams.NativeParams.Font));
end;

function TcxCustomVerticalGridReportLinkFormatter.CalculatedMinWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to RowCount - 1 do
    Result := Max(Result, RowPlaces[0, I].CalculatedMinWidth);
end;

procedure TcxCustomVerticalGridReportLinkFormatter.CalculateRecordCounts;
var
  V, RecordIndex, RecordCount: Integer;
begin
  V := Builder.AvailableSiteWidth - HeaderWidth;
  if V > 0 then
  begin
    RecordIndex := 0;
    repeat
      RecordCount := 0;
      while (V > 0) and (RecordIndex < InternalRecordCount) do
      begin
        Dec(V, RecordWidths[RecordIndex]);
        Inc(RecordIndex);
        Inc(RecordCount);
        if V > 0 then
        begin
          Dec(V, InterRecordsSpace);
          if (V < 0) and (RecordCount <> 1) then
            Inc(RecordIndex);
        end;
      end;
      if V <= 0 then
      begin
        if RecordCount > 1 then Dec(RecordIndex);
        AddWrappedRecordStartIndex(RecordIndex);
        V := Builder.AvailableSiteWidth - HeaderWidth;
      end;
    until RecordIndex > InternalRecordCount - 1;
  end
  else
    for RecordIndex := 1 to InternalRecordCount - 1 do
      AddWrappedRecordStartIndex(RecordIndex);
  AddWrappedRecordStartIndex(InternalRecordCount);
end;

procedure TcxCustomVerticalGridReportLinkFormatter.CalculateRowHeights;
var
  I, RowHeight: Integer;
  Row: TcxCustomRow;
begin
  for I := 0 to RowCount - 1 do
  begin
    Row := Rows[I];
    RowHeight := Adapter.RowHeights[Row];
    if RowHeight = -1 then
      RowHeight := RowPlaces[0, IndexOfRow(Row)].CalculatedMinHeight;
    RowHeights[Row] := RowHeight;
  end;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.CalculateRecordAutoWidths;

  procedure AssignAutoWidthItem(AnItem: TcxAutoWidthItem; AMinWidth, AWidth: Integer);
  begin
    AnItem.MinWidth := AMinWidth;
    AnItem.Width := AWidth;
  end;

  function GetAvailableSiteWidth(ARecordCount: Integer): Integer;
  begin
    Result := Builder.AvailableSiteWidth - InterRecordsSpace * (ARecordCount - 1) - HeaderWidth;
  end;

var
  I, J: Integer;
  AutoWidthObject: TcxAutoWidthObject;
begin
  if InternalRecordCount <> 1 then
    for I := 0 to WrapCount - 1 do
    begin
      AutoWidthObject := TcxAutoWidthObject.Create(RecordCounts[I]);
      try
        for J := WrappedRecordStartIndexes[I] to WrappedRecordStopIndexes[I] do
          AssignAutoWidthItem(AutoWidthObject.AddItem, RecordMinWidths[J], RecordWidths[J]);

        AutoWidthObject.AvailableWidth := GetAvailableSiteWidth(RecordCounts[I]);
        AutoWidthObject.Calculate;

        for J := WrappedRecordStartIndexes[I] to WrappedRecordStopIndexes[I] do
          RecordWidths[J] := AutoWidthObject[J - WrappedRecordStartIndexes[I]].AutoWidth;
      finally
        AutoWidthObject.Free;
      end;
    end
  else
    for I := WrappedRecordStartIndexes[0] to WrappedRecordStopIndexes[WrapCount - 1] do // v3.03 {0 to InternalRecordCount - 1}
      RecordWidths[I] := GetAvailableSiteWidth(InternalRecordCount);
end;

procedure TcxCustomVerticalGridReportLinkFormatter.CalculateRowPlaces;
var
  I, J: Integer;
begin
  for I := 0 to WrapCount - 1 do
    for J := 0 to RowCount - 1 do
      RowPlaces[I, J].Calculate;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.CalculateViewWidths;
var
  I, J, V: Integer;
begin
  FViewWidths.Count := WrapCount;
  for I := 0 to WrapCount - 1 do
  begin
    V := HeaderWidth;
    for J := WrappedRecordStartIndexes[I] to WrappedRecordStopIndexes[I] do
    begin
      Inc(V, RecordWidths[J]);
      if J < WrappedRecordStopIndexes[I] then
        Inc(V, InterRecordsSpace);
    end;
    ViewWidths[I] := V;
  end;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.CreateRowList;
var
  I: Integer;
  Row: TcxCustomRow;
begin
  FRows.Clear;
  for I := 0 to Adapter.RowCount - 1 do
  begin
    Row := Adapter.Rows[I];
    if Adapter.IsRowVisible(Row) then FRows.Add(Row);
  end;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.CreateRowPlaces;
var
  I, J: Integer;
  Place: TcxVerticalGridCustomRowPlace;
begin
  ClearRowPlaces;
  for I := 0 to WrapCount - 1 do
    for J := 0 to RowCount - 1 do
    begin
      Place := CreateRowPlace(Rows[J], I);
      FRowPlaces.Add(Place);
    end;
end;

function TcxCustomVerticalGridReportLinkFormatter.CreateRowPlace(ARow: TcxCustomRow;
  AWrapIndex: Integer): TcxVerticalGridCustomRowPlace;
begin
  Result := RowHelpers[ARow].RowPlaceClass.Create(Self, ARow, AWrapIndex);
end;

procedure TcxCustomVerticalGridReportLinkFormatter.InitializeRecordWidths;
var
  I: Integer;
begin
  FRecordWidths.Count := InternalRecordCount;
  for I := WrappedRecordStartIndexes[0] to WrappedRecordStopIndexes[WrapCount - 1] do // v3.03 {0 to InternalRecordCount - 1}
    RecordWidths[I] := -1;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.InitializeRowHeights;
var
  I: Integer;
begin
  FRowHeights.Count := RowCount;
  for I := 0 to RowCount - 1 do
    RowHeights[Rows[I]] := -1;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.InitializeWrappedRecordIndexes;
begin
  AddWrappedRecordStartIndex(FirstInternalRecordIndex);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetBackgroundBitmap(Index: Integer): TBitmap;
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
    Result := ReportLink.Styles.GetBitmap(MapStyleBackgroundBitmapIndex(Index))
  else
    Result := Adapter.GetBackgroundBitmap(Index);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetBackgroundBitmapIndex(Index: Integer): Integer;
begin
  Result := ReportLink.AddBackgroundBitmapToPool(GetBackgroundBitmap(Index));
end;

function TcxCustomVerticalGridReportLinkFormatter.HasBackgroundBitmap(Index: Integer): Boolean;
var
  Bitmap: TBitmap;
begin
  if not ReportLink.OptionsFormatting.SuppressBackgroundBitmaps then
  begin
    Bitmap := GetBackgroundBitmap(Index);
    Result := (Bitmap <> nil) and not Bitmap.Empty;
  end
  else
    Result := False;
end;

function TcxCustomVerticalGridReportLinkFormatter.MapStyleBackgroundBitmapIndex(AVerticalGridBackgroundBitmapIndex: Integer): Integer;
begin
  case AVerticalGridBackgroundBitmapIndex of
    vgs_Category:
      Result := vspsVGridCategory;
    vgs_Content:
      Result := vspsVGridContent;
    vgs_Header:
      Result := vspsVGridHeader;
  else
    Result := 0;
  end;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetCategoryRowFont(ARow: TcxCustomRow): TFont;
begin
  Result := GetCategoryRowViewParams(ARow).NativeParams.Font;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowHeaderFont(ARow: TcxCustomRow;
  AnIndex: Integer): TFont;
begin
  Result := GetRowHeaderViewParams(ARow, AnIndex).NativeParams.Font;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowHeaderSeparatorsFont(ARow: TcxCustomRow;
  AnIndex: Integer): TFont;
begin
  Result := GetRowHeaderViewParams(ARow, AnIndex).NativeParams.Font;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowValueFont(ARow: TcxCustomRow;
  AnIndex, ARecordIndex: Integer): TFont;
begin
  Result := GetRowValueViewParams(ARow, AnIndex, ARecordIndex).NativeParams.Font;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowValueSeparatorsFont(ARow: TcxCustomRow;
  AnIndex, ARecordIndex: Integer): TFont;
begin
  Result := GetRowValueViewParams(ARow, AnIndex, ARecordIndex).NativeParams.Font;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.ClearRowPlaces;
var
  I: Integer;
begin
  for I := 0 to FRowPlaces.Count - 1 do
    TObject(FRowPlaces[I]).Free;
  FRowPlaces.Clear;
end;

function TcxCustomVerticalGridReportLinkFormatter.FixupHorzCellSides(ARow: TcxCustomRow;
  ACellSides: TdxCellSides): TdxCellSides;
begin
  Result := ACellSides;
  if not ShowHorzGridLines then
  begin
    if not IsFirstRow(ARow) then Exclude(Result, csTop);
    if not IsLastRow(ARow) then Exclude(Result, csBottom);
  end;
  if not ShowBorders then
  begin
    if IsFirstRow(ARow) then Exclude(Result, csTop);
    if IsLastRow(ARow) then Exclude(Result, csBottom);
  end;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.FreeAndNilRowPlaces;
begin
  ClearRowPlaces;
  FreeAndNil(FRowPlaces);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetImageHeight(ARow: TcxCustomRow;
  ACellIndex: Integer): Integer;
begin
  Result := Adapter.GetImageHeight(ARow, ACellIndex);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetImageWidth(ARow: TcxCustomRow;
  ACellIndex: Integer): Integer;
begin
  Result := Adapter.GetImageWidth(ARow, ACellIndex);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetNextRow(ARow: TcxCustomRow): TcxCustomRow;
var
  Index: Integer;
begin
  Index := IndexOfRow(ARow);
  if Index < RowCount - 1 then
    Result := Rows[Index + 1]
  else
    Result := nil;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowByReportRow(ARow: TdxReportCell): TcxCustomRow;
begin
  Result := TcxCustomRow(ARow.Cells[0].Data);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetPrevRow(ARow: TcxCustomRow): TcxCustomRow;
var
  Index: Integer;
begin
  Index := IndexOfRow(ARow);
  if Index > 0 then
    Result := Rows[Index - 1]
  else
    Result := nil;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetStyleFontIndex(const AParams: TdxReportItemViewParams): Integer;
begin
  if AParams.NativeParams.Font <> nil then
  begin
    FFont.Assign(AParams.NativeParams.Font);
    if not dxPSUtl.dxIsTrueTypeFont(FFont) then
      FFont.Name := ReportLink.Font.Name;
    FFont.Color := ColorToRGB(AParams.NativeParams.TextColor);
    FFont.Style := FFont.Style + AParams.FontStyle;
    Result := ReportLink.AddFontToPool(FFont);
  end
  else
    Result := 0;
end;

function TcxCustomVerticalGridReportLinkFormatter.IndexOfRow(ARow: TcxCustomRow): Integer;
begin
  Result := FRows.IndexOf(ARow);
end;

function TcxCustomVerticalGridReportLinkFormatter.IsColorTransparent(AColor: TColor): Boolean;
begin
  Result := ColorToRGB(AColor) = ColorToRGB(TransparentColor);
end;

function TcxCustomVerticalGridReportLinkFormatter.IsFirstRow(ARow: TcxCustomRow): Boolean;
begin
  Result := ARow = FRows.First;
end;

function TcxCustomVerticalGridReportLinkFormatter.IsLastRow(ARow: TcxCustomRow): Boolean;
begin
  Result := ARow = FRows.Last;
end;

function TcxCustomVerticalGridReportLinkFormatter.MakeCustomDrawCodeData(AnAttribute: TcxVerticalGridAttributeID;
  ACellIndex, ARecordIndex: Integer): Integer;
begin
  Result := ReportLink.MakeCustomDrawCodeData(AnAttribute, ACellIndex, ARecordIndex);
end;

procedure TcxCustomVerticalGridReportLinkFormatter.RegisterLookAndFeelItem(AnItem: TdxReportVisualItem;
  AEdgeStyle: TdxCellEdgeStyle);
begin
  AnItem.EdgeMode := cem3DEffects;
  AnItem.Edge3DStyle := AEdgeStyle;
  FLookAndFeelItems.Add(AnItem);
end;

procedure TcxCustomVerticalGridReportLinkFormatter.SetViewParams(AnItem: TdxReportVisualItem;
  const AParams: TdxReportItemViewParams);
begin
  with AnItem do
  begin
    CellSides := AParams.CellSides;
    Color := ColorToRGB(AParams.NativeParams.Color);
    FontIndex := GetStyleFontIndex(AParams);
    Transparent := AParams.Transparent;
  end;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetAutoWidth: Boolean;
begin
  Result := ReportLink.OptionsSize.AutoWidth;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetDataController: TcxCustomDataController;
begin
  Result := Adapter.DataController;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetScreenCanvas: TdxPSReportRenderCustomCanvas;
begin
  Result := ReportLink.ScreenCanvas;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetExpandButtonColor(Row: TcxCustomRow): TColor;
begin
  if Row is TcxCategoryRow then
    Result := GetCategoryRowViewParams(Row).NativeParams.Color
  else
    Result := ExpandButtonInteriorColor;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetExpandButtonSize: Integer;
begin
  Result := DefaultExpandButtonSize;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetGridLinesColor: TColor;
begin
  Result := ReportLink.OptionsFormatting.GridLineColor;
  if Result = clDefault then
    Result := Adapter.GridLinesColor;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetHeaderAvailableWidth(Row: TcxCustomRow): Integer;
begin
  if ShowHeaders then
    if HeaderWidth <> -1 then
    begin
      Result := HeaderWidth - IndentAreas[Row];
      if Result < 0 then Result := 0;
    end
    else
      Result := Adapter.HeaderAvailableWidths[Row]
  else
    Result := 0;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetHeaderMinWidth: Integer;
begin
  Result := Adapter.HeaderMinWidth;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetHeaderWidth: Integer;
begin
  if ShowHeaders then
    if FHeaderWidth = -1 then
      Result := Adapter.HeaderWidth
    else
      Result := FHeaderWidth
  else
    Result := 0;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetIndentArea(Row: TcxCustomRow): Integer;
begin
  if ShowHeaders then
    Result := Adapter.IndentAreas[Row]
  else
    Result := 0;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetInternalRecordWidth: Integer;
begin
  Result := Adapter.RecordWidth[InternalRecordCount];
end;

function TcxCustomVerticalGridReportLinkFormatter.GetInterRecordsSpace: Integer;
begin
  if FInterRecordsSpace = -1 then
    Result := Adapter.InterRecordsSpace
  else
    Result := FInterRecordsSpace;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetLookAndFeelItem(Index: Integer): TdxReportVisualItem;
begin
  Result := TdxReportVisualItem(FLookAndFeelItems[Index]);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetLookAndFeelItemCount: Integer;
begin
  Result := FLookAndFeelItems.Count;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetOptionsView: TcxVerticalGridReportLinkOptionsView;
begin
  Result := ReportLink.OptionsView;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRecordCount(Index: Integer): Integer;
begin
  Result := WrappedRecordStopIndexes[Index] - WrappedRecordStartIndexes[Index] + 1;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetInternalIndexByRecordIndex(RecordIndex: Integer): Integer;
begin
  Result := RecordIndex - FirstInternalRecordIndex;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRecordMinWidth(RecordIndex: Integer): Integer;
begin
  Result := Adapter.RecordMinWidths[RecordIndex];
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRecordWidth(RecordIndex: Integer): Integer;
begin
  Result := Integer(FRecordWidths[InternalIndexesByRecordIndex[RecordIndex]]);
  if Result = -1 then
    Result := InternalRecordWidth;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRow(Index: Integer): TcxCustomRow;
begin
  Result := TcxCustomRow(FRows[Index]);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowCount: Integer;
begin
  Result := FRows.Count;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowHeight(Row: TcxCustomRow): Integer;
begin
  Result := Integer(FRowHeights[IndexOfRow(Row)]);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowHelper(Row: TcxCustomRow): TcxVerticalGridCustomRowHelper;
begin
  Result := Adapter.Helpers[Row];
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowPlace(WrapIndex, Index: Integer): TcxVerticalGridCustomRowPlace;
begin
  Result := FRowPlaces[RowPlaceFlatIndexes[WrapIndex, Index]];
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowPlaceByRow(Row: TcxCustomRow; WrapIndex: Integer): TcxVerticalGridCustomRowPlace;
begin
  Result := RowPlaces[WrapIndex, IndexOfRow(Row)];
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowPlaceFlatIndex(WrapIndex, Index: Integer): Integer;
begin
  Result := WrapIndex * RowCount + Index;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRowPlaceCount: Integer;
begin
  Result := FRowPlaces.Count;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetRenderer: TdxPSReportRenderer;
begin
  Result := ReportLink.Renderer;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetShowBorders: Boolean;
begin
  Result := OptionsView.Borders;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetShowHeaders: Boolean;
begin
  Result := OptionsView.Headers;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetShowHorzGridLines: Boolean;
begin
  Result := Adapter.ShowHorzGridLines;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetShowVertGridLines: Boolean;
begin
  Result := Adapter.ShowVertGridLines;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetViewMaxWidth: Integer;
var
  I, V: Integer;
begin
  Result := 0;
  for I := 0 to WrapCount - 1 do
  begin
    V := ViewWidths[I];
    if Result < V then Result := V;
  end;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetViewWidth(WrapIndex: Integer): Integer;
begin
  Result := Integer(FViewWidths[WrapIndex]);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetWrapCount: Integer;
begin
  Result := Max(1, FWrappedRecordStartIndexes.Count - 1);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetWrappedRecordStartIndex(Index: Integer): Integer;
begin
  Result := Integer(FWrappedRecordStartIndexes[Index]);
end;

function TcxCustomVerticalGridReportLinkFormatter.GetWrappedRecordStopIndex(Index: Integer): Integer;
begin
  if Index < WrapCount - 1 then
    Result := WrappedRecordStartIndexes[Index + 1] - 1
  else
    Result := WrappedRecordStartIndexes[0] + InternalRecordCount - 1;
end;

function TcxCustomVerticalGridReportLinkFormatter.GetWrapSeparatorHeight: Integer;
begin
  Result := ReportLink.OptionsSize.WrapSeparatorHeight;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.SetInterRecordsSpace(Value: Integer);
begin
  if Value < 0 then Value := 0;
  FInterRecordsSpace := Value;
end;

procedure TcxCustomVerticalGridReportLinkFormatter.SetRecordWidth(RecordIndex: Integer; Value: Integer);
begin
  FRecordWidths[InternalIndexesByRecordIndex[RecordIndex]] := TObject(Value);
end;

procedure TcxCustomVerticalGridReportLinkFormatter.SetRowHeight(Row: TcxCustomRow; Value: Integer);
begin
  FRowHeights[IndexOfRow(Row)] := TObject(Value);
end;

procedure TcxCustomVerticalGridReportLinkFormatter.SetViewWidth(WrapIndex: Integer; Value: Integer);
begin
  FViewWidths[WrapIndex] := TObject(Value);
end;

procedure TcxCustomVerticalGridReportLinkFormatter.FormatLookAndFeelItems;
const
  Borders3D: array[TdxCellEdgeStyle, Boolean] of TdxPSCellBorderClass =
   ((TdxPSCellRaisedBorder, TdxPSCellRaisedSoftBorder),
    (TdxPSCellSunkenBorder, TdxPSCellSunkenSoftBorder));
var
  AItem: TdxReportVisualItem;
  I: Integer;
begin
  for I := 0 to LookAndFeelItemCount - 1 do
  begin
    AItem := LookAndFeelItems[I];
    if ReportLink.Effects3D then
      AItem.BorderClass := Borders3D[AItem.Edge3DStyle, ReportLink.Soft3D]
    else
      AItem.BorderClass := TdxPSCellUltraFlatBorder;
  end;
end;

{ TcxVerticalGridReportLinkOptionsExpanding }

procedure TcxVerticalGridReportLinkOptionsExpanding.Assign(Source: TPersistent);
begin
  if Source is TcxVerticalGridReportLinkOptionsExpanding then
    with TcxVerticalGridReportLinkOptionsExpanding(Source) do
    begin
      Self.AutoExpandRows := AutoExpandRows;
    end;
  inherited;
end;

procedure TcxVerticalGridReportLinkOptionsExpanding.RestoreDefaults;
begin
  inherited;
  AutoExpandRows := False;
end;

function TcxVerticalGridReportLinkOptionsExpanding.DesignerTabIndex: Integer;
begin
  Result := 1;
end;

function TcxVerticalGridReportLinkOptionsExpanding.GetReportLink: TcxCustomVerticalGridReportLink;
begin
  Result := inherited ReportLink as TcxCustomVerticalGridReportLink;
end;

procedure TcxVerticalGridReportLinkOptionsExpanding.SetAutoExpandRows(Value: Boolean);
begin
  if FAutoExpandRows <> Value then
  begin
    FAutoExpandRows := Value;
    Changed;
  end;
end;

{ TcxVerticalGridReportLinkOptionsFormatting }

function TcxVerticalGridReportLinkOptionsFormatting.DesignerTabIndex: Integer;
begin
  Result := 2;
end;

function TcxVerticalGridReportLinkOptionsFormatting.GetReportLink: TcxCustomVerticalGridReportLink;
begin
  Result := inherited ReportLink as TcxCustomVerticalGridReportLink;
end;

{ TcxVerticalGridReportLinkOptionsPagination }

procedure TcxVerticalGridReportLinkOptionsPagination.Assign(Source: TPersistent);
begin
  if Source is TcxVerticalGridReportLinkOptionsPagination then
    with TcxVerticalGridReportLinkOptionsPagination(Source) do
    begin
      Self.ByRows := ByRows;
      Self.ByWrapping := ByWrapping;
      Self.OneWrappingPerPage := OneWrappingPerPage;
    end;
  inherited;
end;

procedure TcxVerticalGridReportLinkOptionsPagination.RestoreDefaults;
begin
  inherited;
  ByRows := True;
  ByWrapping := True;
  OneWrappingPerPage := False;
end;

function TcxVerticalGridReportLinkOptionsPagination.DesignerTabIndex: Integer;
begin
  Result := 2;
end;

function TcxVerticalGridReportLinkOptionsPagination.GetReportLink: TcxCustomVerticalGridReportLink;
begin
  Result := inherited ReportLink as TcxCustomVerticalGridReportLink;
end;

procedure TcxVerticalGridReportLinkOptionsPagination.SetByRows(Value: Boolean);
begin
  if FByRows <> Value then
  begin
    FByRows := Value;
    Changed;
  end;
end;

procedure TcxVerticalGridReportLinkOptionsPagination.SetByWrapping(Value: Boolean);
begin
  if FByWrapping <> Value then
  begin
    FByWrapping := Value;
    Changed;
  end;
end;

procedure TcxVerticalGridReportLinkOptionsPagination.SetOneWrappingPerPage(Value: Boolean);
begin
  if FOneWrappingPerPage <> Value then
  begin
    FOneWrappingPerPage := Value;
    if ByWrapping then Changed;
  end;
end;

{ TcxVerticalGridReportLinkOptionsRefinements }

function TcxVerticalGridReportLinkOptionsRefinements.DesignerTabIndex: Integer;
begin
  Result := 2;
end;

function TcxVerticalGridReportLinkOptionsRefinements.GetReportLink: TcxCustomVerticalGridReportLink;
begin
  Result := inherited ReportLink as TcxCustomVerticalGridReportLink;
end;

function TcxVerticalGridReportLinkOptionsRefinements.GetSuppressBackgroundBitmaps: Boolean;
begin
  Result := ReportLink.OptionsFormatting.SuppressBackgroundBitmaps;
end;

procedure TcxVerticalGridReportLinkOptionsRefinements.SetSuppressBackgroundBitmaps(Value: Boolean);
begin
  ReportLink.OptionsFormatting.SuppressBackgroundBitmaps := Value;
end;

{ TcxVerticalGridReportLinkOptionsSize }

procedure TcxVerticalGridReportLinkOptionsSize.Assign(Source: TPersistent);
begin
  if Source is TcxVerticalGridReportLinkOptionsSize then
    with TcxVerticalGridReportLinkOptionsSize(Source) do
    begin
      Self.BestFit := BestFit;
      Self.KeepSameRecordWidths := KeepSameRecordWidths;
      Self.WrapRecords := WrapRecords;
      Self.WrapSeparatorHeight := WrapSeparatorHeight;
    end;
  inherited;
end;

procedure TcxVerticalGridReportLinkOptionsSize.RestoreDefaults;
begin
  inherited;
  BestFit := False;
  KeepSameRecordWidths := True;
  WrapRecords := True;
  WrapSeparatorHeight := 20;
end;

function TcxVerticalGridReportLinkOptionsSize.DesignerTabIndex: Integer;
begin
  Result := 1;
end;

function TcxVerticalGridReportLinkOptionsSize.GetReportLink: TcxCustomVerticalGridReportLink;
begin
  Result := inherited ReportLink as TcxCustomVerticalGridReportLink;
end;

procedure TcxVerticalGridReportLinkOptionsSize.SetBestFit(Value: Boolean);
begin
  if FBestFit <> Value then
  begin
    FBestFit := Value;
    Changed;
  end;
end;

procedure TcxVerticalGridReportLinkOptionsSize.SetKeepSameRecordWidths(Value: Boolean);
begin
  if FKeepSameRecordWidths <> Value then
  begin
    FKeepSameRecordWidths := Value;
    if BestFit then Changed;
  end;
end;

procedure TcxVerticalGridReportLinkOptionsSize.SetWrapRecords(Value: Boolean);
begin
  if FWrapRecords <> Value then
  begin
    FWrapRecords := Value;
    Changed;
  end;
end;

procedure TcxVerticalGridReportLinkOptionsSize.SetWrapSeparatorHeight(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FWrapSeparatorHeight <> Value then
  begin
    FWrapSeparatorHeight := Value;
    Changed;
  end;
end;

{ TcxVerticalGridReportLinkOptionsView }

procedure TcxVerticalGridReportLinkOptionsView.Assign(Source: TPersistent);
begin
  if Source is TcxVerticalGridReportLinkOptionsView then
    with TcxVerticalGridReportLinkOptionsView(Source) do
    begin
      Self.Borders := Borders;
      Self.ExpandButtons := ExpandButtons;
      Self.Headers := Headers;
      Self.Mode := Mode;
    end;
  inherited;
end;

procedure TcxVerticalGridReportLinkOptionsView.RestoreDefaults;
begin
  inherited;
  Borders := True;
  ExpandButtons := True;
  Headers := True;
  Mode := vpmLoadedRecords;
end;

function TcxVerticalGridReportLinkOptionsView.DesignerTabIndex: Integer;
begin
  Result := 0;
end;

function TcxVerticalGridReportLinkOptionsView.GetActualMode: TcxVerticalGridReportLinkPrintMode;
begin
  Result := vpmLoadedRecords; // i.e. always equals 1 record for non Virtual(DB)VerticalGrid
end;

function TcxVerticalGridReportLinkOptionsView.GetReportLink: TcxCustomVerticalGridReportLink;
begin
  Result := inherited ReportLink as TcxCustomVerticalGridReportLink;
end;

procedure TcxVerticalGridReportLinkOptionsView.SetBorders(Value: Boolean);
begin
  if FBorders <> Value then
  begin
    FBorders := Value;
    Changed;
  end;
end;

procedure TcxVerticalGridReportLinkOptionsView.SetExpandButtons(Value: Boolean);
begin
  if FExpandButtons <> Value then
  begin
    FExpandButtons := Value;
    Changed;
  end;
end;

procedure TcxVerticalGridReportLinkOptionsView.SetHeaders(Value: Boolean);
begin
  if FHeaders <> Value then
  begin
    FHeaders := Value;
    Changed;
  end;
end;

procedure TcxVerticalGridReportLinkOptionsView.SetMode(Value: TcxVerticalGridReportLinkPrintMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    Changed;
  end;
end;

{ TcxVerticalGridReportLinkStyles }

procedure TcxVerticalGridReportLinkStyles.Assign(Source: TPersistent);
begin
  if Source is TcxVerticalGridReportLinkStyles then
    with TcxVerticalGridReportLinkStyles(Source) do
    begin
      Self.Category := Category;
      Self.Content := Content;
      Self.Header := Header;
    end;
  inherited;
end;

procedure TcxVerticalGridReportLinkStyles.GetCategoryParams(ARow: TcxCustomRow;
  out AParams: TcxViewParams);
begin
  GetViewParams(vspsVGridCategory, ARow, nil, AParams);
end;

procedure TcxVerticalGridReportLinkStyles.GetContentParams(ARow: TcxCustomRow;
  AnIndex, ARecordIndex: Integer; out AParams: TcxViewParams);
begin
  GetViewParams(vspsVGridContent, ARow, nil, AParams);
end;

procedure TcxVerticalGridReportLinkStyles.GetHeaderParams(ARow: TcxCustomRow;
  out AParams: TcxViewParams);
begin
  GetViewParams(vspsVGridHeader, nil, nil, AParams);
end;

procedure TcxVerticalGridReportLinkStyles.GetIndentParams(ARow: TcxCustomRow;
  out AParams: TcxViewParams);
const
  StyleIndexes: array[Boolean] of Integer = (vspsVGridHeader, vspsVGridCategory);
begin
  GetViewParams(StyleIndexes[ARow is TcxCategoryRow], ARow, nil, AParams)
end;

function TcxVerticalGridReportLinkStyles.DesignerTabIndex: Integer;
begin
  Result := 3;
end;

procedure TcxVerticalGridReportLinkStyles.GetDefaultViewParams(Index: Integer; AData: TObject;
  out AParams: TcxViewParams);
const
  FixedParts = [vspsVGridCategory, vspsVGridHeader];
begin
  inherited;
  if ReportLink <> nil then
    with AParams do
    begin
      if Index in FixedParts then
        Color := dxPSCore.dxDefaultFixedColor
      else
        Color := dxPSCore.dxDefaultContentColor;

      if Index = vspsVGridCategory then
        Font := ReportLink.CategoryFont
      else
        Font := ReportLink.Font;

      TextColor := Font.Color;
    end;
end;

class function TcxVerticalGridReportLinkStyles.GetStyleCaption(AnIndex: Integer): string;
begin
  case AnIndex of
    vspsVGridContent:
      Result := cxGetResourceString(@sdxContentStyle);
    vspsVGridHeader:
      Result := cxGetResourceString(@sdxHeaderStyle);
  else
    Result := cxGetResourceString(@sdxCategoryStyle);
  end;
end;

function TcxVerticalGridReportLinkStyles.GetStyleIndexByCaption(const Caption: string): Integer;
begin
  for Result := vspsVGridFirst to vspsVGridLast do
    if dxPSUtl.dxSameText(Caption, GetStyleCaption(Result)) then
      Exit;
  Result := -1;
end;

function TcxVerticalGridReportLinkStyles.GetReportLink: TcxCustomVerticalGridReportLink;
begin
  Result := inherited ReportLink as TcxCustomVerticalGridReportLink;
end;

{ TcxVerticalGridReportLinkStyleSheet }

class function TcxVerticalGridReportLinkStyleSheet.GetStylesClass: TcxCustomStylesClass;
begin
  Result := TcxVerticalGridReportLinkStyles;
end;

function TcxVerticalGridReportLinkStyleSheet.GetStylesValue: TcxVerticalGridReportLinkStyles;
begin
  if GetStyles is TcxVerticalGridReportLinkStyles then
    Result := TcxVerticalGridReportLinkStyles(GetStyles)
  else
    Result := nil;
end;

procedure TcxVerticalGridReportLinkStyleSheet.SetStylesValue(Value: TcxVerticalGridReportLinkStyles);
begin
  SetStyles(Value);
end;

{ TcxVerticalGridHostInfo }

procedure TcxVerticalGridHostInfo.Initialize(AParent: TdxReportCell);
begin
  FParent := AParent;
end;

{ TcxCustomVerticalGridReportLink }

constructor TcxCustomVerticalGridReportLink.Create(AOwner: TComponent);
begin
  inherited;
  FCategoryFont := TFont.Create;
  CategoryFont := Font;
  FDelimitersHardVert := TList.Create;
  FDelimitersHardHorz := TList.Create;
  InternalRestoreDefaults;
  LinkModified(False);
end;

destructor TcxCustomVerticalGridReportLink.Destroy;
begin
  FreeAndNil(FDelimitersHardVert);
  FreeAndNil(FDelimitersHardHorz);
  FreeAndNil(FCategoryFont);
  inherited Destroy;
end;

procedure TcxCustomVerticalGridReportLink.ConstructReport(AReportCells: TdxReportCells);

  function HasData: Boolean;
  begin
    Result := VerticalGrid.HasVisibleRows;
  end;

  procedure Build;
  begin
    FBuilder := CreateBuilder;
    try
      FBuilder.Build;
    finally
      FreeAndNil(FBuilder);
    end;
  end;

  procedure CalculateSizes;

    procedure CalculateReportPartSizes(ACell: TdxReportCell);
    var
      Width, I, V: Integer;
    begin
      if ACell.CellCount > 0 then
      begin
        Width := 0;
        for I := 0 to ACell.CellCount - 1 do
        begin
          V := ACell.Cells[I].Width;
          if Width < V then Width := V;
        end;
        ACell.BoundsRect := Rect(0, 0, Width, ACell[ACell.CellCount - 1].BoundsRect.Bottom);
      end;
    end;

  begin
    CalculateReportPartSizes(AReportCells.Cells);
  end;

begin
  if VerticalGrid = nil then Exit;
  inherited;
  if not HasData then Exit;

  PrepareConstruct;
  try
    Build;
    if not AbortBuilding then CalculateSizes;
  finally
    UnprepareConstruct;
  end;
end;

procedure TcxCustomVerticalGridReportLink.ConvertCoords;
begin
  inherited ConvertCoords;
  ConvertDelimiters(DelimitersHardVert);
  ConvertDelimiters(DelimitersHardHorz);
end;

procedure TcxCustomVerticalGridReportLink.CustomDraw(AItem: TAbstractdxReportCellData;
  ACanvas: TCanvas; ABoundsRect, AClientRect: TRect; var ADone: Boolean);
var
  DrawInfo: TcxVerticalGridCellCustomDrawInfo;
begin
  GetItemCustomDrawInfo(AItem, DrawInfo);
  with DrawInfo do
    case AttributeID of
      cxVerticalGridCategoryRowID:
        DoCustomDrawRowHeaderCell(ACanvas, Row, CellIndex, TdxReportCellImage(AItem), ADone);
      cxVerticalGridRowHeaderID:
        DoCustomDrawRowHeaderCell(ACanvas, Row, CellIndex, TdxReportCellImage(AItem), ADone);
      cxVerticalGridRowHeaderSeparatorID:
        DoCustomDrawRowHeaderSeparatorCell(ACanvas, Row, CellIndex, TdxReportCellString(AItem), ADone);
      cxVerticalGridRowValueID:
        DoCustomDrawRowValueCell(ACanvas, Row, CellIndex, RecordIndex, AItem, ADone);
      cxVerticalGridRowValueSeparatorID:
        DoCustomDrawRowValueSeparatorCell(ACanvas, Row, CellIndex, RecordIndex, TdxReportCellString(AItem), ADone);
      cxVerticalGridRowIndentID:
        DoCustomDrawRowIndentCell(ACanvas, Row, CellIndex, TdxReportCellExpandButton(AItem), ADone);
    end;
end;

procedure TcxCustomVerticalGridReportLink.FontChanged(Sender: TObject);
begin
  if CategoryFont <> nil then CategoryFont := Font;
  inherited;
end;

function TcxCustomVerticalGridReportLink.GetBreakPagesByHardDelimiters: Boolean;
begin
  Result := OptionsPagination.OneWrappingPerPage;
end;

procedure TcxCustomVerticalGridReportLink.GetImageLists(AProc: TdxPSGetImageListProc);
begin
  inherited;
  AProc(VerticalGrid.Images);
end;

function TcxCustomVerticalGridReportLink.GetRebuildOnPageParamsChange(AUpdateCodes: TdxPrinterPageUpdateCodes): Boolean;
begin
  Result := (AUpdateCodes * uaMarginsVert  <> []) and (OptionsSize.AutoWidth or OptionsSize.WrapRecords);
end;

function TcxCustomVerticalGridReportLink.GetUseHardVertDelimiters: Boolean;
begin
  Result := OptionsPagination.ByWrapping;
end;

procedure TcxCustomVerticalGridReportLink.InternalRestoreFromOriginal;
begin
  inherited;
  if VerticalGrid <> nil then
  begin
    OptionsFormatting.LookAndFeelKind := VerticalGrid.LookAndFeel.Kind;
    OptionsSize.AutoWidth := VerticalGrid.OptionsView.AutoScaleBands;
    OptionsView.ExpandButtons := VerticalGrid.OptionsView.ShowButtons;
    OptionsView.Headers := VerticalGrid.OptionsView.ShowHeaders;
  end;
end;

function TcxCustomVerticalGridReportLink.IsSupportedCustomDraw(Item: TAbstractdxReportCellData): Boolean;
var
  DrawInfo: TcxVerticalGridCellCustomDrawInfo;
begin
  Result := inherited IsSupportedCustomDraw(Item) and
    (Item <> nil) and IsCustomDrawn(GetItemCustomDrawInfo(Item, DrawInfo));
end;

procedure TcxCustomVerticalGridReportLink.MakeHardDelimiters(
  AReportCells: TdxReportCells; AHorzDelimiters, AVertDelimiters: TList);
begin
  inherited MakeHardDelimiters(AReportCells, AHorzDelimiters, AVertDelimiters);
  dxCopyList(DelimitersHardHorz, AHorzDelimiters);
  dxCopyList(DelimitersHardVert, AVertDelimiters);
end;

procedure TcxCustomVerticalGridReportLink.DoCustomDrawRowHeaderCell(ACanvas: TCanvas;
  ARow: TcxCustomRow; AnIndex: Integer; AnItem: TdxReportCellImage; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawRowHeaderCell) then
    FOnCustomDrawRowHeaderCell(Self, ACanvas, ARow, AnIndex, AnItem, ADone);
end;

procedure TcxCustomVerticalGridReportLink.DoCustomDrawRowHeaderSeparatorCell(ACanvas: TCanvas;
  ARow: TcxCustomRow; AnIndex: Integer; AnItem: TdxReportCellString; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawRowHeaderSeparatorCell) then
    FOnCustomDrawRowHeaderSeparatorCell(Self, ACanvas, ARow, AnIndex, AnItem, ADone);
end;

procedure TcxCustomVerticalGridReportLink.DoCustomDrawRowIndentCell(ACanvas: TCanvas;
  ARow: TcxCustomRow; AnIndex: Integer; AnItem: TdxReportCellExpandButton; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawRowIndentCell) then
    FOnCustomDrawRowIndentCell(Self, ACanvas, ARow, AnIndex, AnItem, ADone);
end;

procedure TcxCustomVerticalGridReportLink.DoCustomDrawRowValueCell(ACanvas: TCanvas;
  ARow: TcxCustomRow; ACellIndex, ARecordIndex: Integer; AnItem: TAbstractdxReportCellData;
  var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawRowValueCell) then
    FOnCustomDrawRowValueCell(Self, ACanvas, ARow, ACellIndex, ARecordIndex, AnItem, ADone);
end;

procedure TcxCustomVerticalGridReportLink.DoCustomDrawRowValueSeparatorCell(ACanvas: TCanvas;
  ARow: TcxCustomRow; ACellIndex, ARecordIndex: Integer; AnItem: TdxReportCellString; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawRowValueSeparatorCell) then
    FOnCustomDrawRowValueSeparatorCell(Self, ACanvas, ARow, ACellIndex, ARecordIndex, AnItem, ADone);
end;

procedure TcxCustomVerticalGridReportLink.DoInitializeRowHeaderCell(ARow: TcxCustomRow;
  ACellIndex: Integer; AnItem: TdxReportCellImage);
begin
  if Assigned(FOnInitializeRowHeaderCell) then
    FOnInitializeRowHeaderCell(Self, ARow, ACellIndex, AnItem);
end;

procedure TcxCustomVerticalGridReportLink.DoInitializeRowHeaderSeparatorCell(ARow: TcxCustomRow;
  AnIndex: Integer; AnItem: TdxReportCellString);
begin
  if Assigned(FOnInitializeRowHeaderSeparatorCell) then
    FOnInitializeRowHeaderSeparatorCell(Self, ARow, AnIndex, AnItem);
end;

procedure TcxCustomVerticalGridReportLink.DoInitializeRowIndentCell(ARow: TcxCustomRow;
  AnIndex: Integer; AnItem: TdxReportCellExpandButton);
begin
  if Assigned(FOnInitializeRowIndentCell) then
    FOnInitializeRowIndentCell(Self, ARow, AnIndex, AnItem);
end;

procedure TcxCustomVerticalGridReportLink.DoInitializeRowValueCell(ARow: TcxCustomRow;
  ACellIndex, ARecordIndex: Integer; AnItem: TAbstractdxReportCellData);
begin
  if Assigned(FOnInitializeRowValueCell) then
    FOnInitializeRowValueCell(Self, ARow, ACellIndex, ARecordIndex, AnItem);
end;

procedure TcxCustomVerticalGridReportLink.DoInitializeRowValueSeparatorCell(ARow: TcxCustomRow;
  ACellIndex, ARecordIndex: Integer; AnItem: TdxReportCellString);
begin
  if Assigned(FOnInitializeRowValueSeparatorCell) then
    FOnInitializeRowValueSeparatorCell(Self, ARow, ACellIndex, ARecordIndex, AnItem);
end;

function TcxCustomVerticalGridReportLink.GetOptionsExpandingClass: TdxCustomReportLinkOptionsExpandingClass;
begin
  Result := TcxVerticalGridReportLinkOptionsExpanding;
end;

function TcxCustomVerticalGridReportLink.GetOptionsFormattingClass: TdxCustomReportLinkOptionsFormattingClass;
begin
  Result := TcxVerticalGridReportLinkOptionsFormatting;
end;

function TcxCustomVerticalGridReportLink.GetOptionsPaginationClass: TdxCustomReportLinkOptionsPaginationClass;
begin
  Result := TcxVerticalGridReportLinkOptionsPagination;
end;

function TcxCustomVerticalGridReportLink.GetOptionsRefinementsClass: TdxCustomReportLinkOptionsRefinementsClass;
begin
  Result := TcxVerticalGridReportLinkOptionsRefinements;
end;

function TcxCustomVerticalGridReportLink.GetOptionsSizeClass: TdxCustomReportLinkOptionsSizeClass;
begin
  Result := TcxVerticalGridReportLinkOptionsSize;
end;

function TcxCustomVerticalGridReportLink.GetOptionsViewClass: TdxCustomReportLinkOptionsViewClass;
begin
  Result := TcxVerticalGridReportLinkOptionsView;
end;

function TcxCustomVerticalGridReportLink.GetAreNativeStylesAvailable: Boolean;
begin
  Result := OptionsFormatting.UseNativeStyles;
end;

function TcxCustomVerticalGridReportLink.GetStylesClass: TdxCustomReportLinkStylesClass;
begin
  Result := TcxVerticalGridReportLinkStyles;
end;

function TcxCustomVerticalGridReportLink.GetStyleSheetClass: TdxCustomReportLinkStyleSheetClass;
begin
  Result := TcxVerticalGridReportLinkStyleSheet;
end;

function TcxCustomVerticalGridReportLink.GetStyleSheetPrototype: TdxCustomReportLinkStyleSheet;
begin
  Result := DefaultdxPScxVerticalGridLinkStyleSheet;
end;

procedure TcxCustomVerticalGridReportLink.PrepareConstruct;
begin
  inherited PrepareConstruct;
  DelimitersHardVert.Clear;
  DelimitersHardHorz.Clear;
  ReportCells.LookAndFeel := nil;//CreateGroupLookAndFeel(TdxPSReportGroupNullLookAndFeel);
  FHostInfo := TcxVerticalGridHostInfo.Create;
  FHostInfo.Initialize(ReportCells.Cells);
end;

procedure TcxCustomVerticalGridReportLink.UnprepareConstruct;
begin
  FreeAndNil(FHostInfo);
  inherited UnprepareConstruct;
end;

procedure TcxCustomVerticalGridReportLink.AddHorizontalHardDelimiter(ADelimiter: Integer);
begin
  DelimitersHardHorz.Add(Pointer(ADelimiter));
end;

procedure TcxCustomVerticalGridReportLink.AddVerticalHardDelimiter(ADelimiter: TdxReportCell);
begin
  AddVerticalHardDelimiter(ADelimiter.AbsoluteRect.Bottom);
end;

procedure TcxCustomVerticalGridReportLink.AddVerticalHardDelimiter(ADelimiter: Integer);
begin
  DelimitersHardVert.Add(TObject(ADelimiter));
end;

function TcxCustomVerticalGridReportLink.CreateBuilder: TcxCustomVerticalGridReportLinkBuilder;
begin
  Result := GetBuilderClass.Create(Self);
end;

class function TcxCustomVerticalGridReportLink.GetBuilderClass: TcxCustomVerticalGridReportLinkBuilderClass;
begin
  Result := TcxCustomVerticalGridReportLinkBuilder;
end;

procedure TcxCustomVerticalGridReportLink.ExtractCustomDrawCodeDataValues(ACode: Integer;
  var AnAttribute: TcxVerticalGridAttributeID; var ACellIndex, ARecordIndex: Integer);
begin
  AnAttribute := (ACode shr CodeAttributeOffset) and CodeAttributeMask;
  ACellIndex := (ACode shr CodeIndexOffset) and CodeIndexMask;
  ARecordIndex := (ACode shr CodeRecordIndexOffset) and CodeRecordIndexMask;
end;

function TcxCustomVerticalGridReportLink.GetItemCustomDrawInfo(AnItem: TdxReportVisualItem;
  out ADrawInfo: TcxVerticalGridCellCustomDrawInfo): TcxVerticalGridAttributeID;
begin
  FillChar(ADrawInfo, SizeOf(ADrawInfo), 0);
  try
    try
      with ADrawInfo do
      begin
        Row := TcxCustomRow(AnItem.Parent.Data);
        ExtractCustomDrawCodeDataValues(AnItem.Data, AttributeID, CellIndex, RecordIndex);
      end;
    except
      FillChar(ADrawInfo, SizeOf(ADrawInfo), 0);
    end;
  finally
    Result := ADrawInfo.AttributeID;
  end;
end;

function TcxCustomVerticalGridReportLink.IsCustomDrawn(AnAttributeID: TcxVerticalGridAttributeID): Boolean;
begin
  Result := False;
  case AnAttributeID of
    cxVerticalGridCategoryRowID:
      Result := Assigned(FOnCustomDrawRowHeaderCell);
    cxVerticalGridRowHeaderID:
      Result := Assigned(FOnCustomDrawRowHeaderCell);
    cxVerticalGridRowHeaderSeparatorID:
      Result := Assigned(FOnCustomDrawRowHeaderSeparatorCell);
    cxVerticalGridRowValueID:
      Result := Assigned(FOnCustomDrawRowValueCell);
    cxVerticalGridRowValueSeparatorID:
      Result := Assigned(FOnCustomDrawRowValueSeparatorCell);
    cxVerticalGridRowIndentID:
      Result := Assigned(FOnCustomDrawRowIndentCell);
  end;
end;

function TcxCustomVerticalGridReportLink.MakeCustomDrawCodeData(
  AnAttribute: TcxVerticalGridAttributeID; ACellIndex, ARecordIndex: Integer): Integer;
begin
  Result := ((AnAttribute and CodeAttributeMask) shl CodeAttributeOffset) or
            ((ACellIndex and CodeIndexMask) shl CodeIndexOffset) or
            ((ARecordIndex and CodeRecordIndexMask) shl CodeRecordIndexOffset);
end;

procedure TcxCustomVerticalGridReportLink.InitializePrintModeControl(AControl: TcxComboBox);
begin
end;

procedure TcxCustomVerticalGridReportLink.UpdateStatePrintModeControl(
  AControl: TcxComboBox; ALayoutItem: TdxLayoutItem);
begin
  ALayoutItem.Enabled := False;
  ALayoutItem.Visible := False;
end;

function TcxCustomVerticalGridReportLink.GetActiveStyles: TcxVerticalGridReportLinkStyles;
begin
  Result := inherited ActiveStyles as TcxVerticalGridReportLinkStyles;
end;

function TcxCustomVerticalGridReportLink.GetDesignWindow: TcxfmVerticalGridReportLinkDesignWindow;
begin
  Result := inherited DesignWindow as TcxfmVerticalGridReportLinkDesignWindow;
end;

function TcxCustomVerticalGridReportLink.GetOptionsExpanding: TcxVerticalGridReportLinkOptionsExpanding;
begin
  Result := inherited OptionsExpanding as TcxVerticalGridReportLinkOptionsExpanding;
end;

function TcxCustomVerticalGridReportLink.GetOptionsFormatting: TcxVerticalGridReportLinkOptionsFormatting;
begin
  Result := inherited OptionsFormatting as TcxVerticalGridReportLinkOptionsFormatting;
end;

function TcxCustomVerticalGridReportLink.GetOptionsPagination: TcxVerticalGridReportLinkOptionsPagination;
begin
  Result := inherited OptionsPagination as TcxVerticalGridReportLinkOptionsPagination;
end;

function TcxCustomVerticalGridReportLink.GetOptionsRefinements: TcxVerticalGridReportLinkOptionsRefinements;
begin
  Result := inherited OptionsRefinements as TcxVerticalGridReportLinkOptionsRefinements;
end;

function TcxCustomVerticalGridReportLink.GetOptionsSize: TcxVerticalGridReportLinkOptionsSize;
begin
  Result := inherited OptionsSize as TcxVerticalGridReportLinkOptionsSize;
end;

function TcxCustomVerticalGridReportLink.GetOptionsView: TcxVerticalGridReportLinkOptionsView;
begin
  Result := inherited OptionsView as TcxVerticalGridReportLinkOptionsView;
end;

function TcxCustomVerticalGridReportLink.GetStyles: TcxVerticalGridReportLinkStyles;
begin
  Result := inherited Styles as TcxVerticalGridReportLinkStyles;
end;

function TcxCustomVerticalGridReportLink.GetVerticalGrid: TcxCustomVerticalGrid;
begin
  Result := TcxCustomVerticalGrid(Component);
end;

procedure TcxCustomVerticalGridReportLink.SetOnCustomDrawRowHeaderCell(
  Value: TcxVerticalGridReportLinkCustomDrawRowHeaderCellEvent);
begin
  if @FOnCustomDrawRowHeaderCell <> @Value then
  begin
    FOnCustomDrawRowHeaderCell := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TcxCustomVerticalGridReportLink.SetCategoryFont(Value: TFont);
begin
  CategoryFont.Assign(Value);
  CategoryFont.Style := CategoryFont.Style + [fsBold];
end;

procedure TcxCustomVerticalGridReportLink.SetOnCustomDrawRowHeaderSeparatorCell(
  Value: TcxVerticalGridReportLinkCustomDrawRowHeaderSeparatorCellEvent);
begin
  if @FOnCustomDrawRowHeaderSeparatorCell <> @Value then
  begin
    FOnCustomDrawRowHeaderSeparatorCell := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TcxCustomVerticalGridReportLink.SetOnCustomDrawRowIndentCell(
  Value: TcxVerticalGridReportLinkCustomDrawRowIndentCellEvent);
begin
  if @FOnCustomDrawRowIndentCell <> @Value then
  begin
    FOnCustomDrawRowIndentCell := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TcxCustomVerticalGridReportLink.SetOnCustomDrawRowValueCell(
  Value: TcxVerticalGridReportLinkCustomDrawRowValueCellEvent);
begin
  if @FOnCustomDrawRowValueCell <> @Value then
  begin
    FOnCustomDrawRowValueCell := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TcxCustomVerticalGridReportLink.SetOnCustomDrawRowValueSeparatorCell(
  Value: TcxVerticalGridReportLinkCustomDrawRowValueSeparatorCellEvent);
begin
  if @FOnCustomDrawRowValueSeparatorCell <> @Value then
  begin
    FOnCustomDrawRowValueSeparatorCell := Value;
    if SupportedCustomDraw then LinkModified(True);
  end;
end;

procedure TcxCustomVerticalGridReportLink.SetOptionsExpanding(Value: TcxVerticalGridReportLinkOptionsExpanding);
begin
  inherited OptionsExpanding := Value;
end;

procedure TcxCustomVerticalGridReportLink.SetOptionsFormatting(Value: TcxVerticalGridReportLinkOptionsFormatting);
begin
  inherited OptionsFormatting := Value;
end;

procedure TcxCustomVerticalGridReportLink.SetOptionsPagination(Value: TcxVerticalGridReportLinkOptionsPagination);
begin
  inherited OptionsPagination := Value;
end;

procedure TcxCustomVerticalGridReportLink.SetOptionsRefinements(Value: TcxVerticalGridReportLinkOptionsRefinements);
begin
  inherited OptionsRefinements := Value;
end;

procedure TcxCustomVerticalGridReportLink.SetOptionsSize(Value: TcxVerticalGridReportLinkOptionsSize);
begin
  inherited OptionsSize := Value;
end;

procedure TcxCustomVerticalGridReportLink.SetOptionsView(Value: TcxVerticalGridReportLinkOptionsView);
begin
  inherited OptionsView := Value;
end;

procedure TcxCustomVerticalGridReportLink.SetStyles(Value: TcxVerticalGridReportLinkStyles);
begin
  inherited Styles := Value;
end;

{ TcxUnboundVerticalGridReportLinkBuilder }

class function TcxUnboundVerticalGridReportLinkBuilder.AdapterClass: TcxCustomVerticalGridAdapterClass;
begin
  Result := TcxUnboundVerticalGridAdapter;
end;

{ TcxUnboundVerticalGridAdapter }

function TcxUnboundVerticalGridAdapter.GetLayoutStyle: TcxvgLayoutStyle;
begin
  Result := TcxvgLayoutStyle(UnboundVerticalGrid_GetLayoutStyle(VerticalGrid));
end;

function TcxUnboundVerticalGridAdapter.GetVerticalGrid: TcxUnboundVerticalGrid;
begin
  Result := inherited VerticalGrid as TcxUnboundVerticalGrid;
end;

{ TcxUnboundVerticalGridReportLink }

class function TcxUnboundVerticalGridReportLink.GetBuilderClass: TcxCustomVerticalGridReportLinkBuilderClass;
begin
  Result := TcxUnboundVerticalGridReportLinkBuilder;
end;

{ TcxVerticalGridReportLink }

function TcxVerticalGridReportLink.GetVerticalGrid: TcxVerticalGrid;
begin
  Result := TcxVerticalGrid(Component);
end;

{ TcxRTTIInspectorReportLink }

function TcxRTTIInspectorReportLink.GetRTTIInspector: TcxRTTIInspector;
begin
  Result := TcxRTTIInspector(Component);
end;

{ TcxVirtualVerticalGridReportLinkBuilder }

class function TcxVirtualVerticalGridReportLinkBuilder.AdapterClass: TcxCustomVerticalGridAdapterClass;
begin
  Result := TcxVirtualVerticalGridAdapter;
end;

class function TcxVirtualVerticalGridReportLinkBuilder.FormatterClass: TcxCustomVerticalGridReportLinkFormatterClass;
begin
  Result := TcxVirtualVerticalGridReportLinkFormatter;
end;

{ TcxVirtualVerticalGridAdapter }

function TcxVirtualVerticalGridAdapter.GetInterRecordsSpace: Integer;
begin
  Result := OptionsView.RecordsInterval;
end;

function TcxVirtualVerticalGridAdapter.GetLayoutStyle: TcxvgLayoutStyle;
begin
  Result := VerticalGrid.LayoutStyle;
end;

function TcxVirtualVerticalGridAdapter.GetOptionsView: TcxvgMultiRecordsOptionsView;
begin
  Result := inherited OptionsView as TcxvgMultiRecordsOptionsView;
end;

function TcxVirtualVerticalGridAdapter.GetVerticalGrid: TcxVirtualVerticalGrid;
begin
  Result := inherited VerticalGrid as TcxVirtualVerticalGrid;
end;

{ TcxVirtualVerticalGridReportLinkFormatter }

function TcxVirtualVerticalGridReportLinkFormatter.GetFirstInternalRecordIndex: Integer;
begin
  Result := inherited GetFirstInternalRecordIndex;
  if (Adapter.LayoutStyle <> lsMultiRecordView) and (InternalRecordCount = 1) and
   (ReportLink.OptionsView.ActualMode = vpmLoadedRecords) then
    Result := Adapter.FirstRecordIndex;
end;

function TcxVirtualVerticalGridReportLinkFormatter.GetInternalRecordCount: Integer;
begin
  Result := inherited GetInternalRecordCount;
  if (Adapter.LayoutStyle <> lsMultiRecordView) and (Result <> 0) and
   (ReportLink.OptionsView.ActualMode = vpmLoadedRecords) then
    Result := 1;
end;

{ TcxVirtualVerticalGridReportLinkOptionsView }

function TcxVirtualVerticalGridReportLinkOptionsView.GetActualMode: TcxVerticalGridReportLinkPrintMode;
begin
  Result := Mode;
  if ReportLink.VirtualVerticalGrid <> nil then
    with ReportLink.VirtualVerticalGrid do
      if not DataController.IsGridMode and (LayoutStyle = lsMultiRecordView) then
        Result := vpmAllRecords;
end;

function TcxVirtualVerticalGridReportLinkOptionsView.GetReportLink: TcxVirtualVerticalGridReportLink;
begin
  Result := inherited ReportLink as TcxVirtualVerticalGridReportLink;
end;

{ TcxVirtualVerticalGridReportLink }

procedure TcxVirtualVerticalGridReportLink.InternalRestoreFromOriginal;
begin
  inherited;
  if VirtualVerticalGrid <> nil then
    if VirtualVerticalGrid.DataController.IsGridMode then
      OptionsView.Mode := vpmLoadedRecords
    else
      if VirtualVerticalGrid.LayoutStyle = lsMultiRecordView then
        OptionsView.Mode := vpmAllRecords
      else
        OptionsView.Mode := vpmLoadedRecords;
end;

class function TcxVirtualVerticalGridReportLink.GetBuilderClass: TcxCustomVerticalGridReportLinkBuilderClass;
begin
  Result := TcxVirtualVerticalGridReportLinkBuilder;
end;

function TcxVirtualVerticalGridReportLink.GetOptionsViewClass: TdxCustomReportLinkOptionsViewClass;
begin
  Result := TcxVirtualVerticalGridReportLinkOptionsView;
end;

procedure TcxVirtualVerticalGridReportLink.InitializePrintModeControl(
  AControl: TcxComboBox);

  function IsModeAccepted(AMode: TcxVerticalGridReportLinkPrintMode): Boolean;
  begin
    if VirtualVerticalGrid <> nil then
      if VirtualVerticalGrid.DataController.IsGridMode then
        Result := True
      else
        if VirtualVerticalGrid.LayoutStyle = lsMultiRecordView then
          Result := AMode = vpmAllRecords
        else
          Result := True
    else
      Result := True;
  end;

  function PrintModeString(AMode: TcxVerticalGridReportLinkPrintMode): string;
  begin
    if (VirtualVerticalGrid = nil) or (VirtualVerticalGrid.LayoutStyle = lsMultiRecordView) then
      if AMode = vpmLoadedRecords then
        Result := cxGetResourceString(@sdxLoadedRecords)
      else
        Result := cxGetResourceString(@sdxAllRecords)
    else
      if AMode = vpmLoadedRecords then
        Result := cxGetResourceString(@sdxCurrentRecord)
      else
        Result := cxGetResourceString(@sdxAllRecords);
  end;

var
  I: TcxVerticalGridReportLinkPrintMode;
  AItems: TStrings;
begin
  AItems := AControl.Properties.Items;
  AItems.BeginUpdate;
  try
    AItems.Clear;
    for I := Low(TcxVerticalGridReportLinkPrintMode) to High(TcxVerticalGridReportLinkPrintMode) do
      if IsModeAccepted(I) then
        AItems.AddObject(PrintModeString(I), TObject(I));

    AControl.ItemIndex := AItems.IndexOfObject(TObject(OptionsView.ActualMode));
  finally
    AItems.EndUpdate;
  end;
end;

procedure TcxVirtualVerticalGridReportLink.UpdateStatePrintModeControl(AControl: TcxComboBox; ALayoutItem: TdxLayoutItem);
begin
  ALayoutItem.Enabled := AControl.Properties.Items.Count > 1;
  ALayoutItem.Visible := True;
end;

function TcxVirtualVerticalGridReportLink.GetOptionsView: TcxVirtualVerticalGridReportLinkOptionsView;
begin
  Result := inherited OptionsView as TcxVirtualVerticalGridReportLinkOptionsView;
end;

function TcxVirtualVerticalGridReportLink.GetVirtualVerticalGrid: TcxVirtualVerticalGrid;
begin
  Result := Component as TcxVirtualVerticalGrid;
end;

procedure TcxVirtualVerticalGridReportLink.SetOptionsView(Value: TcxVirtualVerticalGridReportLinkOptionsView);
begin
  inherited OptionsView := Value;
end;

{ TcxDBVerticalGridReportLinkBuilder }

class function TcxDBVerticalGridReportLinkBuilder.FormatterClass: TcxCustomVerticalGridReportLinkFormatterClass;
begin
  Result := TcxDBVerticalGridReportLinkFormatter;
end;

{ TcxDBVerticalGridReportLinkFormatter }

function TcxDBVerticalGridReportLinkFormatter.GetAreRecordsNeededLoading: Boolean;
begin
  Result := DataController.IsGridMode and (OptionsView.Mode = vpmAllRecords);
end;

function TcxDBVerticalGridReportLinkFormatter.GetAreRecordsNeededUnloading: Boolean;
begin
  Result := IsPrevGridMode and (OptionsView.Mode = vpmAllRecords);
end;

procedure TcxDBVerticalGridReportLinkFormatter.LoadRecords;
begin
  DataController.GridMode := False;
end;

procedure TcxDBVerticalGridReportLinkFormatter.UnloadRecords;
begin
  DataController.GridMode := True;
end;

function TcxDBVerticalGridReportLinkFormatter.GetDataController: TcxDBVerticalGridDataController;
begin
  Result := inherited DataController as TcxDBVerticalGridDataController;
end;

{ TcxDBVerticalGridReportLink }

class function TcxDBVerticalGridReportLink.GetBuilderClass: TcxCustomVerticalGridReportLinkBuilderClass;
begin
  Result := TcxDBVerticalGridReportLinkBuilder;
end;

function TcxDBVerticalGridReportLink.GetDBVerticalGrid: TcxDBVerticalGrid;
begin
  Result := Component as TcxDBVerticalGrid;
end;

{ TcxfmVerticalGridReportLinkDesignWindow }

constructor TcxfmVerticalGridReportLinkDesignWindow.Create(AOwner: TComponent);
begin
  HelpContext := dxPSGlbl.dxhccxVerticalGridReportLinkDesigner;
  inherited Create(AOwner);
  CreateControls;
  SetActivePage;
end;

destructor TcxfmVerticalGridReportLinkDesignWindow.Destroy;
begin
  dxPSPopupMan.dxPSPopupMenuController.UnregisterControl(lbxStyles);
  inherited Destroy;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.LookAndFeelChange(Sender: TObject);
begin
  if not LockControlsUpdate then
  begin
    ReportLink.OptionsFormatting.LookAndFeelKind := TcxLookAndFeelKind(TcxComboBox(Sender).ItemObject);
    Modified := True;
    UpdatePreview;
  end;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.OptionsExpandingClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsExpandingByIndex(TTagToInt(Tag), Checked);
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.OptionsFormattingClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsFormattingByIndex(TTagToInt(Tag), Checked);
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.OptionsRefinementsClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsRefinementsByIndex(TTagToInt(Tag), Checked);
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.OptionsPaginationClick(
  Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsPaginationByIndex(TTagToInt(Tag), Checked);
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.OptionsSizeClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsSizeByIndex(TTagToInt(Tag), Checked);
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.OptionsViewClick(Sender: TObject);
begin
  with TcxCheckBox(Sender) do
    SetOptionsViewByIndex(TTagToInt(Tag), Checked);
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.cbxPrintModeChange(Sender: TObject);
begin
  if not LockControlsUpdate then
  begin
    ReportLink.OptionsView.Mode :=
      TcxVerticalGridReportLinkPrintMode(TcxComboBox(Sender).ItemObject);
    Modified := True;
  end;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.pmStylesPopup(Sender: TObject);
begin
  lbxStyles.HideToolTips;
  miStyleColor.Enabled := AreNativeStylesAvailable and HasSelectedStyles;
  miStyleFont.Enabled := AreNativeStylesAvailable and HasSelectedStyles;
  miStyleBackgroundBitmap.Enabled := AreNativeStylesAvailable and HasSelectedStyles;
  miStyleBackgroundBitmapClear.Enabled := AreNativeStylesAvailable and HasSelectedStylesWithAssignedBitmap;
  miStyleRestoreDefaults.Enabled := AreNativeStylesAvailable and HasSelectedStyles;
  miStylesSelectAll.Enabled := CanSelectAllStyles;
  miStylesSaveAs.Enabled := CanSaveStyles;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.lblUseNativeStylesClick(
  Sender: TObject);
begin
  if chbxUseNativeStyles.CanFocus then ActiveControl := chbxUseNativeStyles;
  chbxUseNativeStyles.Checked := not chbxUseNativeStyles.Checked;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.StyleColorClick(Sender: TObject);
begin
  PerformStylesChangeColor;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.StyleFontClick(Sender: TObject);
begin
  PerformStylesChangeFont;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.StyleBackgroundBitmapClick(
  Sender: TObject);
begin
  PerformStylesChangeBitmap;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.StyleBackgroundBitmapClearClick(
  Sender: TObject);
begin
  PerformStylesClearBitmap;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.StyleRestoreDefaultsClick(
  Sender: TObject);
begin
  PerformStylesRestoreDefaults;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.DoInitialize;
begin
  lbxStyles.ReportLinkStyles := ReportLink.ActiveStyles;
  inherited;
  RefreshStylesList;
  LoadDataIntoPreviewVerticalGrid;
  InitializePreviewVerticalGridStyles;

 (*
  chbxTransparentRichEdits.Visible := False;
 *)
  with ReportLink.OptionsView do
  begin
    chbxShowBorders.Checked := Borders;
    chbxShowExpandButtons.Checked := ExpandButtons;
    chbxShowHeaders.Checked := Headers;
  end;
  ReportLink.InitializePrintModeControl(cbxPrintMode);
  with cbxLookAndFeel.Properties do
  begin
    cbxLookAndFeel.Clear;
    Items.AddObject(cxGetResourceString(@sdxLookAndFeelFlat), TObject(lfFlat));
    Items.AddObject(cxGetResourceString(@sdxLookAndFeelStandard), TObject(lfStandard));
    Items.AddObject(cxGetResourceString(@sdxLookAndFeelUltraFlat), TObject(lfUltraFlat));
    cbxLookAndFeel.ItemIndex := Items.IndexOfObject(TObject(ReportLink.OptionsFormatting.LookAndFeelKind));
  end;

  with ReportLink.OptionsFormatting do
  begin
    chbxSuppressBackgroundBitmaps.Checked := SuppressBackgroundBitmaps;
    chbxUseNativeStyles.Checked := UseNativeStyles;
  end;

  with ReportLink.OptionsExpanding do
  begin
    chbxExpandRows.Checked := AutoExpandRows;
  end;

  with ReportLink.OptionsSize do
  begin
    chbxAutoWidth.Checked := AutoWidth;
    chbxBestFit.Checked := BestFit;
    chbxKeepSameRecordWidths.Checked := KeepSameRecordWidths;
    chbxWrapRecords.Checked := WrapRecords;
  end;

  with ReportLink.OptionsRefinements do
  begin
    chbxTransparentGraphics.Checked := TransparentGraphics;
    chbxDisplayGraphicsAsText.Checked := DisplayGraphicsAsText;
    chbxDisplayTrackBarsAsText.Checked := DisplayTrackBarsAsText;
    chbxFlatCheckMarks.Checked := FlatCheckMarks;
   (*
    chbxTransparentRichEdits.Checked := TransparentRichEdits;
   *)
  end;

  with ReportLink.OptionsPagination do
  begin
    chbxPaginateByRows.Checked := ByRows;
    chbxPaginateByWrapping.Checked := ByWrapping;
    chbxOneWrappingPerPage.Checked := OneWrappingPerPage;
  end;
end;

function TcxfmVerticalGridReportLinkDesignWindow.GetPreviewHost: TCustomPanel;
begin
  Result := pnlPreview;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.LoadStrings;
begin
  inherited;

  lblPreviewWindow.Caption := DropAmpersand(cxGetResourceString(@sdxPreview));

  tshView.Caption := cxGetResourceString(@sdxViewTab);

  lblShow.Caption := cxGetResourceString(@sdxShow);
  chbxShowHeaders.Caption := cxGetResourceString(@sdxHeaders);
  chbxShowBorders.Caption := cxGetResourceString(@sdxBorders);
  chbxShowExpandButtons.Caption := cxGetResourceString(@sdxExpandButtons);
  lblPrintMode.Caption := cxGetResourceString(@sdxMode);

  tshBehaviors.Caption := cxGetResourceString(@sdxBehaviorsTab);

  lblExpanding.Caption := cxGetResourceString(@sdxExpanding);
  chbxExpandRows.Caption := cxGetResourceString(@sdxRows);

  lblSize.Caption := cxGetResourceString(@sdxSize);
  chbxAutoWidth.Caption := cxGetResourceString(@sdxAutoWidth);
  chbxBestFit.Caption := cxGetResourceString(@sdxBestFit);
  chbxKeepSameRecordWidths.Caption := cxGetResourceString(@sdxKeepSameRecordWidths);
  chbxWrapRecords.Caption := cxGetResourceString(@sdxWrapRecords);

  tshFormatting.Caption := cxGetResourceString(@sdxFormatting);
  lblLookAndFeel.Caption := cxGetResourceString(@sdxLookAndFeel);

  lblRefinements.Caption := cxGetResourceString(@sdxRefinements);
  chbxTransparentGraphics.Caption := cxGetResourceString(@sdxTransparentGraphics);
  chbxDisplayGraphicsAsText.Caption := DropAmpersand(cxGetResourceString(@sdxDisplayGraphicsAsText));
  chbxDisplayTrackBarsAsText.Caption := DropAmpersand(cxGetResourceString(@sdxDisplayTrackBarsAsText));
  chbxFlatCheckMarks.Caption := cxGetResourceString(@sdxFlatCheckMarks);
  //chbxTransparentRichEdits.Caption := cxGetResourceString(@sdxTransparentRichEdits);  {.3}
  chbxSuppressBackgroundBitmaps.Caption := cxGetResourceString(@sdxSuppressBackgroundBitmaps);

  lblPagination.Caption := cxGetResourceString(@sdxPagination);
  chbxPaginateByRows.Caption := cxGetResourceString(@sdxByRows);
  chbxPaginateByWrapping.Caption := cxGetResourceString(@sdxByWrapping);
  chbxOneWrappingPerPage.Caption := cxGetResourceString(@sdxOneWrappingPerPage);

  tshStyles.Caption := cxGetResourceString(@sdxStyles);
  lblStyleSheets.Caption := cxGetResourceString(@sdxStyleSheets);
  lblUseNativeStyles.Caption := cxGetResourceString(@sdxUseNativeStyles);
  btnStyleColor.Caption := cxGetResourceString(@sdxBtnColor);
  btnStyleFont.Caption := cxGetResourceString(@sdxBtnFont);
  btnStyleBackgroundBitmap.Caption := cxGetResourceString(@sdxBtnTexture);
  btnStyleBackgroundBitmapClear.Caption := cxGetResourceString(@sdxBtnTextureClear);
  btnStyleRestoreDefaults.Caption := cxGetResourceString(@sdxBtnRestoreDefaults);
  btnStylesSaveAs.Caption := cxGetResourceString(@sdxBtnSaveAs);
  btnStyleSheetNew.Caption := cxGetResourceString(@sdxBtnNew);
  btnStyleSheetCopy.Caption := cxGetResourceString(@sdxBtnCopy);
  btnStyleSheetDelete.Caption := cxGetResourceString(@sdxBtnDelete);
  btnStyleSheetRename.Caption := cxGetResourceString(@sdxBtnRename);

  miStyleColor.Caption := cxGetResourceString(@sdxBtnColor);
  miStyleFont.Caption := cxGetResourceString(@sdxBtnFont);
  miStyleBackgroundBitmap.Caption := cxGetResourceString(@sdxBtnTexture);
  miStyleBackgroundBitmapClear.Caption := cxGetResourceString(@sdxBtnTextureClear);
  miStyleRestoreDefaults.Caption := cxGetResourceString(@sdxBtnRestoreDefaults);
  miStylesSelectAll.Caption := cxGetResourceString(@sdxSelectAll);
  miStylesSaveAs.Caption := cxGetResourceString(@sdxBtnSaveAs);
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.UpdateControlsState;
begin
  inherited;
  ReportLink.UpdateStatePrintModeControl(cbxPrintMode, lblPrintMode);
  bvlMultipleRecords.Visible := lblPrintMode.Visible;

  chbxBestFit.Enabled := not chbxAutoWidth.Checked;
  chbxKeepSameRecordWidths.Enabled := chbxBestFit.Enabled and chbxBestFit.Checked;
  chbxOneWrappingPerPage.Enabled := chbxPaginateByWrapping.Checked;

  lbxStyles.Enabled := AreNativeStylesAvailable;
  btnStyleColor.Enabled := AreNativeStylesAvailable and HasSelectedStyles;
  btnStyleFont.Enabled := AreNativeStylesAvailable and HasSelectedStyles;
  btnStyleBackgroundBitmap.Enabled := AreNativeStylesAvailable and HasSelectedStyles;
  btnStyleBackgroundBitmapClear.Enabled := AreNativeStylesAvailable and HasSelectedStylesWithAssignedBitmap;
  btnStyleRestoreDefaults.Enabled := AreNativeStylesAvailable and HasSelectedStyles;
  btnStylesSaveAs.Enabled := CanSaveStyles;

  lblStyleSheets.Enabled := AreNativeStylesAvailable;
  cbxStyleSheets.Enabled := AreNativeStylesAvailable;
  btnStyleSheetNew.Enabled := CanCreateStyleSheet;
  btnStyleSheetCopy.Enabled := CanCopyStyleSheet;
  btnStyleSheetDelete.Enabled := CanDeleteStyleSheet;
  btnStyleSheetRename.Enabled := CanRenameStyleSheet;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.UpdatePreview;
const
  TransparentsMap: array[Boolean] of TcxImageTransparency = (gtOpaque, gtTransparent);
begin
  PreviewVGrid.Enabled := False;
  PreviewVGrid.Font := ReportLink.Font;
  dxSetupPreviewControlLookAndFeel(PreviewVGrid.LookAndFeel, ReportLink.OptionsFormatting.LookAndFeelKind, ReportLink.VerticalGrid);
  CustomVerticalGrid_GetCategoryFont(PreviewVGrid).Assign(ReportLink.CategoryFont);

  TcxImageProperties(rowPicture.Properties.EditProperties).GraphicTransparency :=
    TransparentsMap[ReportLink.OptionsRefinements.TransparentGraphics];

  PreviewVGrid.OptionsView.ShowHeaders := ReportLink.OptionsView.Headers;
  PreviewVGrid.OptionsView.ShowButtons := ReportLink.OptionsView.ExpandButtons;
  PreviewVGrid.Invalidate;
end;

function TcxfmVerticalGridReportLinkDesignWindow.GetDesignerTabIndex: Integer;
begin
  Result := pcMain.ItemIndex;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.SetDesignerTabIndex(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value > pcMain.Count - 1 then
    Value := pcMain.Count - 1;
  pcMain.ItemIndex := Value;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.DoActiveStyleSheetChanged;
begin
  lbxStyles.ReportLinkStyles := ReportLink.ActiveStyles;
  inherited;
  cbxStyleSheets.ItemIndex := cbxStyleSheets.Properties.Items.IndexOfObject(ActiveStyleSheet);

  if not LockControlsUpdate then
  begin
    Modified := True;
    UpdatePreview;
  end;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.DoFormActivated(AnActive: Boolean);
begin
  inherited;
  if not AnActive then lbxStyles.HideToolTips;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.DoRefreshStylesList;
var
  Styles: TcxVerticalGridReportLinkStyles;
  List: TList;
begin
  Styles := ReportLink.ActiveStyles;
  with lbxStyles.Items do
  begin
    BeginUpdate;
    try
      List := TList.Create;
      try
        SaveSelectedStyles(List);
        try
          Clear;
          AddObject(cxGetResourceString(@sdxCategoryStyle), Styles.Category);
          AddObject(cxGetResourceString(@sdxContentStyle), Styles.Content);
          AddObject(cxGetResourceString(@sdxHeaderStyle), Styles.Header);
        finally
          RestoreSelectedStyles(List);
        end;
      finally
        List.Free;
      end;
    finally
      EndUpdate;
    end;
  end;
  InitializePreviewVerticalGridStyles;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.DoStyleChanged(const ACaption: string;
  AStyle: TcxStyle);
begin
  inherited;
  UpdatePreviewVerticalGridStyles(ACaption, AStyle);
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.DoStylesChanged(AStrings: TStrings;
  ARecreate: Boolean);
begin
  if ARecreate then
    RecreateStylesListBox
  else
    lbxStyles.Invalidate;
  Modified := True;
  UpdatePreview;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.GetSelectedStyleNames(AStrings: TStrings);
var
  I: Integer;
begin
  AStrings.Clear;
  with lbxStyles do
    for I := 0 to Items.Count - 1 do
      if Selected[I] then
        AStrings.AddObject(Items[I], Items.Objects[I]);
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.GetStyleSheetNames(out AStrings: TStrings);
begin
  AStrings := cbxStyleSheets.Properties.Items;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.GetStyleNames(out AStrings: TStrings);
begin
  AStrings := lbxStyles.Items;
end;

function TcxfmVerticalGridReportLinkDesignWindow.GetActiveStyle: TcxStyle;
begin
  with lbxStyles do
    if ItemIndex = -1 then
      Result := nil
    else
      Result := TcxStyle(Items.Objects[ItemIndex]);
end;

function TcxfmVerticalGridReportLinkDesignWindow.GetComponent: TcxCustomVerticalGrid;
begin
  Result := inherited Component as TcxCustomVerticalGrid;
end;

function TcxfmVerticalGridReportLinkDesignWindow.GetHasSelectedStyles: Boolean;
begin
  Result := lbxStyles.SelCount <> 0;
end;

function TcxfmVerticalGridReportLinkDesignWindow.GetHasSelectedStylesWithAssignedBitmap: Boolean;
var
  Styles: TStrings;
  I: Integer;
  cxStyle: TcxStyle;
begin
  Result := True;
  Styles := TStringList.Create;
  try
    GetSelectedStyleNames(Styles);
    for I := 0 to Styles.Count - 1 do
    begin
      cxStyle := TcxStyle(Styles.Objects[I]);
      if (cxStyle <> nil) and (cxStyle.Bitmap <> nil) and not cxStyle.Bitmap.Empty then
        Exit;
    end;
  finally
    Styles.Free;
  end;
  Result := False;
end;

function TcxfmVerticalGridReportLinkDesignWindow.GetReportLink: TcxCustomVerticalGridReportLink;
begin
  Result := inherited ReportLink as TcxCustomVerticalGridReportLink;
end;

function TcxfmVerticalGridReportLinkDesignWindow.CanSelectAllStyles: Boolean;
var
  I: Integer;
begin
  Result := AreNativeStylesAvailable;
  if Result then
  begin
    for I := 0 to lbxStyles.Items.Count - 1 do
      if not lbxStyles.Selected[I] then Exit;
    Result := False;
  end;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.CreateControls;

  procedure CreateStylesListBox;
  begin
    lbxStyles := TdxStylesListBox.Create(Self);
    lbxStyles.PopupMenu := pmStyles;
    lbxStyles.TabOrder := chbxUseNativeStyles.TabOrder + 1;
    lbxStyles.OnClick := lbxStylesClick;
    bvlStylesHost.Control := lbxStyles;
    dxPSPopupMan.dxPSPopupMenuController.RegisterControl(lbxStyles);
  end;

begin
  CreateStylesListBox;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.CustomDrawTextRect(ACanvas: TcxCanvas;
  AViewInfo: TcxRowValueInfo; const AText: string);
const
  AlignmentHorz = cxAlignLeft;
  AlignmentVert = cxAlignTop;
var
  R: TRect;
begin
  with AViewInfo do
  begin
    R := BoundsRect;

    if ViewParams.Bitmap = nil then
    begin
      ACanvas.Brush.Color := ViewParams.Color;
      ACanvas.FillRect(R);
    end
    else
      cxBkgndDrawPicture(ViewParams.Bitmap, ACanvas.Canvas, R, ppmTile, 1, 1, -R.Left, -R.Top);

    InflateRect(R, -2, -1);
    if ViewParams.Font <> nil then
      ACanvas.Font := ViewParams.Font;
    ACanvas.Font.Color := ViewParams.TextColor;
    ACanvas.Brush.Style := bsClear;
    ACanvas.DrawText(AText, R, AlignmentHorz or AlignmentVert or cxSingleLine);
    ACanvas.Brush.Style := bsSolid;
  end;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.InitializePreviewVerticalGridStyles;

  procedure ResetEvents(AStyles: TcxVerticalGridStyles);
  begin
    with AStyles do
    begin
      OnGetCategoryStyle := nil;
      OnGetHeaderStyle := nil;
      OnGetContentStyle := nil;
    end;
  end;

var
  Styles: TcxVerticalGridReportLinkStyles;
begin
  if ReportLink.OptionsFormatting.UseNativeStyles then
  begin
    dxPScxCommon.dxPSResetStyles(PreviewVGrid.Styles);

    Styles := ReportLink.ActiveStyles;
    with PreviewVGrid.Styles do
    begin
      Category := Styles.Category;
      Content := Styles.Content;
      Header := Styles.Header;
    end;
  end
  else
    if Component <> nil then
      PreviewVGrid.Styles := Component.Styles
    else
      dxPScxCommon.dxPSResetStyles(PreviewVGrid.Styles);

  ResetEvents(PreviewVGrid.Styles);
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.LoadDataIntoPreviewVerticalGrid;
const
  RowCount = 6;
  RowEditProperties: array[0..RowCount - 1] of TcxCustomEditPropertiesClass =
    (TcxTextEditProperties, TcxTextEditProperties, TcxImageProperties,
     TcxMemoProperties, TcxTextEditProperties, TcxMemoProperties);
  RowHeights: array[0..RowCount - 1] of Integer =
    (-1, -1, 116, 57, -1, 44);

  function RowCaptionString(Index: Integer): string;
  begin
    case Index of
      0: Result := cxGetResourceString(@sdxCarManufacturer);
      1: Result := cxGetResourceString(@sdxCarModel);
      2: Result := cxGetResourceString(@sdxPicture);
      3: Result := cxGetResourceString(@sdxCarEngine);
      4: Result := cxGetResourceString(@sdxCarTransmission);
    else
      Result := cxGetResourceString(@sdxCarTires);
    end;
  end;

  function RowValueString(Index: Integer): string;
  begin
    case Index of
      0: Result := cxGetResourceString(@sdx760V12Manufacturer);
      1: Result := cxGetResourceString(@sdx760V12Model);
      2: Result := '';
      3: Result := cxGetResourceString(@sdx760V12Engine);
      4: Result := cxGetResourceString(@sdx760V12Transmission);
    else
      Result := cxGetResourceString(@sdx760V12Tires);
    end;
  end;

var
  I: Integer;
  S: AnsiString;
begin
  rowLuxurySedan.Properties.Caption := cxGetResourceString(@sdxLuxurySedans);
  for I := 0 to rowLuxurySedan.Count - 1 do
    with TcxEditorRow(rowLuxurySedan.Rows[I]) do
    begin
      Properties.Caption := DropAmpersand(RowCaptionString(I));
      Properties.EditPropertiesClass := RowEditProperties[I];
      if RowValueString(I) <> '' then
        Properties.Value := RowValueString(I);
      if RowHeights[I] <> -1 then
        Height := RowHeights[I];
    end;
  cxImage.SavePicture(PreviewImage, S);
  rowPicture.Properties.Value := S;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.LoadGroupsIcons;
begin
  inherited LoadGroupsIcons;
  dxLoadImageListFromResources(ilStylesPopup, IDIL_DXPSSTYLESMENU);
  dxLoadIconFromResourceEx(imgExpanding, IDB_DXPSGROUPICON_EXPANDING);
  dxLoadIconFromResourceEx(imgShow, IDB_DXPSGROUPICON_SHOW);
  dxLoadIconFromResourceEx(imgGridSize, IDB_DXPSGROUPICON_SIZE);
  dxLoadIconFromResourceEx(imgLookAndFeel, IDB_DXPSGROUPICON_LOOKANDFEEL);
  dxLoadIconFromResourceEx(imgRefinements, IDB_DXPSGROUPICON_REFINEMENTS);
  dxLoadIconFromResourceEx(imgPagination, IDB_DXPSGROUPICON_PAGINATION);
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.RecreateStylesListBox;
var
  List: TList;
begin
  List := TList.Create;
  try
    SaveSelectedStyles(List);
    cxRecreateControlWnd(lbxStyles);
    RestoreSelectedStyles(List);
  finally
    List.Free;
  end;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.RestoreSelectedStyles(AList: TList);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    lbxStyles.Selected[Integer(AList[I])] := True;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.SaveSelectedStyles(AList: TList);
var
  I: Integer;
begin
  AList.Clear;
  for I := 0 to lbxStyles.Items.Count - 1 do
    if lbxStyles.Selected[I] then AList.Add(TObject(I));
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.SetActivePage;
begin
  pcMain.ItemIndex := DesignerTabIndex;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.UpdatePreviewVerticalGridStyles(const ACaption: string;
  AStyle: TcxStyle);
begin
  with PreviewVGrid.Styles do
  begin
    if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxCategoryStyle)) then
      Category := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxContentStyle)) then
      Content := AStyle
    else if dxPSUtl.dxSameText(ACaption, cxGetResourceString(@sdxHeaderStyle)) then
      Header := AStyle;
  end;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.SetOptionsExpandingByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;
  with ReportLink.OptionsExpanding do
    case Index of
      0: AutoExpandRows := Value;
    end;
  Modified := True;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.SetOptionsFormattingByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;
  with ReportLink.OptionsFormatting do
    case Index of
      0:
         begin
           UseNativeStyles := Value;
           InitializePreviewVerticalGridStyles;
         end;
      1: SuppressBackgroundBitmaps := Value;
    end;
  Modified := True;
  UpdatePreview;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.SetOptionsPaginationByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;
  with ReportLink.OptionsPagination do
    case Index of
      0: ByRows := Value;
      1: ByWrapping := Value;
      2: OneWrappingPerPage := Value;
    end;
  Modified := True;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.SetOptionsRefinementsByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;
  with ReportLink.OptionsRefinements do
    case Index of
      0: TransparentGraphics := Value;
      1: DisplayGraphicsAsText := Value;
      2: FlatCheckMarks := Value;
      3: TransparentRichEdits := Value;
      4: DisplayTrackBarsAsText := Value;
    end;
  Modified := True;
  UpdatePreview;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.SetOptionsSizeByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;
  with ReportLink.OptionsSize do
    case Index of
      0: AutoWidth := Value;
      1: BestFit := Value;
      2: KeepSameRecordWidths := Value;
      3: WrapRecords := Value;
    end;
  Modified := True;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.SetOptionsViewByIndex(Index: Integer;
  Value: Boolean);
begin
  if LockControlsUpdate then Exit;
  with ReportLink.OptionsView do
    case Index of
      0: Headers := Value;
      1: ExpandButtons := Value;
      2: Borders := Value;
    end;
  Modified := True;
  UpdatePreview;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.PreviewVGridDrawValue(
  Sender: TObject; ACanvas: TcxCanvas; APainter: TcxvgPainter;
  AValueInfo: TcxRowValueInfo; var Done: Boolean);
begin
  if AValueInfo.Row = rowPicture then
    with ReportLink.OptionsRefinements do
      if DisplayGraphicsAsText then
      begin
        CustomDrawTextRect(ACanvas, AValueInfo, GraphicsText);
        Done := True;
      end;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.cbxStyleSheetsClick(Sender: TObject);
begin
  ActiveStyleSheet := TcxCustomStyleSheet(TcxComboBox(Sender).ItemObject);
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.btnStyleSheetNewClick(Sender: TObject);
begin
  PerformStyleSheetNew;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.btnStyleSheetCopyClick(
  Sender: TObject);
begin
  PerformStyleSheetCopy;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.btnStyleSheetDeleteClick(Sender: TObject);
begin
  PerformStyleSheetDelete;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.btnStyleSheetRenameClick(Sender: TObject);
begin
  PerformStyleSheetRename;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.miStylesSelectAllClick(Sender: TObject);
begin
  lbxStyles.SelectAll;
  UpdateControlsState;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.StylesSaveAsClick(Sender: TObject);
begin
  PerformStylesSaveAsStyleSheet;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.cbxStyleSheetsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  PerformStyleSheetKeyDown(Sender, Key, Shift);
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.lbxStylesClick(Sender: TObject);
begin
  if not LockControlsUpdate then
    UpdateControlsState;
end;

procedure TcxfmVerticalGridReportLinkDesignWindow.cbxStyleSheetsPropertiesDrawItem(
  AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer;
  const ARect: TRect; AState: TOwnerDrawState);
begin
  PerformStyleSheetDrawItem(ACanvas.Canvas, AIndex, ARect, AState, AControl.Enabled);
end;

procedure RegisterAssistants;
begin
  TcxVerticalGridCustomRowHelper.Register;
  TcxVerticalGridCategoryRowHelper.Register;
  TcxVerticalGridCustomEditorRowHelper.Register;
  TcxVerticalGridEditorRowHelper.Register;
  TcxVerticalGridDBEditorRowHelper.Register;
  TcxVerticalGridCustomMultiEditorRowHelper.Register;

  TcxVerticalGridReportLinkStyleSheet.Register;
end;

procedure UnregisterAssistants;
begin
  TcxVerticalGridReportLinkStyleSheet.Unregister;

  TcxVerticalGridCustomMultiEditorRowHelper.Unregister;
  TcxVerticalGridDBEditorRowHelper.Unregister;
  TcxVerticalGridEditorRowHelper.Unregister;
  TcxVerticalGridCustomEditorRowHelper.Unregister;
  TcxVerticalGridCategoryRowHelper.Unregister;
  TcxVerticalGridCustomRowHelper.Unregister;

  TcxVerticalGridRowHelperFactory.ReleaseInstance;
end;

procedure RegisterVGridReportLink(AReportLinkClass: TdxReportLinkClass; AVGridClass: TComponentClass);
begin
  dxPSRegisterReportLink(AReportLinkClass, AVGridClass, TcxfmVerticalGridReportLinkDesignWindow);
end;

procedure UnregisterVGridReportLink(AReportLinkClass: TdxReportLinkClass; AVGridClass: TComponentClass);
begin
  dxPSUnregisterReportLink(AReportLinkClass, AVGridClass, TcxfmVerticalGridReportLinkDesignWindow);
end;

initialization
  RegisterAssistants;

  RegisterVGridReportLink(TcxVerticalGridReportLink, TcxVerticalGrid);
  RegisterVGridReportLink(TcxRTTIInspectorReportLink, TcxRTTIInspector);
  RegisterVGridReportLink(TcxDBVerticalGridReportLink, TcxDBVerticalGrid);
  RegisterVGridReportLink(TcxVirtualVerticalGridReportLink, TcxVirtualVerticalGrid);

finalization
  UnregisterVGridReportLink(TcxVirtualVerticalGridReportLink, TcxVirtualVerticalGrid);
  UnregisterVGridReportLink(TcxDBVerticalGridReportLink, TcxDBVerticalGrid);
  UnregisterVGridReportLink(TcxRTTIInspectorReportLink, TcxRTTIInspector);
  UnregisterVGridReportLink(TcxVerticalGridReportLink, TcxVerticalGrid);

  UnregisterAssistants;
  FreeAndNil(FDefaultdxPScxVerticalGridLinkStyleSheet);
  FreeAndNil(FBMW760Li);

end.
