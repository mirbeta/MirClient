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

unit dxRichEdit.Import.OpenXML.DestinationTable;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxGenerics,
  dxXMLReader,
  dxRichEdit.Utils.Types,
  dxRichEdit.Platform.Font,
  dxRichEdit.Options,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter;

type

  { TdxTableDestination }

  TdxTableDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    FTable: TdxTable;
    FTableGrid: TdxIntegerList;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableDestination; static;
    function GetElementHandlerTable: TdxElementHandlerTable; override;

    class function OnRow(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnStructuredDocument(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnCustomXml(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableGrid(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;

    property Table: TdxTable read FTable;
    property TableGrid: TdxIntegerList read FTableGrid;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AParentCell: TdxTableCell = nil);
    destructor Destroy; override;
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    procedure EnsureTableCellsWidth(ATable: TdxTable);
    procedure EnsureRowCellsWidth(ATableRow: TdxTableRow);
    function CalculateCellWidth(AColumnIndex: Integer; AColumnSpan: Integer): Integer;
  end;

  { TdxTableGridDestination }

  TdxTableGridDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FTableGrid: TdxIntegerList;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableGridDestination; static;
    class function OnColumn(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ATableGrid: TdxIntegerList);
  end;

  { TdxGridColumnDestination }

  TdxGridColumnDestination = class(TdxElementDestination)
  strict private
    FTableGrid: TdxIntegerList;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ATableGrid: TdxIntegerList);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableRowDestination }

  TdxTableRowDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    FRow: TdxTableRow;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableRowDestination; static;
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnCell(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableRowProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTablePropertiesException(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnCustomXml(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;

    property Row: TdxTableRow read FRow;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ATable: TdxTable);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;

    class function OnStructuredDocument(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxTableCellDestination }

  TdxTableCellDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableCellDestination; static;
    class function OnParagraph(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTable(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnCellProperies(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  protected
    FCell: TdxTableCell;
    FStartParagraphIndex: TdxParagraphIndex;
    FEndParagraphIndex: TdxParagraphIndex;
    class function OnBookmarkStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnBookmarkEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRangePermissionStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRangePermissionEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnStructuredDocument(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnCustomXml(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    function GetElementHandlerTable: TdxElementHandlerTable; override;

    property Cell: TdxTableCell read FCell;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ARow: TdxTableRow);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;

    property StartParagraphIndex: TdxParagraphIndex read FStartParagraphIndex write FStartParagraphIndex;
    property EndParagraphIndex: TdxParagraphIndex read FEndParagraphIndex write FEndParagraphIndex;
  end;

  { TdxTablePropertiesBaseDestination }

  TdxTablePropertiesBaseDestination<T: class, IdxPropertiesContainer> = class abstract(TdxElementDestination)
  strict private
    FProperties: T;
  protected
    property Properties: T read FProperties;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AProperties: T);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxTablePropertiesDestinationCore }

  TdxTablePropertiesDestinationCore = class(TdxTablePropertiesBaseDestination<TdxTableProperties>)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTablePropertiesDestinationCore; static;
    class function GetProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableProperties; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnTableWidth(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableIndent(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableLook(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableBorders(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableCellMarginBorders(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableCellCellSpacing(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableLayout(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableAlignment(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableOverlap(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableFloatingPosition(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableStyleColBandSize(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableStyleRowBandSize(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableBackground(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableAvoidDoubleBorders(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;

  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxTablePropertiesDestination }

  TdxTablePropertiesDestination = class(TdxTablePropertiesDestinationCore)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FTable: TdxTable;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTablePropertiesDestination; static;
    class function OnTableStyle(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;

  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ATable: TdxTable);

    property Table: TdxTable read FTable;
  end;

  { TdxTableRowPropertiesDestination }

  TdxTableRowPropertiesDestination = class(TdxTablePropertiesBaseDestination<TdxTableRowProperties>)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableRowPropertiesDestination; static;
    class function GetProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableRowProperties; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnTableRowCantSplit(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableRowGridAfter(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableRowGridBefore(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableRowHideCellMark(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableRowAlignment(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableRowCellSpacing(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableRowHeader(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableRowHeight(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableRowWidthAfter(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableRowWidthBefore(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableRowConditionalFormatting(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxTableCellPropertiesDestinationCore }

  TdxTableCellPropertiesDestinationCore = class(TdxTablePropertiesBaseDestination<TdxTableCellProperties>)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableCellPropertiesDestinationCore; static;
    class function GetProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableCellProperties; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnTableCellWidth(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableCellBorders(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableCellMerge(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableCellColumnSpan(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableCellShading(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableCellFitText(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableCellNoWrap(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableCellMargins(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableCellHideMark(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableCellTextDirection(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableCellVerticalAlignment(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTableCellConditionalFormatting(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;

  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxTableCellPropertiesDestination }

  TdxTableCellPropertiesDestination = class(TdxTableCellPropertiesDestinationCore)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FCell: TdxTableCell;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableCellPropertiesDestination; static;
    class function OnTableCellStyle(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;

    property Cell: TdxTableCell read FCell;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ACell: TdxTableCell);
  end;

  { TdxWidthUnitDestination }

  TdxWidthUnitDestination = class(TdxLeafElementDestination)
  strict private
    class var
      FWidthUnitTypeTable: TdxEnumeratedDictionary<TdxWidthUnitType, string>;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateWidthUnitTypeTable: TdxEnumeratedDictionary<TdxWidthUnitType, string>; static;
  strict private
    FWidthUnit: TdxWidthUnit;
  protected
    function IsValid(AValue: Integer): Boolean; virtual;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AWidthUnit: TdxWidthUnit);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxWidthUnitNonNegativeDestination }

  TdxWidthUnitNonNegativeDestination = class(TdxWidthUnitDestination)
  protected
    function IsValid(AValue: Integer): Boolean; override;
  end;

  { TdxHeightUnitDestination }

  TdxHeightUnitDestination = class(TdxLeafElementDestination)
  strict private
    class var
      FHeightUnitTypeTable: TdxEnumeratedDictionary<TdxHeightUnitType, string>;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateHeightUnitTypeTable: TdxEnumeratedDictionary<TdxHeightUnitType, string>; static;
  strict private
    FHeightUnit: TdxHeightUnit;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AHeightUnit: TdxHeightUnit);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableStyleDestination }

  TdxTableStyleDestination = class(TdxLeafElementDestination)
  strict private
    FTable: TdxTable;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ATable: TdxTable);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableCellStyleDestination }

  TdxTableCellStyleDestination = class(TdxLeafElementDestination)
  strict private
    FCell: TdxTableCell;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ACell: TdxTableCell);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTablePropertiesElementBaseDestination }

  TdxTablePropertiesElementBaseDestination = class abstract(TdxElementDestination)
  strict private
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTablePropertiesElementBaseDestination; static;
  protected
    FTableProperties: TdxTableProperties;
    class function GetProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableProperties; static;

    property TableProperties: TdxTableProperties read FTableProperties;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ATableProperties: TdxTableProperties);
  end;

  { TdxTablePropertiesLeafElementDestination }

  TdxTablePropertiesLeafElementDestination = class abstract(TdxTablePropertiesElementBaseDestination)
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  end;

  { TdxTableLookDestination }

  TdxTableLookDestination = class(TdxTablePropertiesLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableBordersDestination }

  TdxTableBordersDestination = class(TdxTablePropertiesElementBaseDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    class function GetTableBorders(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableBorders; static;
    class function OnTopBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnLeftBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnBottomBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRightBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnInsideHorizontalBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnInsideVerticalBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  end;

  { TdxTableLayoutDestination }

  TdxTableLayoutDestination = class(TdxTablePropertiesLeafElementDestination)
  strict private
    class var
      FTableLayoutTypeTable: TdxEnumeratedDictionary<TdxTableLayoutType, string>;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateTableLayoutTypeTable: TdxEnumeratedDictionary<TdxTableLayoutType, string>; static;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableAlignmentDestination }

  TdxTableAlignmentDestination = class(TdxTablePropertiesLeafElementDestination)
  strict private
    class var
      FTableRowAlignmentTable: TdxEnumeratedDictionary<TdxTableRowAlignment, string>;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateTableRowAlignmentTable: TdxEnumeratedDictionary<TdxTableRowAlignment, string>; static;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableOverlapDestination }

  TdxTableOverlapDestination = class(TdxTablePropertiesLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableStyleColBandSizeDestination }

  TdxTableStyleColBandSizeDestination = class(TdxTablePropertiesLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableStyleRowBandSizeDestination }

  TdxTableStyleRowBandSizeDestination = class(TdxTablePropertiesLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableBackgroundDestination }

  TdxTableBackgroundDestination = class(TdxTablePropertiesLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableAvoidDoubleBordersDestination }

  TdxTableAvoidDoubleBordersDestination = class(TdxTablePropertiesLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableFloatingPositionDestination }

  TdxTableFloatingPositionDestination = class(TdxLeafElementDestination)
  strict private
    class var
      FHorizontalAnchorTypeTable: TdxEnumeratedDictionary<TdxHorizontalAnchorTypes, string>;
      FVerticalAnchorTypeTable: TdxEnumeratedDictionary<TdxVerticalAnchorTypes, string>;
      FHorizontalAlignModeTable: TdxEnumeratedDictionary<TdxHorizontalAlignMode, string>;
      FVerticalAlignModeTable: TdxEnumeratedDictionary<TdxVerticalAlignMode, string>;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateHorizontalAnchorTypeTable: TdxEnumeratedDictionary<TdxHorizontalAnchorTypes, string>; static;
    class function CreateVerticalAnchorTypeTable: TdxEnumeratedDictionary<TdxVerticalAnchorTypes, string>; static;
    class function CreateHorizontalAlignModeTable: TdxEnumeratedDictionary<TdxHorizontalAlignMode, string>; static;
    class function CreateVerticalAlignModeTable: TdxEnumeratedDictionary<TdxVerticalAlignMode, string>; static;
  strict private
    FFloatingPosition: TdxTableFloatingPosition;
  protected
    function GetHorizontalAnchor(AReader: TdxXmlReader; ADefaultValue: TdxHorizontalAnchorTypes): TdxHorizontalAnchorTypes; virtual;
    function GetVerticalAnchor(AReader: TdxXmlReader; ADefaultValue: TdxVerticalAnchorTypes): TdxVerticalAnchorTypes; virtual;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFloatingPosition: TdxTableFloatingPosition);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableBorderElementDestinationBase }

  TdxTableBorderElementDestinationBase = class(TdxBorderDestination)
  strict private
    FBorder: TdxBorderBase;
  strict protected
    procedure SetBorderOffset(ASpace: Integer);
    procedure SetBorderWidth(ASize: Integer);
    procedure SetBorderLineStyle(ABorderLineStyle: TdxBorderLineStyle);

    property Border: TdxBorderBase read FBorder;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ABorder: TdxBorderBase);
  end;

  { TdxTableCellBorderElementDestination }

  TdxTableCellBorderElementDestination = class(TdxTableBorderElementDestinationBase)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableBorderElementDestination }

  TdxTableBorderElementDestination = class(TdxTableBorderElementDestinationBase)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableRowPropertiesElementBaseDestination }

  TdxTableRowPropertiesElementBaseDestination = class abstract(TdxElementDestination)
  strict private
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableRowPropertiesElementBaseDestination; static;
  protected
    FRowProperties: TdxTableRowProperties;
    class function GetProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableRowProperties; static;

    property RowProperties: TdxTableRowProperties read FRowProperties;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ARowProperties: TdxTableRowProperties);
  end;

  { TdxTableRowPropertiesLeafElementDestination }

  TdxTableRowPropertiesLeafElementDestination = class abstract(TdxTableRowPropertiesElementBaseDestination)
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  end;

  { TdxTableRowCantSplitDestination }

  TdxTableRowCantSplitDestination = class(TdxTableRowPropertiesLeafElementDestination)
  public
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxTableRowGridAfterDestination }

  TdxTableRowGridAfterDestination = class(TdxTableRowPropertiesLeafElementDestination)
  public
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxTableRowGridBeforeDestination }

  TdxTableRowGridBeforeDestination = class(TdxTableRowPropertiesLeafElementDestination)
  public
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxTableRowHideCellMarkDestination }

  TdxTableRowHideCellMarkDestination = class(TdxTableRowPropertiesLeafElementDestination)
  public
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxTableRowAlignmentDestination }

  TdxTableRowAlignmentDestination = class(TdxTableRowPropertiesLeafElementDestination)
  strict private
    class var
      FTableRowAlignmentTable: TdxEnumeratedDictionary<TdxTableRowAlignment, string>;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateTableRowAlignmentTable: TdxEnumeratedDictionary<TdxTableRowAlignment, string>; static;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableRowHeaderDestination }

  TdxTableRowHeaderDestination = class(TdxTableRowPropertiesLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableRowConditionalFormattingDestination }

  TdxTableRowConditionalFormattingDestination = class(TdxTableRowPropertiesLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableCellMarginsDestination }

  TdxTableCellMarginsDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FCellMargins: TdxCellMargins;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableCellMarginsDestination; static;
    class function GetCellMargins(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxCellMargins; static;
    class function OnTopCellMargin(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnLeftCellMargin(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnBottomCellMargin(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRightCellMargin(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;

    property CellMargins: TdxCellMargins read FCellMargins;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ACellMargins: TdxCellMargins);
  end;

  { TdxTableShadingDestination }

  TdxTableShadingDestination = class(TdxTablePropertiesLeafElementDestination)
  public
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxTableCellPropertiesElementBaseDestination }

  TdxTableCellPropertiesElementBaseDestination = class abstract(TdxElementDestination)
  strict private
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableCellPropertiesElementBaseDestination; static;
  protected
    FCellProperties: TdxTableCellProperties;
    class function GetProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableCellProperties; static;

    property CellProperties: TdxTableCellProperties read FCellProperties;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ACellProperties: TdxTableCellProperties);
  end;

  { TdxTableCellPropertiesLeafElementDestination }

  TdxTableCellPropertiesLeafElementDestination = class abstract(TdxTableCellPropertiesElementBaseDestination)
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  end;

  { TdxTableCellBordersDestination }

  TdxTableCellBordersDestination = class(TdxTableCellPropertiesElementBaseDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    class function GetCellBorders(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableCellBorders; static;
    class function OnTopBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnLeftBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnBottomBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRightBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnInsideHorizontalBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnInsideVerticalBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTopLeftDiagonalBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTopRightDiagonalBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  end;

  { TdxTableCellVerticalMergingStateDestination }

  TdxTableCellVerticalMergingStateDestination = class(TdxTableCellPropertiesLeafElementDestination)
  strict private
    class var
      FMergingStateTable: TdxEnumeratedDictionary<TdxMergingState, string>;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateMergingStateTable: TdxEnumeratedDictionary<TdxMergingState, string>; static;
  public
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxTableCellShadingDestination }

  TdxTableCellShadingDestination = class(TdxTableCellPropertiesLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableCellColumnSpanDestination }

  TdxTableCellColumnSpanDestination = class(TdxTableCellPropertiesLeafElementDestination)
  public
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
  end;

  { TdxTableCellFitTextDestination }

  TdxTableCellFitTextDestination = class(TdxTableCellPropertiesLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableCellNoWrapDestination }

  TdxTableCellNoWrapDestination = class(TdxTableCellPropertiesLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableCellHideMarkDestination }

  TdxTableCellHideMarkDestination = class(TdxTableCellPropertiesLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableCellTextDirectionDestination }

  TdxTableCellTextDirectionDestination = class(TdxTableCellPropertiesLeafElementDestination)
  strict private
    class var
      FTextDirectionTable: TdxEnumeratedDictionary<TdxTextDirection, string>;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateTextDirectionTable: TdxEnumeratedDictionary<TdxTextDirection, string>; static;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableCellVerticalAlignmentDestination }

  TdxTableCellVerticalAlignmentDestination = class(TdxTableCellPropertiesLeafElementDestination)
  strict private
    class var
      FVerticalAlignmentTable: TdxEnumeratedDictionary<TdxVerticalAlignment, string>;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateVerticalAlignmentTable: TdxEnumeratedDictionary<TdxVerticalAlignment, string>; static;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableCellConditionalFormattingDestination }

  TdxTableCellConditionalFormattingDestination = class(TdxTableCellPropertiesLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTableDisabledDestination }

  TdxTableDisabledDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnRow(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxTableRowDisabledDestination }

  TdxTableRowDisabledDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnCell(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxTableCellDisabledDestination }

  TdxTableCellDisabledDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnParagraph(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTable(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnBookmarkStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnBookmarkEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRangePermissionStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRangePermissionEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

implementation

uses
  Math,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Export.OpenXML,
  dxStringHelper,
  dxRichEdit.Utils.NumberParser;

{ TdxTableDestination }

constructor TdxTableDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AParentCell: TdxTableCell);
begin
  inherited Create(AImporter);
  FTable := TdxTable.Create(PieceTable, AParentCell, 0, 0);
  FTableGrid := TdxIntegerList.Create;
end;

destructor TdxTableDestination.Destroy;
begin
  FTableGrid.Free;
  inherited Destroy;
end;

class constructor TdxTableDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTableDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTableDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('tr', OnRow);
  Result.Add('tblPr', OnTableProperties);
  Result.Add('sdt', OnStructuredDocument);
  Result.Add('customXml', OnCustomXml);
  Result.Add('tblGrid', OnTableGrid);
end;

class function TdxTableDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableDestination;
begin
  Result := TdxTableDestination(AImporter.PeekDestination);
end;

function TdxTableDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxTableDestination.OnRow(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
var
  ADestination: TdxTableDestination;
begin
  ADestination := GetThis(AImporter);
  Result := TdxTableRowDestination.Create(AImporter, ADestination.Table);
end;

class function TdxTableDestination.OnTableProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
var
  ADestination: TdxTableDestination;
begin
  ADestination := GetThis(AImporter);
  Result := TdxTablePropertiesDestination.Create(AImporter, ADestination.Table);
end;

class function TdxTableDestination.OnStructuredDocument(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxStructuredDocumentDestination.Create(AImporter);
end;

class function TdxTableDestination.OnCustomXml(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxCustomXmlDestination.Create(AImporter);
end;

class function TdxTableDestination.OnTableGrid(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
var
  ADestination: TdxTableDestination;
begin
  ADestination := GetThis(AImporter);
  Result := TdxTableGridDestination.Create(AImporter, ADestination.TableGrid);
end;

procedure TdxTableDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  TdxWordProcessingMLBaseImporter(Importer).BeginTable;
  Importer.PieceTable.Tables.Add(FTable);
end;

procedure TdxTableDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  if (Table.TableLayout = TdxTableLayoutType.Fixed) and (FTableGrid.Count > 0) then
    EnsureTableCellsWidth(Table);
  TdxWordProcessingMLBaseImporter(Importer).EndTable;
end;

procedure TdxTableDestination.EnsureTableCellsWidth(ATable: TdxTable);
var
  ARowsCount, I: Integer;
begin
  ARowsCount := ATable.Rows.Count;
  for I := 0 to ARowsCount - 1 do
    EnsureRowCellsWidth(ATable.Rows[I]);
end;

procedure TdxTableDestination.EnsureRowCellsWidth(ATableRow: TdxTableRow);
var
  AColumnIndex, ACellsCount, I, ARemainedCellsCount, AMaxAvailableColumnSpan, AColumnSpan: Integer;
  AProperties: TdxTableCellProperties;
begin
  AColumnIndex := 0;
  ACellsCount := ATableRow.Cells.Count;
  for I := 0 to ACellsCount - 1 do
  begin
    AProperties := ATableRow.Cells[I].Properties;
    ARemainedCellsCount := ACellsCount - I - 1;
    AMaxAvailableColumnSpan := TableGrid.Count - AColumnIndex - ARemainedCellsCount;
    AColumnSpan := Min(AMaxAvailableColumnSpan, AProperties.ColumnSpan);
    if (AProperties.PreferredWidth.&Type = TdxWidthUnitType.Nil) or (AProperties.PreferredWidth.&Type = TdxWidthUnitType.Auto) then
    begin
      AProperties.PreferredWidth.Value := CalculateCellWidth(AColumnIndex, AColumnSpan);
      AProperties.PreferredWidth.&Type := TdxWidthUnitType.ModelUnits;
    end;
    Inc(AColumnIndex, AColumnSpan);
  end;
end;

function TdxTableDestination.CalculateCellWidth(AColumnIndex: Integer; AColumnSpan: Integer): Integer;
var
  AResult, AStartIndex, AEndIndex, I: Integer;
begin
  AResult := 0;
  AStartIndex := AColumnIndex;
  AEndIndex := AStartIndex + AColumnSpan - 1;
  for I := AStartIndex to AEndIndex do
    Inc(AResult, TableGrid[I]);
  Result := AResult;
end;

{ TdxTableGridDestination }

constructor TdxTableGridDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ATableGrid: TdxIntegerList);
begin
  inherited Create(AImporter);
  FTableGrid := ATableGrid;
end;

class constructor TdxTableGridDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTableGridDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTableGridDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('gridCol', OnColumn);
end;

function TdxTableGridDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxTableGridDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableGridDestination;
begin
  Result := TdxTableGridDestination(AImporter.PeekDestination);
end;

class function TdxTableGridDestination.OnColumn(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
var
  ADestination: TdxTableGridDestination;
begin
  ADestination := GetThis(AImporter);
  Result := TdxGridColumnDestination.Create(AImporter, ADestination.FTableGrid);
end;

{ TdxGridColumnDestination }

constructor TdxGridColumnDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ATableGrid: TdxIntegerList);
begin
  inherited Create(AImporter);
  FTableGrid := ATableGrid;
end;

function TdxGridColumnDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Empty;
end;

procedure TdxGridColumnDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: Integer;
begin
  AValue := Importer.GetWpSTIntegerValue(AReader, 'w');
  FTableGrid.Add(AValue);
end;

{ TdxTableRowDestination }

constructor TdxTableRowDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ATable: TdxTable);
begin
  inherited Create(AImporter);
  FRow := TdxTableRow.Create(ATable);
end;

class constructor TdxTableRowDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTableRowDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTableRowDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('tc', OnCell);
  Result.Add('trPr', OnTableRowProperties);
  Result.Add('tblPrEx', OnTablePropertiesException);
  Result.Add('sdt', OnStructuredDocument);
  Result.Add('customXml', OnCustomXml);
end;

class function TdxTableRowDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableRowDestination;
begin
  Result := TdxTableRowDestination(AImporter.PeekDestination);
end;

function TdxTableRowDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxTableRowDestination.OnCell(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellDestination.Create(AImporter, GetThis(AImporter).Row);
end;

class function TdxTableRowDestination.OnTableRowProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableRowPropertiesDestination.Create(AImporter, GetThis(AImporter).Row.Properties);
end;

class function TdxTableRowDestination.OnTablePropertiesException(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTablePropertiesDestinationCore.Create(AImporter, GetThis(AImporter).Row.TablePropertiesException);
end;

class function TdxTableRowDestination.OnCustomXml(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxCustomXmlDestination.Create(AImporter);
end;

procedure TdxTableRowDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FRow.Table.Rows.AddInternal(FRow);
end;

class function TdxTableRowDestination.OnStructuredDocument(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxStructuredDocumentDestination.Create(AImporter);
end;

{ TdxTableCellDestination }

constructor TdxTableCellDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ARow: TdxTableRow);
begin
  inherited Create(AImporter);
  FStartParagraphIndex := AImporter.Position.ParagraphIndex;
  FCell := TdxTableCell.Create(ARow);
end;

class constructor TdxTableCellDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTableCellDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTableCellDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('p', OnParagraph);
  Result.Add('tbl', OnTable);
  Result.Add('tcPr', OnCellProperies);
  Result.Add('bookmarkStart', OnBookmarkStart);
  Result.Add('bookmarkEnd', OnBookmarkEnd);
  Result.Add('permStart', OnRangePermissionStart);
  Result.Add('permEnd', OnRangePermissionEnd);
  Result.Add('sdt', OnStructuredDocument);
  Result.Add('customXml', OnCustomXml);
end;

class function TdxTableCellDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableCellDestination;
begin
  Result := TdxTableCellDestination(AImporter.PeekDestination);
end;

class function TdxTableCellDestination.OnParagraph(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  GetThis(AImporter).EndParagraphIndex := AImporter.Position.ParagraphIndex;
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateParagraphDestination;
end;

class function TdxTableCellDestination.OnTable(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
var
  ADestination: TdxTableCellDestination;
begin
  ADestination := GetThis(AImporter);
  Result := TdxTableDestination.Create(AImporter, ADestination.Cell);
end;

class function TdxTableCellDestination.OnCellProperies(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellPropertiesDestination.Create(AImporter, GetThis(AImporter).Cell);
end;

class function TdxTableCellDestination.OnBookmarkStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateBookmarkStartElementDestination(AReader);
end;

class function TdxTableCellDestination.OnBookmarkEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateBookmarkEndElementDestination(AReader);
end;

class function TdxTableCellDestination.OnRangePermissionStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxRangePermissionStartElementDestination.Create(AImporter);
end;

class function TdxTableCellDestination.OnRangePermissionEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxRangePermissionEndElementDestination.Create(AImporter);
end;

class function TdxTableCellDestination.OnStructuredDocument(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxStructuredDocumentDestination.Create(AImporter);
end;

class function TdxTableCellDestination.OnCustomXml(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxCustomXmlDestination.Create(AImporter);
end;

function TdxTableCellDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

procedure TdxTableCellDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Cell.Row.Cells.AddInternal(FCell);
end;

procedure TdxTableCellDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  PieceTable.TableCellsManager.InitializeTableCell(Cell, StartParagraphIndex, EndParagraphIndex);
end;

{ TdxTablePropertiesBaseDestination }

constructor TdxTablePropertiesBaseDestination<T>.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AProperties: T);
begin
  inherited Create(AImporter);
  Assert(AProperties <> nil, 'properties');
  FProperties := AProperties;
end;

procedure TdxTablePropertiesBaseDestination<T>.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FProperties.BeginPropertiesUpdate;
end;

procedure TdxTablePropertiesBaseDestination<T>.ProcessElementClose(AReader: TdxXmlReader);
begin
  FProperties.EndPropertiesUpdate;
end;

{ TdxTablePropertiesDestinationCore }

class constructor TdxTablePropertiesDestinationCore.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTablePropertiesDestinationCore.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTablePropertiesDestinationCore.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('tblW', OnTableWidth);
  Result.Add('tblInd', OnTableIndent);
  Result.Add('tblLook', OnTableLook);
  Result.Add('tblBorders', OnTableBorders);
  Result.Add('tblCellMar', OnTableCellMarginBorders);
  Result.Add('tblCellSpacing', OnTableCellCellSpacing);
  Result.Add('tblLayout', OnTableLayout);
  Result.Add('tblOverlap', OnTableOverlap);
  Result.Add('tblpPr', OnTableFloatingPosition);
  Result.Add('tblStyleColBandSize', OnTableStyleColBandSize);
  Result.Add('tblStyleRowBandSize', OnTableStyleRowBandSize);
  Result.Add('shd', OnTableBackground);
  Result.Add('jc', OnTableAlignment);
  Result.Add('adb', OnTableAvoidDoubleBorders);
end;

class function TdxTablePropertiesDestinationCore.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTablePropertiesDestinationCore;
begin
  Result := TdxTablePropertiesDestinationCore(AImporter.PeekDestination);
end;

class function TdxTablePropertiesDestinationCore.GetProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableProperties;
var
  ADestination: TdxTablePropertiesDestinationCore;
begin
  ADestination := GetThis(AImporter);
  Result := TdxTableProperties(ADestination.Properties);
end;

function TdxTablePropertiesDestinationCore.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxTablePropertiesDestinationCore.OnTableWidth(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWidthUnitNonNegativeDestination.Create(AImporter, GetProperties(AImporter).PreferredWidth);
end;

class function TdxTablePropertiesDestinationCore.OnTableIndent(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWidthUnitDestination.Create(AImporter, GetProperties(AImporter).TableIndent);
end;

class function TdxTablePropertiesDestinationCore.OnTableLook(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableLookDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTablePropertiesDestinationCore.OnTableBorders(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableBordersDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTablePropertiesDestinationCore.OnTableCellMarginBorders(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellMarginsDestination.Create(AImporter, GetProperties(AImporter).CellMargins);
end;

class function TdxTablePropertiesDestinationCore.OnTableCellCellSpacing(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWidthUnitDestination.Create(AImporter, GetProperties(AImporter).CellSpacing);
end;

class function TdxTablePropertiesDestinationCore.OnTableLayout(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableLayoutDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTablePropertiesDestinationCore.OnTableAlignment(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableAlignmentDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTablePropertiesDestinationCore.OnTableOverlap(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableOverlapDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTablePropertiesDestinationCore.OnTableFloatingPosition(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableFloatingPositionDestination.Create(AImporter, GetProperties(AImporter).FloatingPosition);
end;

class function TdxTablePropertiesDestinationCore.OnTableStyleColBandSize(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableStyleColBandSizeDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTablePropertiesDestinationCore.OnTableStyleRowBandSize(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableStyleRowBandSizeDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTablePropertiesDestinationCore.OnTableBackground(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableBackgroundDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTablePropertiesDestinationCore.OnTableAvoidDoubleBorders(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableAvoidDoubleBordersDestination.Create(AImporter, GetProperties(AImporter));
end;

procedure TdxTablePropertiesDestinationCore.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Properties.BeginInit;
end;

procedure TdxTablePropertiesDestinationCore.ProcessElementClose(AReader: TdxXmlReader);
begin
  Properties.EndInit;
end;

{ TdxTablePropertiesDestination }

constructor TdxTablePropertiesDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ATable: TdxTable);
begin
  inherited Create(AImporter, ATable.TableProperties);
  FTable := ATable;
end;

class constructor TdxTablePropertiesDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTablePropertiesDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTablePropertiesDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('tblStyle', OnTableStyle);
  Result.Add('tblW', OnTableWidth);
  Result.Add('tblInd', OnTableIndent);
  Result.Add('tblLook', OnTableLook);
  Result.Add('tblBorders', OnTableBorders);
  Result.Add('tblCellMar', OnTableCellMarginBorders);
  Result.Add('tblCellSpacing', OnTableCellCellSpacing);
  Result.Add('tblLayout', OnTableLayout);
  Result.Add('tblOverlap', OnTableOverlap);
  Result.Add('tblpPr', OnTableFloatingPosition);
  Result.Add('tblStyleColBandSize', OnTableStyleColBandSize);
  Result.Add('tblStyleRowBandSize', OnTableStyleRowBandSize);
  Result.Add('shd', OnTableBackground);
  Result.Add('jc', OnTableAlignment);
  Result.Add('adb', OnTableAvoidDoubleBorders);
end;

function TdxTablePropertiesDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxTablePropertiesDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTablePropertiesDestination;
begin
  Result := TdxTablePropertiesDestination(AImporter.PeekDestination);
end;

class function TdxTablePropertiesDestination.OnTableStyle(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableStyleDestination.Create(AImporter, GetThis(AImporter).Table);
end;

{ TdxTableRowPropertiesDestination }

class constructor TdxTableRowPropertiesDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTableRowPropertiesDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTableRowPropertiesDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('cantSplit', OnTableRowCantSplit);
  Result.Add('gridAfter', OnTableRowGridAfter);
  Result.Add('gridBefore', OnTableRowGridBefore);
  Result.Add('hidden', OnTableRowHideCellMark);
  Result.Add('jc', OnTableRowAlignment);
  Result.Add('tblCellSpacing', OnTableRowCellSpacing);
  Result.Add('tblHeader', OnTableRowHeader);
  Result.Add('trHeight', OnTableRowHeight);
  Result.Add('wAfter', OnTableRowWidthAfter);
  Result.Add('wBefore', OnTableRowWidthBefore);
  Result.Add('cnfStyle', OnTableRowConditionalFormatting);
end;

class function TdxTableRowPropertiesDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableRowPropertiesDestination;
begin
  Result := TdxTableRowPropertiesDestination(AImporter.PeekDestination);
end;

class function TdxTableRowPropertiesDestination.GetProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableRowProperties;
var
  ADestination: TdxTableRowPropertiesDestination;
begin
  ADestination := GetThis(AImporter);
  Result := TdxTableRowProperties(ADestination.Properties);
end;

function TdxTableRowPropertiesDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxTableRowPropertiesDestination.OnTableRowCantSplit(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableRowCantSplitDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTableRowPropertiesDestination.OnTableRowGridAfter(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableRowGridAfterDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTableRowPropertiesDestination.OnTableRowGridBefore(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableRowGridBeforeDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTableRowPropertiesDestination.OnTableRowHideCellMark(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableRowHideCellMarkDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTableRowPropertiesDestination.OnTableRowAlignment(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableRowAlignmentDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTableRowPropertiesDestination.OnTableRowCellSpacing(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWidthUnitDestination.Create(AImporter, GetProperties(AImporter).CellSpacing);
end;

class function TdxTableRowPropertiesDestination.OnTableRowHeader(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableRowHeaderDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTableRowPropertiesDestination.OnTableRowHeight(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxHeightUnitDestination.Create(AImporter, GetProperties(AImporter).Height);
end;

class function TdxTableRowPropertiesDestination.OnTableRowWidthAfter(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWidthUnitDestination.Create(AImporter, GetProperties(AImporter).WidthAfter);
end;

class function TdxTableRowPropertiesDestination.OnTableRowWidthBefore(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWidthUnitDestination.Create(AImporter, GetProperties(AImporter).WidthBefore);
end;

class function TdxTableRowPropertiesDestination.OnTableRowConditionalFormatting(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableRowConditionalFormattingDestination.Create(AImporter, GetProperties(AImporter));
end;

procedure TdxTableRowPropertiesDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Properties.BeginInit;
end;

procedure TdxTableRowPropertiesDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  Properties.EndInit;
end;

{ TdxTableCellPropertiesDestinationCore }

class constructor TdxTableCellPropertiesDestinationCore.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTableCellPropertiesDestinationCore.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTableCellPropertiesDestinationCore.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('tcW', OnTableCellWidth);
  Result.Add('tcBorders', OnTableCellBorders);
  Result.Add('vMerge', OnTableCellMerge);
  Result.Add('gridSpan', OnTableCellColumnSpan);
  Result.Add('shd', OnTableCellShading);
  Result.Add('tcMar', OnTableCellMargins);
  Result.Add('tcFitText', OnTableCellFitText);
  Result.Add('noWrap', OnTableCellNoWrap);
  Result.Add('hideMark', OnTableCellHideMark);
  Result.Add('textDirection', OnTableCellTextDirection);
  Result.Add('vAlign', OnTableCellVerticalAlignment);
  Result.Add('cnfStyle', OnTableCellConditionalFormatting);
end;

class function TdxTableCellPropertiesDestinationCore.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableCellPropertiesDestinationCore;
begin
  Result := TdxTableCellPropertiesDestinationCore(AImporter.PeekDestination);
end;

class function TdxTableCellPropertiesDestinationCore.GetProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableCellProperties;
var
  ADestination: TdxTableCellPropertiesDestinationCore;
begin
  ADestination := GetThis(AImporter);
  Result := TdxTableCellProperties(ADestination.Properties);
end;

function TdxTableCellPropertiesDestinationCore.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxTableCellPropertiesDestinationCore.OnTableCellWidth(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWidthUnitNonNegativeDestination.Create(AImporter, GetProperties(AImporter).PreferredWidth);
end;

class function TdxTableCellPropertiesDestinationCore.OnTableCellBorders(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellBordersDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTableCellPropertiesDestinationCore.OnTableCellMerge(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellVerticalMergingStateDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTableCellPropertiesDestinationCore.OnTableCellColumnSpan(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellColumnSpanDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTableCellPropertiesDestinationCore.OnTableCellShading(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellShadingDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTableCellPropertiesDestinationCore.OnTableCellFitText(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellFitTextDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTableCellPropertiesDestinationCore.OnTableCellNoWrap(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellNoWrapDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTableCellPropertiesDestinationCore.OnTableCellMargins(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellMarginsDestination.Create(AImporter, GetProperties(AImporter).CellMargins);
end;

class function TdxTableCellPropertiesDestinationCore.OnTableCellHideMark(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellHideMarkDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTableCellPropertiesDestinationCore.OnTableCellTextDirection(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellTextDirectionDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTableCellPropertiesDestinationCore.OnTableCellVerticalAlignment(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellVerticalAlignmentDestination.Create(AImporter, GetProperties(AImporter));
end;

class function TdxTableCellPropertiesDestinationCore.OnTableCellConditionalFormatting(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellConditionalFormattingDestination.Create(AImporter, GetProperties(AImporter));
end;

procedure TdxTableCellPropertiesDestinationCore.ProcessElementOpen(AReader: TdxXmlReader);
begin
  Properties.BeginInit;
end;

procedure TdxTableCellPropertiesDestinationCore.ProcessElementClose(AReader: TdxXmlReader);
begin
  Properties.EndInit;
end;

{ TdxTableCellPropertiesDestination }

constructor TdxTableCellPropertiesDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ACell: TdxTableCell);
begin
  inherited Create(AImporter, ACell.Properties);
  FCell := ACell;
end;

class constructor TdxTableCellPropertiesDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTableCellPropertiesDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTableCellPropertiesDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('tcW', OnTableCellWidth);
  Result.Add('tcBorders', OnTableCellBorders);
  Result.Add('vMerge', OnTableCellMerge);
  Result.Add('tblCStyle', OnTableCellStyle);
  Result.Add('gridSpan', OnTableCellColumnSpan);
  Result.Add('shd', OnTableCellShading);
  Result.Add('tcMar', OnTableCellMargins);
  Result.Add('tcFitText', OnTableCellFitText);
  Result.Add('noWrap', OnTableCellNoWrap);
  Result.Add('hideMark', OnTableCellHideMark);
  Result.Add('textDirection', OnTableCellTextDirection);
  Result.Add('vAlign', OnTableCellVerticalAlignment);
  Result.Add('cnfStyle', OnTableCellConditionalFormatting);
end;

class function TdxTableCellPropertiesDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableCellPropertiesDestination;
begin
  Result := TdxTableCellPropertiesDestination(AImporter.PeekDestination);
end;

class function TdxTableCellPropertiesDestination.OnTableCellStyle(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellStyleDestination.Create(AImporter, GetThis(AImporter).Cell);
end;

function TdxTableCellPropertiesDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

{ TdxWidthUnitDestination }

constructor TdxWidthUnitDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AWidthUnit: TdxWidthUnit);
begin
  inherited Create(AImporter);
  Assert(AWidthUnit <> nil, 'widthUnit');
  FWidthUnit := AWidthUnit;
end;

class constructor TdxWidthUnitDestination.Initialize;
begin
  FWidthUnitTypeTable := CreateWidthUnitTypeTable;
end;

class destructor TdxWidthUnitDestination.Finalize;
begin
  FWidthUnitTypeTable.Free;
end;

class function TdxWidthUnitDestination.CreateWidthUnitTypeTable: TdxEnumeratedDictionary<TdxWidthUnitType, string>;
begin
  Result := TdxEnumeratedDictionary<TdxWidthUnitType, string>.Create;
  Result.Add(TdxWidthUnitType.Auto, 'auto');
  Result.Add(TdxWidthUnitType.FiftiethsOfPercent, 'pct');
  Result.Add(TdxWidthUnitType.Nil, 'nil');
  Result.Add(TdxWidthUnitType.ModelUnits, 'dxa');
end;

function TdxWidthUnitDestination.IsValid(AValue: Integer): Boolean;
begin
  Result := AValue <> MinInt;
end;

procedure TdxWidthUnitDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AUnitType: TdxWidthUnitType;
  AValue: Integer;
begin
  AUnitType := Importer.GetWpEnumValue<TdxWidthUnitType>(AReader, 'type', FWidthUnitTypeTable, TdxWidthUnitType.Auto);

  AValue := Importer.GetWpSTIntegerValue(AReader, 'w');
  if IsValid(AValue) then
  begin
    FWidthUnit.&Type := AUnitType;
    if AUnitType = TdxWidthUnitType.ModelUnits then
      FWidthUnit.Value := DocumentModel.UnitConverter.TwipsToModelUnits(AValue)
    else
      FWidthUnit.Value := AValue;
  end
  else
    FWidthUnit.&Type := TdxWidthUnitType.Auto;
end;

{ TdxWidthUnitNonNegativeDestination }

function TdxWidthUnitNonNegativeDestination.IsValid(AValue: Integer): Boolean;
begin
  Result := inherited IsValid(AValue) and (AValue >= 0);
end;

{ TdxHeightUnitDestination }

constructor TdxHeightUnitDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AHeightUnit: TdxHeightUnit);
begin
  inherited Create(AImporter);
  Assert(AHeightUnit <> nil, 'heightUnit');
  FHeightUnit := AHeightUnit;
end;

class constructor TdxHeightUnitDestination.Initialize;
begin
  FHeightUnitTypeTable := CreateHeightUnitTypeTable;
end;

class destructor TdxHeightUnitDestination.Finalize;
begin
  FHeightUnitTypeTable.Free;
end;

class function TdxHeightUnitDestination.CreateHeightUnitTypeTable: TdxEnumeratedDictionary<TdxHeightUnitType, string>;
begin
  Result := TdxEnumeratedDictionary<TdxHeightUnitType, string>.Create;
  Result.Add(TdxHeightUnitType.Auto, 'auto');
  Result.Add(TdxHeightUnitType.Exact, 'exact');
  Result.Add(TdxHeightUnitType.Minimum, 'atLeast');
end;

procedure TdxHeightUnitDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AUnitType: TdxHeightUnitType;
  AValue: Integer;
begin
  AUnitType := Importer.GetWpEnumValue<TdxHeightUnitType>(AReader, 'hRule', FHeightUnitTypeTable, TdxHeightUnitType.Minimum);
  FHeightUnit.&Type := AUnitType;
  AValue := Importer.GetWpSTIntegerValue(AReader, 'val');
  if AValue <> MinInt then
    FHeightUnit.Value := DocumentModel.UnitConverter.TwipsToModelUnits(AValue);
end;

{ TdxTableStyleDestination }

constructor TdxTableStyleDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ATable: TdxTable);
begin
  inherited Create(AImporter);
  FTable := ATable;
end;

procedure TdxTableStyleDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AStyleName: string;
  AStyleIndex: Integer;
begin
  AStyleName := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);
  AStyleIndex := TdxWordProcessingMLBaseImporter(Importer).LookupTableStyleIndex(AStyleName);
  if AStyleIndex >= 0 then
    FTable.StyleIndex := AStyleIndex;
end;

{ TdxTableCellStyleDestination }

constructor TdxTableCellStyleDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ACell: TdxTableCell);
begin
  inherited Create(AImporter);
  FCell := ACell;
end;

procedure TdxTableCellStyleDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AStyleName: string;
  AStyleIndex: Integer;
begin
  AStyleName := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);
  AStyleIndex := TdxWordProcessingMLBaseImporter(Importer).LookupTableCellStyleIndex(AStyleName);
  if AStyleIndex >= 0 then
    FCell.SetTableCellStyleIndexCore(AStyleIndex);
end;

{ TdxTablePropertiesElementBaseDestination }

constructor TdxTablePropertiesElementBaseDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ATableProperties: TdxTableProperties);
begin
  inherited Create(AImporter);
  Assert(ATableProperties <> nil, 'tableProperties');
  FTableProperties := ATableProperties;
end;

class function TdxTablePropertiesElementBaseDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTablePropertiesElementBaseDestination;
begin
  Result := TdxTablePropertiesElementBaseDestination(AImporter.PeekDestination);
end;

class function TdxTablePropertiesElementBaseDestination.GetProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableProperties;
var
  ADestination: TdxTablePropertiesElementBaseDestination;
begin
  ADestination := GetThis(AImporter);
  Result := TdxTableProperties(ADestination.TableProperties);
end;

{ TdxTablePropertiesLeafElementDestination }

function TdxTablePropertiesLeafElementDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Empty;
end;

{ TdxTableLookDestination }

procedure TdxTableLookDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: Integer;
  ATableLook: TdxTableLookTypes absolute AValue;
begin
  AValue := Importer.GetWpSTIntegerValue(AReader, 'val', TdxNumberStyles.HexNumber, MinInt) shr 5;
  if AValue = MinInt then
    Exit;
  TableProperties.TableLook := ATableLook;
end;

{ TdxTableBordersDestination }

class constructor TdxTableBordersDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTableBordersDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTableBordersDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('top', OnTopBorder);
  Result.Add('left', OnLeftBorder);
  Result.Add('bottom', OnBottomBorder);
  Result.Add('right', OnRightBorder);
  Result.Add('insideH', OnInsideHorizontalBorder);
  Result.Add('insideV', OnInsideVerticalBorder);
end;

class function TdxTableBordersDestination.GetTableBorders(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableBorders;
begin
  Result := GetProperties(AImporter).Borders;
end;

class function TdxTableBordersDestination.OnTopBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableBorderElementDestination.Create(AImporter, GetTableBorders(AImporter).TopBorder);
end;

class function TdxTableBordersDestination.OnLeftBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableBorderElementDestination.Create(AImporter, GetTableBorders(AImporter).LeftBorder);
end;

class function TdxTableBordersDestination.OnBottomBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableBorderElementDestination.Create(AImporter, GetTableBorders(AImporter).BottomBorder);
end;

class function TdxTableBordersDestination.OnRightBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableBorderElementDestination.Create(AImporter, GetTableBorders(AImporter).RightBorder);
end;

class function TdxTableBordersDestination.OnInsideHorizontalBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableBorderElementDestination.Create(AImporter, GetTableBorders(AImporter).InsideHorizontalBorder);
end;

class function TdxTableBordersDestination.OnInsideVerticalBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableBorderElementDestination.Create(AImporter, GetTableBorders(AImporter).InsideVerticalBorder);
end;

function TdxTableBordersDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

{ TdxTableLayoutDestination }

class constructor TdxTableLayoutDestination.Initialize;
begin
  FTableLayoutTypeTable := CreateTableLayoutTypeTable;
end;

class destructor TdxTableLayoutDestination.Finalize;
begin
  FTableLayoutTypeTable.Free;
end;

class function TdxTableLayoutDestination.CreateTableLayoutTypeTable: TdxEnumeratedDictionary<TdxTableLayoutType, string>;
begin
  Result := TdxEnumeratedDictionary<TdxTableLayoutType, string>.Create;
  Result.Add(TdxTableLayoutType.Autofit, 'autofit');
  Result.Add(TdxTableLayoutType.Fixed, 'fixed');
end;

procedure TdxTableLayoutDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  TableProperties.TableLayout := Importer.GetWpEnumValue<TdxTableLayoutType>(AReader, 'type', FTableLayoutTypeTable,
    TdxTableLayoutType.Autofit);
end;

{ TdxTableAlignmentDestination }

class constructor TdxTableAlignmentDestination.Initialize;
begin
  FTableRowAlignmentTable := CreateTableRowAlignmentTable;
end;

class destructor TdxTableAlignmentDestination.Finalize;
begin
  FTableRowAlignmentTable.Free;
end;

class function TdxTableAlignmentDestination.CreateTableRowAlignmentTable: TdxEnumeratedDictionary<TdxTableRowAlignment, string>;
begin
  Result := TdxEnumeratedDictionary<TdxTableRowAlignment, string>.Create;
  Result.Add(TdxTableRowAlignment.Both, 'both');
  Result.Add(TdxTableRowAlignment.Center, 'center');
  Result.Add(TdxTableRowAlignment.Distribute, 'distribute');
  Result.Add(TdxTableRowAlignment.Left, 'left');
  Result.Add(TdxTableRowAlignment.NumTab, 'numTab');
  Result.Add(TdxTableRowAlignment.Right, 'right');
end;

procedure TdxTableAlignmentDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  TableProperties.TableAlignment := Importer.GetWpEnumValue<TdxTableRowAlignment>(AReader, 'val',
    FTableRowAlignmentTable, TdxTableRowAlignment.Left);
end;

{ TdxTableOverlapDestination }

procedure TdxTableOverlapDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AOverlap: string;
begin
  AOverlap := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);
  if AOverlap = 'never' then
    TableProperties.IsTableOverlap := False
  else
    TableProperties.IsTableOverlap := True;
end;

{ TdxTableStyleColBandSizeDestination }

procedure TdxTableStyleColBandSizeDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: Integer;
begin
  AValue := Importer.GetWpSTIntegerValue(AReader, 'val');
  if AValue <> MinInt then
    TableProperties.TableStyleColBandSize := AValue;
end;

{ TdxTableStyleRowBandSizeDestination }

procedure TdxTableStyleRowBandSizeDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: Integer;
begin
  AValue := Importer.GetWpSTIntegerValue(AReader, 'val');
  if AValue <> MinInt then
    TableProperties.TableStyleRowBandSize := AValue;
end;

{ TdxTableBackgroundDestination }

procedure TdxTableBackgroundDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  APattern: TdxShadingPattern;
  AFill, APatternColor, AActualColor: TdxAlphaColor;
  AImporter: TdxWordProcessingMLBaseImporter;
begin
  AImporter := TdxWordProcessingMLBaseImporter(Importer);
  APattern := AImporter.GetShadingPatternDef(AReader, 'val', TdxShadingPattern.Clear);
  AFill := AImporter.GetWpSTColorValue(AReader, 'fill', TdxAlphaColors.Empty);
  APatternColor := AImporter.GetWpSTColorValue(AReader, 'color', TdxAlphaColors.Empty);
  AActualColor := TdxShadingHelper.GetActualBackColor(AFill, APatternColor, APattern);
  if AActualColor <> TdxAlphaColors.Empty then
    TableProperties.BackgroundColor := AActualColor;
end;

{ TdxTableAvoidDoubleBordersDestination }

procedure TdxTableAvoidDoubleBordersDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  TableProperties.AvoidDoubleBorders := Importer.GetWpSTOnOffValue(AReader, 'val', False);
end;

{ TdxTableFloatingPositionDestination }

constructor TdxTableFloatingPositionDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFloatingPosition: TdxTableFloatingPosition);
begin
  inherited Create(AImporter);
  Assert(AFloatingPosition <> nil, 'floatingPosition');
  FFloatingPosition := AFloatingPosition;
end;

class constructor TdxTableFloatingPositionDestination.Initialize;
begin
  FHorizontalAnchorTypeTable := CreateHorizontalAnchorTypeTable;
  FVerticalAnchorTypeTable := CreateVerticalAnchorTypeTable;
  FHorizontalAlignModeTable := CreateHorizontalAlignModeTable;
  FVerticalAlignModeTable := CreateVerticalAlignModeTable;
end;

class destructor TdxTableFloatingPositionDestination.Finalize;
begin
  FHorizontalAnchorTypeTable.Free;
  FVerticalAnchorTypeTable.Free;
  FHorizontalAlignModeTable.Free;
  FVerticalAlignModeTable.Free;
end;

class function TdxTableFloatingPositionDestination.CreateHorizontalAnchorTypeTable: TdxEnumeratedDictionary<TdxHorizontalAnchorTypes, string>;
begin
  Result := TdxEnumeratedDictionary<TdxHorizontalAnchorTypes, string>.Create;
  Result.Add(TdxHorizontalAnchorTypes.Column, 'text');
  Result.Add(TdxHorizontalAnchorTypes.Margin, 'margin');
  Result.Add(TdxHorizontalAnchorTypes.Page, 'page');
end;

class function TdxTableFloatingPositionDestination.CreateVerticalAnchorTypeTable: TdxEnumeratedDictionary<TdxVerticalAnchorTypes, string>;
begin
  Result := TdxEnumeratedDictionary<TdxVerticalAnchorTypes, string>.Create;
  Result.Add(TdxVerticalAnchorTypes.Paragraph, 'text');
  Result.Add(TdxVerticalAnchorTypes.Margin, 'margin');
  Result.Add(TdxVerticalAnchorTypes.Page, 'page');
end;

class function TdxTableFloatingPositionDestination.CreateHorizontalAlignModeTable: TdxEnumeratedDictionary<TdxHorizontalAlignMode, string>;
begin
  Result := TdxEnumeratedDictionary<TdxHorizontalAlignMode, string>.Create;
  Result.Add(TdxHorizontalAlignMode.Center, 'center');
  Result.Add(TdxHorizontalAlignMode.Inside, 'inside');
  Result.Add(TdxHorizontalAlignMode.Left, 'left');
  Result.Add(TdxHorizontalAlignMode.Outside, 'outside');
  Result.Add(TdxHorizontalAlignMode.Right, 'right');
end;

class function TdxTableFloatingPositionDestination.CreateVerticalAlignModeTable: TdxEnumeratedDictionary<TdxVerticalAlignMode, string>;
begin
  Result := TdxEnumeratedDictionary<TdxVerticalAlignMode, string>.Create;
  Result.Add(TdxVerticalAlignMode.Bottom, 'bottom');
  Result.Add(TdxVerticalAlignMode.Center, 'center');
  Result.Add(TdxVerticalAlignMode.Inline, 'inline');
  Result.Add(TdxVerticalAlignMode.Inside, 'inside');
  Result.Add(TdxVerticalAlignMode.Outside, 'outside');
  Result.Add(TdxVerticalAlignMode.Top, 'top');
end;

procedure TdxTableFloatingPositionDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: Integer;
  AUnitConverter: TdxDocumentModelUnitConverter;
  ADefaultHorizontalAnchor: TdxHorizontalAnchorTypes;
  ADefaultVerticalAnchor: TdxVerticalAnchorTypes;
begin
  AValue := Importer.GetWpSTIntegerValue(AReader, 'bottomFromText');
  AUnitConverter := DocumentModel.UnitConverter;
  if AValue <> MinInt then
    FFloatingPosition.BottomFromText := AUnitConverter.TwipsToModelUnits(AValue);
  AValue := Importer.GetWpSTIntegerValue(AReader, 'leftFromText');
  if AValue <> MinInt then
    FFloatingPosition.LeftFromText := AUnitConverter.TwipsToModelUnits(AValue);
  AValue := Importer.GetWpSTIntegerValue(AReader, 'rightFromText');
  if AValue <> MinInt then
    FFloatingPosition.RightFromText := AUnitConverter.TwipsToModelUnits(AValue);
  AValue := Importer.GetWpSTIntegerValue(AReader, 'topFromText');
  if AValue <> MinInt then
    FFloatingPosition.TopFromText := AUnitConverter.TwipsToModelUnits(AValue);
  AValue := Importer.GetWpSTIntegerValue(AReader, 'tblpX');
  if AValue <> MinInt then
  begin
    FFloatingPosition.TableHorizontalPosition := AUnitConverter.TwipsToModelUnits(AValue);
    if AValue <> 0 then
      FFloatingPosition.TextWrapping := TdxTextWrapping.Around;
  end;
  AValue := Importer.GetWpSTIntegerValue(AReader, 'tblpY');
  if AValue <> MinInt then
  begin
    FFloatingPosition.TableVerticalPosition := AUnitConverter.TwipsToModelUnits(AValue);
    if AValue <> 0 then
      FFloatingPosition.TextWrapping := TdxTextWrapping.Around;
  end;

  ADefaultHorizontalAnchor := TdxHorizontalAnchorTypes.Column;
  FFloatingPosition.HorizontalAnchor := GetHorizontalAnchor(AReader, ADefaultHorizontalAnchor);
  if FFloatingPosition.HorizontalAnchor <> ADefaultHorizontalAnchor then
    FFloatingPosition.TextWrapping := TdxTextWrapping.Around;
  ADefaultVerticalAnchor := TdxVerticalAnchorTypes.Margin;
  FFloatingPosition.VerticalAnchor := GetVerticalAnchor(AReader, ADefaultVerticalAnchor);
  if FFloatingPosition.VerticalAnchor <> ADefaultVerticalAnchor then
    FFloatingPosition.TextWrapping := TdxTextWrapping.Around;
  FFloatingPosition.HorizontalAlign := Importer.GetWpEnumValue<TdxHorizontalAlignMode>(AReader, 'tblpXSpec',
    FHorizontalAlignModeTable, TdxHorizontalAlignMode.None);
  FFloatingPosition.VerticalAlign := Importer.GetWpEnumValue<TdxVerticalAlignMode>(AReader, 'tblpYSpec',
    FVerticalAlignModeTable, TdxVerticalAlignMode.None);
end;

function TdxTableFloatingPositionDestination.GetHorizontalAnchor(AReader: TdxXmlReader;
  ADefaultValue: TdxHorizontalAnchorTypes): TdxHorizontalAnchorTypes;
var
  AValue: string;
begin
  AValue := Importer.ReadAttribute(AReader, 'horzAnchor');
  if AValue = '' then
    Exit(ADefaultValue);
  if AValue = 'column' then
    Exit(TdxHorizontalAnchorTypes.Page);
  Result := Importer.GetWpEnumValueCore<TdxHorizontalAnchorTypes>(AValue, FHorizontalAnchorTypeTable, ADefaultValue);
end;

function TdxTableFloatingPositionDestination.GetVerticalAnchor(AReader: TdxXmlReader; ADefaultValue: TdxVerticalAnchorTypes): TdxVerticalAnchorTypes;
var
  AValue: string;
begin
  AValue := Importer.ReadAttribute(AReader, 'vertAnchor');
  if AValue = '' then
    Exit(ADefaultValue);
  if AValue = 'paragraph' then
    Exit(TdxVerticalAnchorTypes.Paragraph);
  Result := Importer.GetWpEnumValueCore<TdxVerticalAnchorTypes>(AValue, FVerticalAnchorTypeTable, ADefaultValue);
end;

{ TdxTableBorderElementDestinationBase }

constructor TdxTableBorderElementDestinationBase.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  ABorder: TdxBorderBase);
begin
  inherited Create(AImporter);
  FBorder := ABorder;
end;

procedure TdxTableBorderElementDestinationBase.SetBorderOffset(ASpace: Integer);
begin
  FBorder.Offset := UnitConverter.PointsToModelUnits(ASpace);
end;

procedure TdxTableBorderElementDestinationBase.SetBorderWidth(ASize: Integer);
begin
  FBorder.Width := Trunc(UnitConverter.PointsToModelUnitsF(ASize * 0.125));
end;

procedure TdxTableBorderElementDestinationBase.SetBorderLineStyle(ABorderLineStyle: TdxBorderLineStyle);
begin
  if ABorderLineStyle = TdxBorderLineStyle.&Nil then
    FBorder.Style := TdxBorderLineStyle.None
  else
    FBorder.Style := ABorderLineStyle;
end;

{ TdxTableCellBorderElementDestination }

procedure TdxTableCellBorderElementDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ABorderLineStyle: TdxBorderLineStyle;
  AColor: TdxAlphaColor;
  ASpace, ASize: Integer;
  AFrame, AShadow, AIsDefaultValue: Boolean;
begin
  ABorderLineStyle := Importer.GetWpEnumValue<TdxBorderLineStyle>(AReader, 'val', BorderStyleTable, TdxBorderLineStyle.None);
  AColor := TdxWordProcessingMLBaseImporter(Importer).GetWpSTColorValue(AReader, 'color');
  AFrame := Importer.GetWpSTOnOffValue(AReader, 'frame', False);
  AShadow := Importer.GetWpSTOnOffValue(AReader, 'shadow', False);
  ASpace := Importer.GetWpSTIntegerValue(AReader, 'space', 0);
  ASize := Importer.GetWpSTIntegerValue(AReader, 'sz', 0);

  AIsDefaultValue := (ABorderLineStyle = TdxBorderLineStyle.None) and (AColor = TdxAlphaColors.Empty) and
    not AFrame and not AShadow and (ASpace = 0) and (ASize = 0);
  if not AIsDefaultValue then
  begin
    SetBorderLineStyle(ABorderLineStyle);
    Border.Color := AColor;
    Border.Frame := AFrame;
    Border.Shadow := AShadow;
    SetBorderOffset(ASpace);
    SetBorderWidth(ASize);
  end;
end;

{ TdxTableBorderElementDestination }

procedure TdxTableBorderElementDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ABorderLineStyle: TdxNullableValue<TdxBorderLineStyle>;
  AColor: TdxNullableValue<TdxAlphaColor>;
  AFrame, AShadow: TdxNullableValue<Boolean>;
  ASpace, ASize: Integer;
begin
  ABorderLineStyle := Importer.GetWpEnumOnOffNullValue<TdxBorderLineStyle>(AReader, 'val', BorderStyleTable);
  if ABorderLineStyle.HasValue then
    SetBorderLineStyle(ABorderLineStyle.Value);
  AColor := TdxWordProcessingMLBaseImporter(Importer).GetWpSTColorOrNullValue(AReader, 'color');
  if AColor.HasValue then
    Border.Color := AColor;
  AFrame := Importer.GetWpSTOnOffNullValue(AReader, 'frame');
  if AFrame.HasValue then
    Border.Frame := AFrame.Value;
  AShadow := Importer.GetWpSTOnOffNullValue(AReader, 'shadow');
  if AShadow.HasValue then
    Border.Shadow := AShadow.Value;
  ASpace := Importer.GetWpSTIntegerValue(AReader, 'space', MinInt);
  if ASpace <> MinInt then
    SetBorderOffset(ASpace);
  ASize := Importer.GetWpSTIntegerValue(AReader, 'sz', MinInt);
  if ASize <> MinInt then
    SetBorderWidth(ASize);
end;

{ TdxTableRowPropertiesElementBaseDestination }

constructor TdxTableRowPropertiesElementBaseDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ARowProperties: TdxTableRowProperties);
begin
  inherited Create(AImporter);
  Assert(ARowProperties <> nil, 'rowProperties');
  FRowProperties := ARowProperties;
end;

class function TdxTableRowPropertiesElementBaseDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableRowPropertiesElementBaseDestination;
begin
  Result := TdxTableRowPropertiesElementBaseDestination(AImporter.PeekDestination);
end;

class function TdxTableRowPropertiesElementBaseDestination.GetProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableRowProperties;
var
  ADestination: TdxTableRowPropertiesElementBaseDestination;
begin
  ADestination := GetThis(AImporter);
  Result := TdxTableRowProperties(ADestination.RowProperties);
end;

{ TdxTableRowPropertiesLeafElementDestination }

function TdxTableRowPropertiesLeafElementDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Empty;
end;

{ TdxTableRowCantSplitDestination }

procedure TdxTableRowCantSplitDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  RowProperties.CantSplit := Importer.GetWpSTOnOffValue(AReader, 'val', True);
end;

{ TdxTableRowGridAfterDestination }

procedure TdxTableRowGridAfterDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  AValue: Integer;
begin
  AValue := Importer.GetWpSTIntegerValue(AReader, 'val');
  if AValue <> MinInt then
    RowProperties.GridAfter := Max(0, AValue);
end;

{ TdxTableRowGridBeforeDestination }

procedure TdxTableRowGridBeforeDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  AValue: Integer;
begin
  AValue := Importer.GetWpSTIntegerValue(AReader, 'val');
  if AValue <> MinInt then
    RowProperties.GridBefore := Math.Max(0, AValue);
end;

{ TdxTableRowHideCellMarkDestination }

procedure TdxTableRowHideCellMarkDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  RowProperties.HideCellMark := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxTableRowAlignmentDestination }

class constructor TdxTableRowAlignmentDestination.Initialize;
begin
  FTableRowAlignmentTable := CreateTableRowAlignmentTable;
end;

class destructor TdxTableRowAlignmentDestination.Finalize;
begin
  FTableRowAlignmentTable.Free;
end;

class function TdxTableRowAlignmentDestination.CreateTableRowAlignmentTable: TdxEnumeratedDictionary<TdxTableRowAlignment, string>;
begin
  Result := TdxEnumeratedDictionary<TdxTableRowAlignment, string>.Create;
  Result.Add(TdxTableRowAlignment.Both, 'both');
  Result.Add(TdxTableRowAlignment.Center, 'center');
  Result.Add(TdxTableRowAlignment.Distribute, 'distribute');
  Result.Add(TdxTableRowAlignment.Left, 'left');
  Result.Add(TdxTableRowAlignment.NumTab, 'numTab');
  Result.Add(TdxTableRowAlignment.Right, 'right');
end;

procedure TdxTableRowAlignmentDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  RowProperties.TableRowAlignment := Importer.GetWpEnumValue<TdxTableRowAlignment>(AReader, 'val',
    FTableRowAlignmentTable, TdxTableRowAlignment.Left);
end;

{ TdxTableRowHeaderDestination }

procedure TdxTableRowHeaderDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  RowProperties.Header := Importer.GetWpSTOnOffValue(AReader, 'val', True);
end;

{ TdxTableRowConditionalFormattingDestination }

procedure TdxTableRowConditionalFormattingDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
end;

{ TdxTableCellMarginsDestination }

constructor TdxTableCellMarginsDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ACellMargins: TdxCellMargins);
begin
  inherited Create(AImporter);
  Assert(ACellMargins <> nil, 'cellMargins');
  FCellMargins := ACellMargins;
end;

class constructor TdxTableCellMarginsDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTableCellMarginsDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTableCellMarginsDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('top', OnTopCellMargin);
  Result.Add('left', OnLeftCellMargin);
  Result.Add('bottom', OnBottomCellMargin);
  Result.Add('right', OnRightCellMargin);
end;

class function TdxTableCellMarginsDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableCellMarginsDestination;
begin
  Result := TdxTableCellMarginsDestination(AImporter.PeekDestination);
end;

class function TdxTableCellMarginsDestination.GetCellMargins(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxCellMargins;
var
  ADestination: TdxTableCellMarginsDestination;
begin
  ADestination := GetThis(AImporter);
  Result := ADestination.CellMargins;
end;

class function TdxTableCellMarginsDestination.OnTopCellMargin(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWidthUnitDestination.Create(AImporter, GetCellMargins(AImporter).Top);
end;

class function TdxTableCellMarginsDestination.OnLeftCellMargin(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWidthUnitDestination.Create(AImporter, GetCellMargins(AImporter).Left);
end;

class function TdxTableCellMarginsDestination.OnBottomCellMargin(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWidthUnitDestination.Create(AImporter, GetCellMargins(AImporter).Bottom);
end;

class function TdxTableCellMarginsDestination.OnRightCellMargin(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWidthUnitDestination.Create(AImporter, GetCellMargins(AImporter).Right);
end;

function TdxTableCellMarginsDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

{ TdxTableShadingDestination }

procedure TdxTableShadingDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
end;

{ TdxTableCellPropertiesElementBaseDestination }

constructor TdxTableCellPropertiesElementBaseDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ACellProperties: TdxTableCellProperties);
begin
  inherited Create(AImporter);
  Assert(ACellProperties <> nil, 'cellProperties');
  FCellProperties := ACellProperties;
end;

class function TdxTableCellPropertiesElementBaseDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableCellPropertiesElementBaseDestination;
begin
  Result := TdxTableCellPropertiesElementBaseDestination(AImporter.PeekDestination);
end;

class function TdxTableCellPropertiesElementBaseDestination.GetProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableCellProperties;
var
  ADestination: TdxTableCellPropertiesElementBaseDestination;
begin
  ADestination := GetThis(AImporter);
  Result := TdxTableCellProperties(ADestination.CellProperties);
end;

{ TdxTableCellPropertiesLeafElementDestination }

function TdxTableCellPropertiesLeafElementDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := ElementHandlerTable.Empty;
end;

{ TdxTableCellBordersDestination }

class constructor TdxTableCellBordersDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTableCellBordersDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTableCellBordersDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('top', OnTopBorder);
  Result.Add('left', OnLeftBorder);
  Result.Add('bottom', OnBottomBorder);
  Result.Add('right', OnRightBorder);
  Result.Add('insideH', OnInsideHorizontalBorder);
  Result.Add('insideV', OnInsideVerticalBorder);
  Result.Add('tl2br', OnTopLeftDiagonalBorder);
  Result.Add('tr2bl', OnTopRightDiagonalBorder);
end;

class function TdxTableCellBordersDestination.GetCellBorders(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTableCellBorders;
begin
  Result := GetProperties(AImporter).Borders;
end;

class function TdxTableCellBordersDestination.OnTopBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellBorderElementDestination.Create(AImporter, GetCellBorders(AImporter).TopBorder);
end;

class function TdxTableCellBordersDestination.OnLeftBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellBorderElementDestination.Create(AImporter, GetCellBorders(AImporter).LeftBorder);
end;

class function TdxTableCellBordersDestination.OnBottomBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellBorderElementDestination.Create(AImporter, GetCellBorders(AImporter).BottomBorder);
end;

class function TdxTableCellBordersDestination.OnRightBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellBorderElementDestination.Create(AImporter, GetCellBorders(AImporter).RightBorder);
end;

class function TdxTableCellBordersDestination.OnInsideHorizontalBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellBorderElementDestination.Create(AImporter, GetCellBorders(AImporter).InsideHorizontalBorder);
end;

class function TdxTableCellBordersDestination.OnInsideVerticalBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellBorderElementDestination.Create(AImporter, GetCellBorders(AImporter).InsideVerticalBorder);
end;

class function TdxTableCellBordersDestination.OnTopLeftDiagonalBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellBorderElementDestination.Create(AImporter, GetCellBorders(AImporter).TopLeftDiagonalBorder);
end;

class function TdxTableCellBordersDestination.OnTopRightDiagonalBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellBorderElementDestination.Create(AImporter, GetCellBorders(AImporter).TopRightDiagonalBorder);
end;

function TdxTableCellBordersDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

{ TdxTableCellVerticalMergingStateDestination }

class constructor TdxTableCellVerticalMergingStateDestination.Initialize;
begin
  FMergingStateTable := CreateMergingStateTable;
end;

class destructor TdxTableCellVerticalMergingStateDestination.Finalize;
begin
  FMergingStateTable.Free;
end;

class function TdxTableCellVerticalMergingStateDestination.CreateMergingStateTable: TdxEnumeratedDictionary<TdxMergingState, string>;
begin
  Result := TdxEnumeratedDictionary<TdxMergingState, string>.Create;
  Result.Add(TdxMergingState.None, 'none');
  Result.Add(TdxMergingState.Restart, 'restart');
  Result.Add(TdxMergingState.Continue, 'continue');
end;

procedure TdxTableCellVerticalMergingStateDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  AState: TdxMergingState;
begin
  AState := Importer.GetWpEnumValue<TdxMergingState>(AReader, 'val', FMergingStateTable, TdxMergingState.Continue);
  CellProperties.VerticalMerging := AState;
end;

{ TdxTableCellShadingDestination }

procedure TdxTableCellShadingDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  APattern: TdxShadingPattern;
  AFill, APatternColor, AActualColor: TdxAlphaColor;
  AImporter: TdxWordProcessingMLBaseImporter;
begin
  if not TdxAlphaColors.IsTransparentOrEmpty(CellProperties.BackgroundColor) then
    Exit;
  AImporter := TdxWordProcessingMLBaseImporter(Importer);
  APattern := AImporter.GetShadingPatternDef(AReader, 'val', TdxShadingPattern.Clear);
  AFill := AImporter.GetWpSTColorValue(AReader, 'fill', TdxAlphaColors.Empty);
  APatternColor := AImporter.GetWpSTColorValue(AReader, 'color', TdxAlphaColors.Empty);
  AActualColor := TdxShadingHelper.GetActualBackColor(AFill, APatternColor, APattern);
  if AActualColor <> TdxAlphaColors.Empty then
    CellProperties.BackgroundColor := AActualColor;
end;

{ TdxTableCellColumnSpanDestination }

procedure TdxTableCellColumnSpanDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  AValue: Integer;
begin
  AValue := Importer.GetWpSTIntegerValue(AReader, 'val');
  if (AValue <> MinInt) and (AValue <> 0) then
    CellProperties.ColumnSpan := AValue;
end;

{ TdxTableCellFitTextDestination }

procedure TdxTableCellFitTextDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  CellProperties.FitText := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxTableCellNoWrapDestination }

procedure TdxTableCellNoWrapDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  CellProperties.NoWrap := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxTableCellHideMarkDestination }

procedure TdxTableCellHideMarkDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  CellProperties.HideCellMark := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxTableCellTextDirectionDestination }

class constructor TdxTableCellTextDirectionDestination.Initialize;
begin
  FTextDirectionTable := CreateTextDirectionTable;
end;

class destructor TdxTableCellTextDirectionDestination.Finalize;
begin
  FTextDirectionTable.Free;
end;

class function TdxTableCellTextDirectionDestination.CreateTextDirectionTable: TdxEnumeratedDictionary<TdxTextDirection, string>;
begin
  Result := TdxEnumeratedDictionary<TdxTextDirection, string>.Create;
  Result.Add(TdxTextDirection.BottomToTopLeftToRight, 'btLr');
  Result.Add(TdxTextDirection.LeftToRightTopToBottom, 'lrTb');
  Result.Add(TdxTextDirection.LeftToRightTopToBottomRotated, 'lrTbV');
  Result.Add(TdxTextDirection.TopToBottomLeftToRightRotated, 'tbLrV');
  Result.Add(TdxTextDirection.TopToBottomRightToLeft, 'tbRl');
  Result.Add(TdxTextDirection.TopToBottomRightToLeftRotated, 'tbRlV');
end;

procedure TdxTableCellTextDirectionDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  CellProperties.TextDirection := Importer.GetWpEnumValue<TdxTextDirection>(AReader, 'val', FTextDirectionTable,
    TdxTextDirection.LeftToRightTopToBottom);
end;

{ TdxTableCellVerticalAlignmentDestination }

class constructor TdxTableCellVerticalAlignmentDestination.Initialize;
begin
  FVerticalAlignmentTable := CreateVerticalAlignmentTable;
end;

class destructor TdxTableCellVerticalAlignmentDestination.Finalize;
begin
  FVerticalAlignmentTable.Free;
end;

class function TdxTableCellVerticalAlignmentDestination.CreateVerticalAlignmentTable: TdxEnumeratedDictionary<TdxVerticalAlignment, string>;
begin
  Result := TdxEnumeratedDictionary<TdxVerticalAlignment, string>.Create;
  Result.Add(TdxVerticalAlignment.Both, 'both');
  Result.Add(TdxVerticalAlignment.Bottom, 'bottom');
  Result.Add(TdxVerticalAlignment.Center, 'center');
  Result.Add(TdxVerticalAlignment.Top, 'top');
end;

procedure TdxTableCellVerticalAlignmentDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  CellProperties.VerticalAlignment := Importer.GetWpEnumValue<TdxVerticalAlignment>(AReader, 'val', FVerticalAlignmentTable,
    TdxVerticalAlignment.Top);
end;

{ TdxTableCellConditionalFormattingDestination }

procedure TdxTableCellConditionalFormattingDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AStrValue: string;
  AValue: Integer;
  ACellConditionalFormatting: TdxConditionalTableStyleFormattingTypes absolute AValue;
begin
  AStrValue := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);
  if AStrValue = '' then
    Exit;
  AValue := TdxStringHelper.ToInt32(AStrValue, 2);
  CellProperties.CellConditionalFormatting := ACellConditionalFormatting;
end;

{ TdxTableDisabledDestination }

class constructor TdxTableDisabledDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTableDisabledDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTableDisabledDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('tr', OnRow);
end;

function TdxTableDisabledDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxTableDisabledDestination.OnRow(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableRowDisabledDestination.Create(AImporter);
end;

{ TdxTableRowDisabledDestination }

class constructor TdxTableRowDisabledDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTableRowDisabledDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTableRowDisabledDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('tc', OnCell);
end;

function TdxTableRowDisabledDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxTableRowDisabledDestination.OnCell(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableCellDisabledDestination.Create(AImporter);
end;

{ TdxTableCellDisabledDestination }

class constructor TdxTableCellDisabledDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTableCellDisabledDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTableCellDisabledDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('p', OnParagraph);
  Result.Add('tbl', OnTable);
  Result.Add('bookmarkStart', OnBookmarkStart);
  Result.Add('bookmarkEnd', OnBookmarkEnd);
  Result.Add('permStart', OnRangePermissionStart);
  Result.Add('permEnd', OnRangePermissionEnd);
end;

function TdxTableCellDisabledDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxTableCellDisabledDestination.OnParagraph(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateParagraphDestination;
end;

class function TdxTableCellDisabledDestination.OnTable(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTableDisabledDestination.Create(AImporter);
end;

class function TdxTableCellDisabledDestination.OnBookmarkStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateBookmarkStartElementDestination(AReader);
end;

class function TdxTableCellDisabledDestination.OnBookmarkEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateBookmarkEndElementDestination(AReader);
end;

class function TdxTableCellDisabledDestination.OnRangePermissionStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxRangePermissionStartElementDestination.Create(AImporter);
end;

class function TdxTableCellDisabledDestination.OnRangePermissionEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxRangePermissionEndElementDestination.Create(AImporter);
end;

end.
