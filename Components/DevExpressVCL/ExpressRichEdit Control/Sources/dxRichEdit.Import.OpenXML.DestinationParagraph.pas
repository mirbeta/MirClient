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

unit dxRichEdit.Import.OpenXML.DestinationParagraph;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxRichEdit.Utils.Types,
  dxXMLReader,
  dxRichEdit.Options,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.DestinationSection,
  dxRichEdit.Import.OpenXML.DestinationRunProperties;

type
  TdxParagraphPropertiesDestination = class;

  { TdxParagraphDestination }

  TdxParagraphDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    FShouldInsertSection: Boolean;
    FListLevelIndex: Integer;
    FNumberingId: Integer;
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    function CreateParagraphPropertiesDestination: TdxParagraphPropertiesDestination; virtual;
    class function OnParagraphProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRun(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnPicture(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnFieldSimple(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnHyperlink(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnComplexFieldMarker(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnFieldInstruction(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnBookmarkStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnBookmarkEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRangePermissionStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRangePermissionEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnCommentStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnCommentEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnSmartTag(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnStructuredDocument(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnCustomXml(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnDrawing(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    procedure AddNumbering(AParagraph: TdxParagraph); virtual;
    function SuppressInsertParagraph: Boolean; virtual;
    function InsertParagraph: TdxParagraph; virtual;
    procedure InsertSection; virtual;
    procedure ApplyParagraphProperties(AParagraphIndex: TdxParagraphIndex); virtual;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxParagraphDestination; static;
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    procedure InsertParagraphCore;
    function IsChoiceNamespaceSupported(const ARequeriesNamespaceUri: string): Boolean; override;

    property ShouldInsertSection: Boolean read FShouldInsertSection write FShouldInsertSection;
    property ListLevelIndex: Integer read FListLevelIndex write FListLevelIndex;
    property NumberingId: Integer read FNumberingId write FNumberingId;
  end;

  { TdxSmartTagDestination }

  TdxSmartTagDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnSmartTag(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRun(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxParagraphPropertiesBaseDestination }

  TdxParagraphPropertiesBaseDestination = class abstract(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FParagraphProperties: IdxParagraphProperties;
    FTabs: TdxTabFormattingInfo;
  protected
    function GetNumberingId: Integer; virtual; abstract;
    procedure SetNumberingId(const AValue: Integer); virtual; abstract;
    function GetListLevelIndex: Integer; virtual; abstract;
    procedure SetListLevelIndex(const AValue: Integer); virtual; abstract;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
    function GetElementHandlerTable: TdxElementHandlerTable; override;

    property ParagraphFormatting: IdxParagraphProperties read FParagraphProperties;
    property Tabs: TdxTabFormattingInfo read FTabs;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const AParagraphProperties: IdxParagraphProperties; ATabs: TdxTabFormattingInfo);
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxParagraphPropertiesBaseDestination; static;
    class function GetParagraphProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): IdxParagraphProperties; static;
    class function GetTabs(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTabFormattingInfo; static;
    class function OnRunProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnSpacing(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnIndents(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnSuppressHyphenation(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnSuppressLineNumbers(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnContextualSpacing(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnPageBreakBefore(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnKeepWithNext(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnKeepLinesTogether(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnWidowOrphanControl(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnAlignment(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTabs(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnOutlineLevel(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnBackground(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnFrameProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnParagraphBorders(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;

    property NumberingId: Integer read GetNumberingId write SetNumberingId;
    property ListLevelIndex: Integer read GetListLevelIndex write SetListLevelIndex;
  end;

  { TdxParagraphPropertiesDestination }

  TdxParagraphPropertiesDestination = class(TdxParagraphPropertiesBaseDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FParagraphDestination: TdxParagraphDestination;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    function GetListLevelIndex: Integer; override;
    procedure SetListLevelIndex(const AValue: Integer); override;
    function GetNumberingId: Integer; override;
    procedure SetNumberingId(const AValue: Integer); override;
    function CreateSectionDestination: TdxSectionDestinationBase; virtual;
    class function OnStyle(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnSection(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnNumbering(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AParagraphDestination: TdxParagraphDestination;
      AParagraphFormatting: TdxParagraphFormattingBase; ATabs: TdxTabFormattingInfo);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxParagraphPropertiesDestination; static;
  end;

  { TdxParagraphMarkRunPropertiesDestination }

  TdxParagraphMarkRunPropertiesDestination = class(TdxRunPropertiesBaseDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    class function OnStyle(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxParagraphMarkRunStyleReferenceDestination }

  TdxParagraphMarkRunStyleReferenceDestination = class(TdxRunStyleReferenceBaseDestination)
  protected
    procedure AssignCharacterStyleIndex(AValue: Integer); override;
  end;

  { TdxParagraphStyleReferenceBaseDestination }

  TdxParagraphStyleReferenceBaseDestination = class abstract(TdxLeafElementDestination)
  protected
    procedure AssignParagraphStyleIndex(AValue: Integer); virtual; abstract;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    function LookupStyleIndex(const AValue: string): Integer;
  end;

  { TdxParagraphStyleReferenceDestination }

  TdxParagraphStyleReferenceDestination = class(TdxParagraphStyleReferenceBaseDestination)
  protected
    procedure AssignParagraphStyleIndex(AValue: Integer); override;
  end;

  { TdxParagraphNumberingReferenceDestination }

  TdxParagraphNumberingReferenceDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FParagraphPropertiesDestination: TdxParagraphPropertiesBaseDestination;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
    function GetListLevelIndex: Integer;
    procedure SetListLevelIndex(const AValue: Integer);
    function GetNumberingId: Integer;
    procedure SetNumberingId(const AValue: Integer);
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnLevel(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnNumberingId(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AParagraphPropertiesDestination: TdxParagraphPropertiesBaseDestination);
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxParagraphNumberingReferenceDestination; static;

    property ListLevelIndex: Integer read GetListLevelIndex write SetListLevelIndex;
    property NumberingId: Integer read GetNumberingId write SetNumberingId;
  end;

  { TdxParagraphNumberingReferenceLevelDestination }

  TdxParagraphNumberingReferenceLevelDestination = class(TdxLeafElementDestination)
  strict private
    FParentDestination: TdxParagraphNumberingReferenceDestination;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AParentDestination: TdxParagraphNumberingReferenceDestination);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxParagraphNumberingReferenceNumberingIdDestination }

  TdxParagraphNumberingReferenceNumberingIdDestination = class(TdxLeafElementDestination)
  strict private
    FParentDestination: TdxParagraphNumberingReferenceDestination;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AParentDestination: TdxParagraphNumberingReferenceDestination);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxParagraphFormattingLeafElementDestination }

  TdxParagraphFormattingLeafElementDestination = class abstract(TdxLeafElementDestination)
  strict private
    FParagraphProperties: IdxParagraphProperties;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const AParagraphProperties: IdxParagraphProperties);

    property ParagraphProperties: IdxParagraphProperties read FParagraphProperties;
  end;

  { TdxParagraphSpacingDestination }

  TdxParagraphSpacingDestination = class(TdxParagraphFormattingLeafElementDestination)
  protected
    procedure ApplyLineSpacingValue(ALineSpacing: Integer; const ALineSpacingRule: string); virtual;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxParagraphIndentsDestination }

  TdxParagraphIndentsDestination = class(TdxParagraphFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxParagraphAlignmentDestination }

  TdxParagraphAlignmentDestination = class(TdxParagraphFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxSuppressHyphenationDestination }

  TdxSuppressHyphenationDestination = class(TdxParagraphFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxSuppressLineNumbersDestination }

  TdxSuppressLineNumbersDestination = class(TdxParagraphFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxContextualSpacingDestination }

  TdxContextualSpacingDestination = class(TdxParagraphFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxPageBreakBeforeDestination }

  TdxPageBreakBeforeDestination = class(TdxParagraphFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxKeepWithNextDestination }

  TdxKeepWithNextDestination = class(TdxParagraphFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxKeepLinesTogetherDestination }

  TdxKeepLinesTogetherDestination = class(TdxParagraphFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxWidowOrphanControlDestination }

  TdxWidowOrphanControlDestination = class(TdxParagraphFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxOutlineLevelDestination }

  TdxOutlineLevelDestination = class(TdxParagraphFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxParagraphBackgroundDestination }

  TdxParagraphBackgroundDestination = class(TdxParagraphFormattingLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxFramePropertiesDestination }

  TdxFramePropertiesDestination = class(TdxLeafElementDestination)
  strict private
    FFrameProperties: TdxParagraphFrameFormattingBase;
    function GetFrameProperties: TdxParagraphFrameFormattingBase;
  protected
    procedure ApplyHorizontalRuleValue(const AHorizontalRule: string); virtual;
    procedure ApplyWrapTypeValue(const AWrapType: string); virtual;
    procedure ApplyVerticalPositionTypeValue(const AVerticalPositionType: string); virtual;
    procedure ApplyHorizontalPositionTypeValue(const AHorizontalPositionType: string); virtual;
    procedure ApplyHorizontalPositionAlignmentValue(const AHorizontalPositionAlignment: string); virtual;
    procedure ApplyVerticalPositionAlignmentValue(const AVerticalPositionAlignment: string); virtual;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;

    property FrameProperties: TdxParagraphFrameFormattingBase read GetFrameProperties;
  end;

  { TdxParagraphBordersDestination }

  TdxParagraphBordersDestination = class(TdxParagraphFormattingLeafElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxParagraphBordersDestination; static;
    class function GetParagraphProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): IdxParagraphProperties; static;
    class function OnTopBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnLeftBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnBottomBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnRightBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const AParagraphProperties: IdxParagraphProperties);
  end;

  { TdxParagraphBorderDestination }

  TdxParagraphBorderDestination = class(TdxBorderDestination)
  strict private
    FBorder: TdxBorderInfo;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ABorder: TdxBorderInfo);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxTabsDestination }

  TdxTabsDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FTabs: TdxTabFormattingInfo;
  protected
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetTabs(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTabFormattingInfo; static;
    class function OnTab(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ATabs: TdxTabFormattingInfo);
  end;

  { TdxTabsLeafElementDestination }

  TdxTabsLeafElementDestination = class abstract(TdxLeafElementDestination)
  strict private
    FTabs: TdxTabFormattingInfo;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ATabs: TdxTabFormattingInfo);

    property Tabs: TdxTabFormattingInfo read FTabs;
  end;

  { TdxTabDestination }

  TdxTabDestination = class(TdxTabsLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxInnerSectionDestination }

  TdxInnerSectionDestination = class(TdxSectionDestination);

implementation

uses
  Math, Contnrs, dxGenerics,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.TableFormatting,
  dxCharacters,
  dxRichEdit.Export.OpenXML,
  dxRichEdit.Export.OpenXML.WordProcessingMLBaseExporter,
  dxRichEdit.Import.OpenXML.DestinationNumbering,
  dxRichEdit.Import.OpenXML.DestinationText,
  dxRichEdit.Import.OpenXML.DestinationDrawing,
  dxRichEdit.Import.OpenXML.DestinationPicture,
  dxRichEdit.Import.OpenXML.DestinationFields,
  dxRichEdit.Import.OpenXML.DestinationComment,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter;

{ TdxParagraphDestination }

constructor TdxParagraphDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
begin
  inherited Create(AImporter);
  FListLevelIndex := MinInt;
  FNumberingId := MinInt;
  AImporter.DocumentModel.ResetParagraphFormatting(AImporter.Position.ParagraphFormatting);
  AImporter.Position.ParagraphStyleIndex := 0;
  AImporter.DocumentModel.ResetCharacterFormatting(AImporter.Position.ParagraphMarkCharacterFormatting);
  AImporter.Position.ParagraphMarkCharacterStyleIndex := 0;
  AImporter.Position.ParagraphTabs.Clear;
end;

class constructor TdxParagraphDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxParagraphDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxParagraphDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('pPr', OnParagraphProperties);
  Result.Add('r', OnRun);
  Result.Add('pict', OnPicture);
  Result.Add('fldSimple', OnFieldSimple);
  Result.Add('hyperlink', OnHyperlink);
  Result.Add('fldChar', OnComplexFieldMarker);
  Result.Add('instrText', OnFieldInstruction);
  Result.Add('bookmarkStart', OnBookmarkStart);
  Result.Add('bookmarkEnd', OnBookmarkEnd);
  Result.Add('permStart', OnRangePermissionStart);
  Result.Add('permEnd', OnRangePermissionEnd);
  Result.Add('commentRangeStart', OnCommentStart);
  Result.Add('commentRangeEnd', OnCommentEnd);
  Result.Add('smartTag', OnSmartTag);
  Result.Add('sdt', OnStructuredDocument);
  Result.Add('customXml', OnCustomXml);
  Result.Add('drawing', OnDrawing);
end;

function TdxParagraphDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxParagraphDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxParagraphDestination;
begin
  Result := TdxParagraphDestination(AImporter.PeekDestination);
end;

function TdxParagraphDestination.CreateParagraphPropertiesDestination: TdxParagraphPropertiesDestination;
begin
  Result := TdxParagraphPropertiesDestination.Create(Importer, Self, Importer.Position.ParagraphFormatting, Importer.Position.ParagraphTabs);
end;

class function TdxParagraphDestination.OnParagraphProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  AImporter.DocumentModel.ResetParagraphFormatting(AImporter.Position.ParagraphFormatting);
  Result := GetThis(AImporter).CreateParagraphPropertiesDestination;
end;

class function TdxParagraphDestination.OnRun(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateRunDestination;
end;

class function TdxParagraphDestination.OnPicture(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxInlinePictureDestination.Create(AImporter);
end;

class function TdxParagraphDestination.OnFieldSimple(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxFieldSimpleDestination.Create(AImporter);
end;

class function TdxParagraphDestination.OnHyperlink(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxHyperlinkDestination.Create(AImporter);
end;

class function TdxParagraphDestination.OnComplexFieldMarker(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxFieldCharDestination.Create(AImporter);
end;

class function TdxParagraphDestination.OnFieldInstruction(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTextDestination.Create(AImporter);
end;

class function TdxParagraphDestination.OnBookmarkStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateBookmarkStartElementDestination(AReader);
end;

class function TdxParagraphDestination.OnBookmarkEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateBookmarkEndElementDestination(AReader);
end;

class function TdxParagraphDestination.OnRangePermissionStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxRangePermissionStartElementDestination.Create(AImporter);
end;

class function TdxParagraphDestination.OnRangePermissionEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxRangePermissionEndElementDestination.Create(AImporter);
end;

class function TdxParagraphDestination.OnCommentStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxCommentStartElementDestination.Create(AImporter);
end;

class function TdxParagraphDestination.OnCommentEnd(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxCommentEndElementDestination.Create(AImporter);
end;

class function TdxParagraphDestination.OnSmartTag(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSmartTagDestination.Create(AImporter);
end;

class function TdxParagraphDestination.OnStructuredDocument(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxStructuredDocumentDestination.Create(AImporter);
end;

class function TdxParagraphDestination.OnCustomXml(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxCustomXmlDestination.Create(AImporter);
end;

class function TdxParagraphDestination.OnDrawing(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingDestination.Create(AImporter);
end;

procedure TdxParagraphDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
end;

procedure TdxParagraphDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  Importer.Position.CharacterFormatting.CopyFrom(Importer.Position.ParagraphMarkCharacterFormatting);
  Importer.Position.CharacterStyleIndex := Importer.Position.ParagraphMarkCharacterStyleIndex;

  if ShouldInsertSection then
    InsertSection
  else
    InsertParagraphCore;

  Importer.Position.CharacterStyleIndex := 0;
end;

procedure TdxParagraphDestination.InsertParagraphCore;
var
  AParagraph: TdxParagraph;
begin
  if SuppressInsertParagraph then
  begin
    PieceTable.InsertTextCore(Importer.Position, ' ');
    Exit;
  end;
  AParagraph := InsertParagraph;
  if TdxDocumentFormatsHelper.ShouldInsertNumbering(DocumentModel) then
    AddNumbering(AParagraph);
end;

function TdxParagraphDestination.IsChoiceNamespaceSupported(const ARequeriesNamespaceUri: string): Boolean;
begin
  if AnsiSameText(ARequeriesNamespaceUri, TdxOpenXmlExporter.WpsNamespace) then
    Exit(True);

  Result := inherited IsChoiceNamespaceSupported(ARequeriesNamespaceUri);
end;

procedure TdxParagraphDestination.AddNumbering(AParagraph: TdxParagraph);
var
  AListInfo: TdxOpenXmlNumberingListInfo;
begin

  FListLevelIndex := Max(0, FListLevelIndex);
  if NumberingId <> MinInt then
  begin
    AListInfo := TdxWordProcessingMLBaseImporter(Importer).ListInfos.FindById(NumberingId);

    if not TdxDocumentFormatsHelper.ShouldInsertMultiLevelNumbering(DocumentModel) then
      ListLevelIndex := 0;
    if AListInfo <> nil then
      PieceTable.AddNumberingListToParagraph(AParagraph, AListInfo.ListIndex, ListLevelIndex)
    else
      if (NumberingId = NumberingListIndexNoNumberingList) and (AParagraph.ParagraphStyle.GetNumberingListIndex >= NumberingListIndexMinValue) then
      begin
        PieceTable.AddNumberingListToParagraph(AParagraph, NumberingListIndexNoNumberingList, ListLevelIndex);

        if not AParagraph.ParagraphProperties.UseFirstLineIndentType then
          AParagraph.FirstLineIndentType := TdxParagraphFirstLineIndent.None;
        if not AParagraph.ParagraphProperties.UseFirstLineIndent then
          AParagraph.FirstLineIndent := 0;
        if not AParagraph.ParagraphProperties.UseLeftIndent then
          AParagraph.LeftIndent := 0;
      end;
  end;
end;

function TdxParagraphDestination.SuppressInsertParagraph: Boolean;
begin
  Result := not DocumentModel.DocumentCapabilities.ParagraphsAllowed;
end;

function TdxParagraphDestination.InsertParagraph: TdxParagraph;
var
  AParagraphIndex: TdxParagraphIndex;
begin
  AParagraphIndex := Importer.Position.ParagraphIndex;
  PieceTable.InsertParagraphCore(Importer.Position);
  ApplyParagraphProperties(AParagraphIndex);
  Result := PieceTable.Paragraphs[AParagraphIndex];
end;

procedure TdxParagraphDestination.InsertSection;
var
  ATransaction: TdxHistoryTransaction;
  AParagraphIndex: TdxParagraphIndex;
begin
  Assert(PieceTable.IsMain);
  ATransaction := TdxHistoryTransaction.Create(DocumentModel.History);
  try
    AParagraphIndex := Importer.Position.ParagraphIndex;

    PieceTable.InsertSectionParagraphCore(Importer.Position);
    ApplyParagraphProperties(AParagraphIndex);
    DocumentModel.SafeEditor.PerformInsertSectionCore(AParagraphIndex);
    Importer.CurrentSection := DocumentModel.Sections.Last;

    Importer.CurrentSection.Reset;
  finally
    ATransaction.Free;
  end;
end;

procedure TdxParagraphDestination.ApplyParagraphProperties(AParagraphIndex: TdxParagraphIndex);
var
  AParagraph: TdxParagraph;
begin
  AParagraph := PieceTable.Paragraphs[AParagraphIndex];
  AParagraph.ParagraphStyleIndex := Importer.Position.ParagraphStyleIndex;
  AParagraph.ParagraphProperties.CopyFrom(Importer.Position.ParagraphFormatting);
  if ((Importer.Position.ParagraphFrameFormatting.Options.Value <> []) and (AParagraph.FrameProperties = nil)) and
    DocumentModel.DocumentCapabilities.ParagraphFramesAllowed then
  begin
    AParagraph.CreateFrameProperties;
    AParagraph.FrameProperties.CopyFrom(Importer.Position.ParagraphFrameFormatting);
  end;
  Importer.Position.ParagraphFrameFormatting.ReplaceInfo(
    Importer.DocumentModel.Cache.ParagraphFrameFormattingInfoCache.DefaultItem,
    TdxParagraphFrameFormattingOptions.Create);
  AParagraph.SetOwnTabs(Importer.Position.ParagraphTabs);
end;

{ TdxSmartTagDestination }

class constructor TdxSmartTagDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxSmartTagDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxSmartTagDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('smartTag', OnSmartTag);
  Result.Add('r', OnRun);
end;

function TdxSmartTagDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxSmartTagDestination.OnSmartTag(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSmartTagDestination.Create(AImporter);
end;

class function TdxSmartTagDestination.OnRun(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateRunDestination;
end;

{ TdxParagraphPropertiesBaseDestination }

constructor TdxParagraphPropertiesBaseDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  const AParagraphProperties: IdxParagraphProperties; ATabs: TdxTabFormattingInfo);
begin
  inherited Create(AImporter);
  Assert(AParagraphProperties <> nil);
  Assert(ATabs <> nil);
  FParagraphProperties := AParagraphProperties;
  FTabs := ATabs;
end;

class constructor TdxParagraphPropertiesBaseDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxParagraphPropertiesBaseDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxParagraphPropertiesBaseDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('rPr', OnRunProperties);
  Result.Add('spacing', OnSpacing);
  Result.Add('ind', OnIndents);
  Result.Add('suppressAutoHyphens', OnSuppressHyphenation);
  Result.Add('suppressLineNumbers', OnSuppressLineNumbers);
  Result.Add('supressLineNumbers', OnSuppressLineNumbers);
  Result.Add('contextualSpacing', OnContextualSpacing);
  Result.Add('pageBreakBefore', OnPageBreakBefore);
  Result.Add('keepNext', OnKeepWithNext);
  Result.Add('keepLines', OnKeepLinesTogether);
  Result.Add('widowControl', OnWidowOrphanControl);
  Result.Add('jc', OnAlignment);
  Result.Add('tabs', OnTabs);
  Result.Add('outlineLvl', OnOutlineLevel);
  Result.Add('shd', OnBackground);

  Result.Add('framePr', OnFrameProperties);
  Result.Add('pBdr', OnParagraphBorders);
end;

function TdxParagraphPropertiesBaseDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxParagraphPropertiesBaseDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxParagraphPropertiesBaseDestination;
begin
  Result := TdxParagraphPropertiesBaseDestination(AImporter.PeekDestination);
end;

class function TdxParagraphPropertiesBaseDestination.GetParagraphProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): IdxParagraphProperties;
begin
  Result := GetThis(AImporter).FParagraphProperties;
end;

class function TdxParagraphPropertiesBaseDestination.GetTabs(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTabFormattingInfo;
begin
  Result := GetThis(AImporter).FTabs;
end;

class function TdxParagraphPropertiesBaseDestination.OnRunProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  AImporter.Position.ParagraphMarkCharacterFormatting.ReplaceInfo(AImporter.DocumentModel.Cache.CharacterFormattingInfoCache.DefaultItem,
    TdxCharacterFormattingOptions.Create(TdxCharacterFormattingOptions.MaskUseNone));
  Result := TdxParagraphMarkRunPropertiesDestination.Create(AImporter, AImporter.Position.ParagraphMarkCharacterFormatting);
end;

class function TdxParagraphPropertiesBaseDestination.OnSpacing(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxParagraphSpacingDestination.Create(AImporter, GetParagraphProperties(AImporter));
end;

class function TdxParagraphPropertiesBaseDestination.OnIndents(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxParagraphIndentsDestination.Create(AImporter, GetParagraphProperties(AImporter));
end;

class function TdxParagraphPropertiesBaseDestination.OnSuppressHyphenation(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSuppressHyphenationDestination.Create(AImporter, GetParagraphProperties(AImporter));
end;

class function TdxParagraphPropertiesBaseDestination.OnSuppressLineNumbers(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSuppressLineNumbersDestination.Create(AImporter, GetParagraphProperties(AImporter));
end;

class function TdxParagraphPropertiesBaseDestination.OnContextualSpacing(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxContextualSpacingDestination.Create(AImporter, GetParagraphProperties(AImporter));
end;

class function TdxParagraphPropertiesBaseDestination.OnPageBreakBefore(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxPageBreakBeforeDestination.Create(AImporter, GetParagraphProperties(AImporter));
end;

class function TdxParagraphPropertiesBaseDestination.OnKeepWithNext(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxKeepWithNextDestination.Create(AImporter, GetParagraphProperties(AImporter));
end;

class function TdxParagraphPropertiesBaseDestination.OnKeepLinesTogether(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxKeepLinesTogetherDestination.Create(AImporter, GetParagraphProperties(AImporter));
end;

class function TdxParagraphPropertiesBaseDestination.OnWidowOrphanControl(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWidowOrphanControlDestination.Create(AImporter, GetParagraphProperties(AImporter));
end;

class function TdxParagraphPropertiesBaseDestination.OnAlignment(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxParagraphAlignmentDestination.Create(AImporter, GetParagraphProperties(AImporter));
end;

class function TdxParagraphPropertiesBaseDestination.OnTabs(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTabsDestination.Create(AImporter, GetTabs(AImporter));
end;

class function TdxParagraphPropertiesBaseDestination.OnOutlineLevel(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxOutlineLevelDestination.Create(AImporter, GetParagraphProperties(AImporter));
end;

class function TdxParagraphPropertiesBaseDestination.OnBackground(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxParagraphBackgroundDestination.Create(AImporter, GetParagraphProperties(AImporter));
end;

class function TdxParagraphPropertiesBaseDestination.OnFrameProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxFramePropertiesDestination.Create(AImporter);
end;

class function TdxParagraphPropertiesBaseDestination.OnParagraphBorders(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxParagraphBordersDestination.Create(AImporter, GetParagraphProperties(AImporter));
end;

{ TdxParagraphPropertiesDestination }

constructor TdxParagraphPropertiesDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AParagraphDestination: TdxParagraphDestination; AParagraphFormatting: TdxParagraphFormattingBase; ATabs: TdxTabFormattingInfo);
begin
  inherited Create(AImporter, AParagraphFormatting, ATabs);
  Assert(AParagraphDestination <> nil);
  FParagraphDestination := AParagraphDestination;
  AImporter.Position.ParagraphMarkCharacterFormatting.ReplaceInfo(AImporter.DocumentModel.Cache.CharacterFormattingInfoCache.DefaultItem,
    TdxCharacterFormattingOptions.Create(TdxCharacterFormattingOptions.MaskUseNone));
  AImporter.Position.ParagraphMarkCharacterStyleIndex := 0;
  AImporter.Position.ParagraphStyleIndex := 0;
  ATabs.Clear;
end;

class constructor TdxParagraphPropertiesDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxParagraphPropertiesDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxParagraphPropertiesDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxParagraphPropertiesBaseDestination.CreateElementHandlerTable;
  Result.Add('pStyle', OnStyle);
  Result.Add('sectPr', OnSection);
  Result.Add('numPr', OnNumbering);
end;

function TdxParagraphPropertiesDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

function TdxParagraphPropertiesDestination.GetListLevelIndex: Integer;
begin
  Result := FParagraphDestination.ListLevelIndex;
end;

procedure TdxParagraphPropertiesDestination.SetListLevelIndex(const AValue: Integer);
begin
  FParagraphDestination.ListLevelIndex := AValue;
end;

function TdxParagraphPropertiesDestination.GetNumberingId: Integer;
begin
  Result := FParagraphDestination.NumberingId;
end;

procedure TdxParagraphPropertiesDestination.SetNumberingId(const AValue: Integer);
begin
  FParagraphDestination.NumberingId := AValue;
end;

function TdxParagraphPropertiesDestination.CreateSectionDestination: TdxSectionDestinationBase;
begin
  Result := TdxInnerSectionDestination.Create(Importer);
end;

procedure TdxParagraphPropertiesDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AParagraphFormatting: TdxParagraphFormattingBase;
begin
  AParagraphFormatting := TdxParagraphFormattingBase(ParagraphFormatting);
  AParagraphFormatting.BeginUpdate;
end;

procedure TdxParagraphPropertiesDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  AParagraphFormatting: TdxParagraphFormattingBase;
begin
  AParagraphFormatting := TdxParagraphFormattingBase(ParagraphFormatting);
  AParagraphFormatting.EndUpdate;
end;

class function TdxParagraphPropertiesDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxParagraphPropertiesDestination;
begin
  Result := TdxParagraphPropertiesDestination(AImporter.PeekDestination);
end;

class function TdxParagraphPropertiesDestination.OnStyle(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxParagraphStyleReferenceDestination.Create(AImporter);
end;

class function TdxParagraphPropertiesDestination.OnSection(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
var
  AThisDestination: TdxParagraphPropertiesDestination;
begin
  AThisDestination := GetThis(AImporter);
  AThisDestination.FParagraphDestination.ShouldInsertSection := AImporter.DocumentModel.DocumentCapabilities.SectionsAllowed and
    not TdxWordProcessingMLBaseImporter(AImporter).InsideTable;
  Result := AThisDestination.CreateSectionDestination;
end;

class function TdxParagraphPropertiesDestination.OnNumbering(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxParagraphNumberingReferenceDestination.Create(AImporter, GetThis(AImporter));
end;

{ TdxParagraphMarkRunPropertiesDestination }

class constructor TdxParagraphMarkRunPropertiesDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxParagraphMarkRunPropertiesDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxParagraphMarkRunPropertiesDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxRunPropertiesBaseDestination.CreateElementHandlerTable;
  Result.Add('rStyle', OnStyle);
end;

function TdxParagraphMarkRunPropertiesDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

procedure TdxParagraphMarkRunPropertiesDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ACharacterFormatting: TdxCharacterFormattingBase;
begin
  ACharacterFormatting := TdxCharacterFormattingBase(CharacterProperties);
  ACharacterFormatting.BeginUpdate;
end;

procedure TdxParagraphMarkRunPropertiesDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  ACharacterFormatting: TdxCharacterFormattingBase;
begin
  ACharacterFormatting := TdxCharacterFormattingBase(CharacterProperties);
  ACharacterFormatting.EndUpdate;
end;

class function TdxParagraphMarkRunPropertiesDestination.OnStyle(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxParagraphMarkRunStyleReferenceDestination.Create(AImporter);
end;

{ TdxParagraphMarkRunStyleReferenceDestination }

procedure TdxParagraphMarkRunStyleReferenceDestination.AssignCharacterStyleIndex(AValue: Integer);
begin
  Importer.Position.ParagraphMarkCharacterStyleIndex := AValue;
end;

{ TdxParagraphStyleReferenceBaseDestination }

procedure TdxParagraphStyleReferenceBaseDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: string;
  AStyleIndex: Integer;
begin
  AValue := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);
  if AValue <> '' then
  begin
    AStyleIndex := LookupStyleIndex(AValue);
    if AStyleIndex >= 0 then
      AssignParagraphStyleIndex(AStyleIndex);
  end;
end;

function TdxParagraphStyleReferenceBaseDestination.LookupStyleIndex(const AValue: string): Integer;
begin
  Result := TdxWordProcessingMLBaseImporter(Importer).LookupParagraphStyleIndex(AValue);
end;

{ TdxParagraphStyleReferenceDestination }

procedure TdxParagraphStyleReferenceDestination.AssignParagraphStyleIndex(AValue: Integer);
begin
  Importer.Position.ParagraphStyleIndex := AValue;
end;

{ TdxParagraphNumberingReferenceDestination }

constructor TdxParagraphNumberingReferenceDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AParagraphPropertiesDestination: TdxParagraphPropertiesBaseDestination);
begin
  inherited Create(AImporter);
  Assert(AParagraphPropertiesDestination <> nil);
  FParagraphPropertiesDestination := AParagraphPropertiesDestination;
end;

class constructor TdxParagraphNumberingReferenceDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxParagraphNumberingReferenceDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxParagraphNumberingReferenceDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('ilvl', OnLevel);
  Result.Add('numId', OnNumberingId);
end;

function TdxParagraphNumberingReferenceDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

function TdxParagraphNumberingReferenceDestination.GetListLevelIndex: Integer;
begin
  Result := FParagraphPropertiesDestination.ListLevelIndex;
end;

procedure TdxParagraphNumberingReferenceDestination.SetListLevelIndex(const AValue: Integer);
begin
  FParagraphPropertiesDestination.ListLevelIndex := AValue;
end;

function TdxParagraphNumberingReferenceDestination.GetNumberingId: Integer;
begin
  Result := FParagraphPropertiesDestination.NumberingId;
end;

procedure TdxParagraphNumberingReferenceDestination.SetNumberingId(const AValue: Integer);
begin
  FParagraphPropertiesDestination.NumberingId := AValue;
end;

class function TdxParagraphNumberingReferenceDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxParagraphNumberingReferenceDestination;
begin
  Result := TdxParagraphNumberingReferenceDestination(AImporter.PeekDestination);
end;

class function TdxParagraphNumberingReferenceDestination.OnLevel(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxParagraphNumberingReferenceLevelDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxParagraphNumberingReferenceDestination.OnNumberingId(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxParagraphNumberingReferenceNumberingIdDestination.Create(AImporter, GetThis(AImporter));
end;

{ TdxParagraphNumberingReferenceLevelDestination }

constructor TdxParagraphNumberingReferenceLevelDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AParentDestination: TdxParagraphNumberingReferenceDestination);
begin
  inherited Create(AImporter);
  Assert(AParentDestination <> nil);
  FParentDestination := AParentDestination;
end;

procedure TdxParagraphNumberingReferenceLevelDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FParentDestination.ListLevelIndex := Max(0, Min(8, Importer.GetWpSTIntegerValue(AReader, 'val', -1)));
end;

{ TdxParagraphNumberingReferenceNumberingIdDestination }

constructor TdxParagraphNumberingReferenceNumberingIdDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AParentDestination: TdxParagraphNumberingReferenceDestination);
begin
  inherited Create(AImporter);
  Assert(AParentDestination <> nil);
  FParentDestination := AParentDestination;
end;

procedure TdxParagraphNumberingReferenceNumberingIdDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ANumberingId: Integer;
begin
  ANumberingId := Importer.GetWpSTIntegerValue(AReader, 'val', MinInt);
  if ANumberingId = 0 then
    ANumberingId := NumberingListIndexNoNumberingList;
  FParentDestination.NumberingId := ANumberingId;
end;

{ TdxParagraphFormattingLeafElementDestination }

constructor TdxParagraphFormattingLeafElementDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  const AParagraphProperties: IdxParagraphProperties);
begin
  inherited Create(AImporter);
  Assert(AParagraphProperties <> nil);
  FParagraphProperties := AParagraphProperties;
end;

{ TdxParagraphSpacingDestination }

procedure TdxParagraphSpacingDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ASpacingAfter, ASpacingBefore, ALineSpacing: Integer;
  AAttribute: TdxWordProcessingMLValue;
  ALineSpacingRule: string;
begin
  ASpacingAfter := Importer.GetWpSTIntegerValue(AReader, 'after', MinInt);
  if ASpacingAfter <> MinInt then
    ParagraphProperties.SpacingAfter := UnitConverter.TwipsToModelUnits(ASpacingAfter);
  ASpacingBefore := Importer.GetWpSTIntegerValue(AReader, 'before', MinInt);
  if ASpacingBefore <> MinInt then
    ParagraphProperties.SpacingBefore := UnitConverter.TwipsToModelUnits(ASpacingBefore);

  ParagraphProperties.BeforeAutoSpacing := Importer.GetWpSTOnOffValue(AReader,
    Importer.GetWordProcessingMLValue(TdxWordProcessingMLValue.Create('beforeAutospacing', 'before-autospacing')), False);
  ParagraphProperties.AfterAutoSpacing := Importer.GetWpSTOnOffValue(AReader,
    Importer.GetWordProcessingMLValue(TdxWordProcessingMLValue.Create('afterAutospacing', 'after-autospacing')), False);

  ALineSpacing := Importer.GetWpSTIntegerValue(AReader, 'line', MinInt);
  if (ALineSpacing <> MinInt) and (ALineSpacing > 0) then
  begin
    AAttribute := TdxWordProcessingMLValue.Create('lineRule', 'line-rule');
    ALineSpacingRule := AReader.GetAttribute(Importer.GetWordProcessingMLValue(AAttribute), Importer.WordProcessingNamespaceConst);
    ApplyLineSpacingValue(ALineSpacing, ALineSpacingRule);
  end
  else
    if ALineSpacing <> MinInt then
      ParagraphProperties.LineSpacingType := TdxParagraphLineSpacing.Single;
end;

procedure TdxParagraphSpacingDestination.ApplyLineSpacingValue(ALineSpacing: Integer; const ALineSpacingRule: string);
begin
  if (ALineSpacingRule = 'at-least') or (ALineSpacingRule = 'atLeast') then
  begin
    ParagraphProperties.LineSpacingType := TdxParagraphLineSpacing.AtLeast;
    ParagraphProperties.LineSpacing := UnitConverter.TwipsToModelUnits(ALineSpacing);
  end
  else
    if ALineSpacingRule = 'exact' then
    begin
      ParagraphProperties.LineSpacingType := TdxParagraphLineSpacing.Exactly;
      ParagraphProperties.LineSpacing := UnitConverter.TwipsToModelUnits(ALineSpacing);
    end
    else
    begin
      if ALineSpacing = 240 then
        ParagraphProperties.LineSpacingType := TdxParagraphLineSpacing.Single
      else
        if ALineSpacing = 360 then
          ParagraphProperties.LineSpacingType := TdxParagraphLineSpacing.Sesquialteral
        else
          if ALineSpacing = 480 then
            ParagraphProperties.LineSpacingType := TdxParagraphLineSpacing.Double
          else
          begin
            ParagraphProperties.LineSpacingType := TdxParagraphLineSpacing.Multiple;
            ParagraphProperties.LineSpacing := ALineSpacing / 240.0;
          end;
    end;
end;

{ TdxParagraphIndentsDestination }

procedure TdxParagraphIndentsDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ALeft, ARight, AFirstLine, AHanging: Integer;
  AFirstLineAttributeName: TdxWordProcessingMLValue;
begin
  ALeft := Importer.GetWpSTIntegerValue(AReader, 'left', MinInt);
  if ALeft <> MinInt then
    ParagraphProperties.LeftIndent := UnitConverter.TwipsToModelUnits(ALeft);

  ARight := Importer.GetWpSTIntegerValue(AReader, 'right', MinInt);
  if ARight <> MinInt then
    ParagraphProperties.RightIndent := UnitConverter.TwipsToModelUnits(ARight);

  AFirstLineAttributeName := TdxWordProcessingMLValue.Create('firstLine', 'first-line');
  AFirstLine := Importer.GetWpSTIntegerValue(AReader, Importer.GetWordProcessingMLValue(AFirstLineAttributeName), MinInt);
  if AFirstLine <> MinInt then
  begin
    if AFirstLine > 0 then
      ParagraphProperties.FirstLineIndentType := TdxParagraphFirstLineIndent.Indented
    else
      if AFirstLine < 0 then
        ParagraphProperties.FirstLineIndentType := TdxParagraphFirstLineIndent.Hanging
      else
        ParagraphProperties.FirstLineIndentType := TdxParagraphFirstLineIndent.None;

    ParagraphProperties.FirstLineIndent := UnitConverter.TwipsToModelUnits(Abs(AFirstLine));
  end;

  AHanging := Importer.GetWpSTIntegerValue(AReader, 'hanging', MinInt);
  if AHanging <> MinInt then
  begin
    ParagraphProperties.FirstLineIndentType := TdxParagraphFirstLineIndent.Hanging;
    ParagraphProperties.FirstLineIndent := UnitConverter.TwipsToModelUnits(AHanging);
  end;
end;

{ TdxParagraphAlignmentDestination }

procedure TdxParagraphAlignmentDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: string;
begin
  AValue := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);
  if AValue <> '' then
    ParagraphProperties.Alignment := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValueCore<TdxParagraphAlignment>(AValue,
      TdxOpenXmlExporter.ParagraphAlignmentTable, TdxParagraphAlignment.Left);
end;

{ TdxSuppressHyphenationDestination }

procedure TdxSuppressHyphenationDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  ParagraphProperties.SuppressHyphenation := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxSuppressLineNumbersDestination }

procedure TdxSuppressLineNumbersDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  ParagraphProperties.SuppressLineNumbers := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxContextualSpacingDestination }

procedure TdxContextualSpacingDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  ParagraphProperties.ContextualSpacing := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxPageBreakBeforeDestination }

procedure TdxPageBreakBeforeDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  ParagraphProperties.PageBreakBefore := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxKeepWithNextDestination }

procedure TdxKeepWithNextDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  ParagraphProperties.KeepWithNext := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxKeepLinesTogetherDestination }

procedure TdxKeepLinesTogetherDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  ParagraphProperties.KeepLinesTogether := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxWidowOrphanControlDestination }

procedure TdxWidowOrphanControlDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  ParagraphProperties.WidowOrphanControl := Importer.GetWpSTOnOffValue(AReader, 'val');
end;

{ TdxOutlineLevelDestination }

procedure TdxOutlineLevelDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ALevel: Integer;
begin
  ALevel := Importer.GetWpSTIntegerValue(AReader, 'val', 9);
  if (ALevel < 0) or (ALevel >= 9) then
    ALevel := 0
  else
    Inc(ALevel);
  ParagraphProperties.OutlineLevel := ALevel;
end;

{ TdxParagraphBackgroundDestination }

procedure TdxParagraphBackgroundDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  APattern: TdxShadingPattern;
  AFill, APatternColor, AActualColor: TdxAlphaColor;
  AImporter: TdxWordProcessingMLBaseImporter;
begin
  AImporter := Importer as TdxWordProcessingMLBaseImporter;
  APattern := AImporter.GetWpEnumValue<TdxShadingPattern>(AReader, 'val',
    TdxOpenXmlExporter.ShadingPatternTable, TdxShadingPattern.Clear);
  AFill := AImporter.GetWpSTColorValue(AReader, 'fill', TdxAlphaColors.Transparent);
  APatternColor := AImporter.GetWpSTColorValue(AReader, 'color', TdxAlphaColors.Transparent);
  AActualColor := TdxShadingHelper.GetActualBackColor(AFill, APatternColor, APattern);
  if AActualColor <> TdxAlphaColors.Empty then
    ParagraphProperties.BackColor := AActualColor;
end;

{ TdxFramePropertiesDestination }

constructor TdxFramePropertiesDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
begin
  inherited Create(AImporter);
  AImporter.Position.ParagraphFrameFormatting.ReplaceInfo(
    AImporter.DocumentModel.Cache.ParagraphFrameFormattingInfoCache.DefaultItem,
    TdxParagraphFrameFormattingOptions.Create);
  FFrameProperties := AImporter.Position.ParagraphFrameFormatting;
end;

procedure TdxFramePropertiesDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AWidth, AHeight, AVerticalPadding, AHorizontalPadding, X, Y: Integer;
  AHorizontalRule, AWrapType, AVerticalPositionType, AHorizontalPositionType, AHorizontalPositionAlignment, AVerticalPositionAlignment: string;
begin
  AWidth := Importer.GetWpSTIntegerValue(AReader, 'w', MinInt);
  if AWidth <> MinInt then
    FrameProperties.Width := AWidth;
  AHeight := Importer.GetWpSTIntegerValue(AReader, 'h', MinInt);
  if AHeight <> MinInt then
  begin
    FrameProperties.Height := AHeight;
    FrameProperties.HorizontalRule := TdxParagraphFrameHorizontalRule.AtLeast;
  end;

  AHorizontalRule := AReader.GetAttribute('hRule', Importer.WordProcessingNamespaceConst);
  ApplyHorizontalRuleValue(AHorizontalRule);
  AVerticalPadding := Importer.GetWpSTIntegerValue(AReader, 'vSpace', MinInt);
  if AVerticalPadding <> MinInt then
    FrameProperties.VerticalPadding := AVerticalPadding;
  AHorizontalPadding := Importer.GetWpSTIntegerValue(AReader, 'hSpace', MinInt);
  if AHorizontalPadding <> MinInt then
    FrameProperties.HorizontalPadding := AHorizontalPadding;
  AWrapType := AReader.GetAttribute('wrap', Importer.WordProcessingNamespaceConst);
  ApplyWrapTypeValue(AWrapType);
  AVerticalPositionType := AReader.GetAttribute('vAnchor', Importer.WordProcessingNamespaceConst);
  ApplyVerticalPositionTypeValue(AVerticalPositionType);
  AHorizontalPositionType := AReader.GetAttribute('hAnchor', Importer.WordProcessingNamespaceConst);
  ApplyHorizontalPositionTypeValue(AHorizontalPositionType);
  X := Importer.GetWpSTIntegerValue(AReader, 'x', MinInt);
  if X <> MinInt then
    FrameProperties.X := X;
  AHorizontalPositionAlignment := AReader.GetAttribute('xAlign', Importer.WordProcessingNamespaceConst);
  ApplyHorizontalPositionAlignmentValue(AHorizontalPositionAlignment);
  Y := Importer.GetWpSTIntegerValue(AReader, 'y', MinInt);
  if Y <> MinInt then
    FrameProperties.Y := Y;
  AVerticalPositionAlignment := AReader.GetAttribute('yAlign', Importer.WordProcessingNamespaceConst);
  ApplyVerticalPositionAlignmentValue(AVerticalPositionAlignment);
end;

procedure TdxFramePropertiesDestination.ApplyHorizontalRuleValue(const AHorizontalRule: string);
begin
  if AHorizontalRule = 'auto' then
    FrameProperties.HorizontalRule := TdxParagraphFrameHorizontalRule.Auto
  else
    if AHorizontalRule = 'atLeast' then
      FrameProperties.HorizontalRule := TdxParagraphFrameHorizontalRule.AtLeast
    else
      if AHorizontalRule = 'exact' then
        FrameProperties.HorizontalRule := TdxParagraphFrameHorizontalRule.Exact;
end;

procedure TdxFramePropertiesDestination.ApplyWrapTypeValue(const AWrapType: string);
begin
  if AWrapType = 'auto' then
    FrameProperties.TextWrapType := TdxParagraphFrameTextWrapType.Auto
  else
    if AWrapType = 'around' then
      FrameProperties.TextWrapType := TdxParagraphFrameTextWrapType.Around
    else
      if AWrapType = 'none' then
        FrameProperties.TextWrapType := TdxParagraphFrameTextWrapType.None
      else
        if AWrapType = 'notBeside' then
          FrameProperties.TextWrapType := TdxParagraphFrameTextWrapType.NotBeside
        else
          if AWrapType = 'through' then
            FrameProperties.TextWrapType := TdxParagraphFrameTextWrapType.Through
          else
            if AWrapType = 'tight' then
              FrameProperties.TextWrapType := TdxParagraphFrameTextWrapType.Tight;
end;

procedure TdxFramePropertiesDestination.ApplyVerticalPositionTypeValue(const AVerticalPositionType: string);
begin
  if AVerticalPositionType = 'margin' then
    FrameProperties.VerticalPositionType := TdxParagraphFrameVerticalPositionType.Margin
  else
    if AVerticalPositionType = 'page' then
      FrameProperties.VerticalPositionType := TdxParagraphFrameVerticalPositionType.Page
    else
      if AVerticalPositionType = 'text' then
        FrameProperties.VerticalPositionType := TdxParagraphFrameVerticalPositionType.Paragraph;
end;

procedure TdxFramePropertiesDestination.ApplyHorizontalPositionTypeValue(const AHorizontalPositionType: string);
begin
  if AHorizontalPositionType = 'margin' then
    FrameProperties.HorizontalPositionType := TdxParagraphFrameHorizontalPositionType.Margin
  else
    if AHorizontalPositionType = 'page' then
      FrameProperties.HorizontalPositionType := TdxParagraphFrameHorizontalPositionType.Page
    else
      if AHorizontalPositionType = 'text' then
        FrameProperties.HorizontalPositionType := TdxParagraphFrameHorizontalPositionType.Column;
end;

procedure TdxFramePropertiesDestination.ApplyHorizontalPositionAlignmentValue(const AHorizontalPositionAlignment: string);
begin
  if AHorizontalPositionAlignment = 'center' then
    FrameProperties.HorizontalPositionAlignment := TdxParagraphFrameHorizontalPositionAlignment.Center
  else
    if AHorizontalPositionAlignment ='inside' then
      FrameProperties.HorizontalPositionAlignment := TdxParagraphFrameHorizontalPositionAlignment.Inside
    else
      if AHorizontalPositionAlignment ='left' then
        FrameProperties.HorizontalPositionAlignment := TdxParagraphFrameHorizontalPositionAlignment.Left
      else
        if AHorizontalPositionAlignment ='outside' then
          FrameProperties.HorizontalPositionAlignment := TdxParagraphFrameHorizontalPositionAlignment.Outside
        else
          if AHorizontalPositionAlignment ='right' then
            FrameProperties.HorizontalPositionAlignment := TdxParagraphFrameHorizontalPositionAlignment.Right;
end;

procedure TdxFramePropertiesDestination.ApplyVerticalPositionAlignmentValue(const AVerticalPositionAlignment: string);
begin
  if AVerticalPositionAlignment = 'bottom' then
    FrameProperties.VerticalPositionAlignment := TdxParagraphFrameVerticalPositionAlignment.Bottom
  else
    if AVerticalPositionAlignment = 'center' then
      FrameProperties.VerticalPositionAlignment := TdxParagraphFrameVerticalPositionAlignment.Center
    else
      if AVerticalPositionAlignment = 'inline' then
        FrameProperties.VerticalPositionAlignment := TdxParagraphFrameVerticalPositionAlignment.Inline
      else
        if AVerticalPositionAlignment = 'inside' then
          FrameProperties.VerticalPositionAlignment := TdxParagraphFrameVerticalPositionAlignment.Inside
        else
          if AVerticalPositionAlignment = 'outside' then
            FrameProperties.VerticalPositionAlignment := TdxParagraphFrameVerticalPositionAlignment.Outside
          else
            if AVerticalPositionAlignment = 'top' then
              FrameProperties.VerticalPositionAlignment := TdxParagraphFrameVerticalPositionAlignment.Top;
end;

function TdxFramePropertiesDestination.GetFrameProperties: TdxParagraphFrameFormattingBase;
begin
  if not Importer.Position.IsContainsParagraphFrame then
    Importer.Position.IsContainsParagraphFrame := True;
  Result := FFrameProperties;
end;

{ TdxParagraphBordersDestination }

constructor TdxParagraphBordersDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  const AParagraphProperties: IdxParagraphProperties);
begin
  inherited Create(AImporter, AParagraphProperties);
  AParagraphProperties.TopBorder    := TdxBorderInfo.Empty;
  AParagraphProperties.LeftBorder   := TdxBorderInfo.Empty;
  AParagraphProperties.BottomBorder := TdxBorderInfo.Empty;
  AParagraphProperties.RightBorder  := TdxBorderInfo.Empty;
end;

class constructor TdxParagraphBordersDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxParagraphBordersDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxParagraphBordersDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('top', OnTopBorder);
  Result.Add('left', OnLeftBorder);
  Result.Add('bottom', OnBottomBorder);
  Result.Add('right', OnRightBorder);
end;

function TdxParagraphBordersDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxParagraphBordersDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxParagraphBordersDestination;
begin
  Result := TdxParagraphBordersDestination(AImporter.PeekDestination);
end;

class function TdxParagraphBordersDestination.GetParagraphProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter): IdxParagraphProperties;
begin
  Result := GetThis(AImporter).ParagraphProperties;
end;

class function TdxParagraphBordersDestination.OnTopBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
var
  AParagraphProperties: IdxParagraphProperties;
begin
  AParagraphProperties := GetParagraphProperties(AImporter);
  Result := TdxParagraphBorderDestination.Create(AImporter, AParagraphProperties.TopBorder);
end;

class function TdxParagraphBordersDestination.OnLeftBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
var
  AParagraphProperties: IdxParagraphProperties;
begin
  AParagraphProperties := GetParagraphProperties(AImporter);
  Result := TdxParagraphBorderDestination.Create(AImporter, AParagraphProperties.LeftBorder);
end;

class function TdxParagraphBordersDestination.OnBottomBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
var
  AParagraphProperties: IdxParagraphProperties;
begin
  AParagraphProperties := GetParagraphProperties(AImporter);
  Result := TdxParagraphBorderDestination.Create(AImporter, AParagraphProperties.BottomBorder);
end;

class function TdxParagraphBordersDestination.OnRightBorder(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
var
  AParagraphProperties: IdxParagraphProperties;
begin
  AParagraphProperties := GetParagraphProperties(AImporter);
  Result := TdxParagraphBorderDestination.Create(AImporter, AParagraphProperties.RightBorder);
end;

{ TdxParagraphBorderDestination }

constructor TdxParagraphBorderDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ABorder: TdxBorderInfo);
begin
  inherited Create(AImporter);
  Assert(ABorder <> nil);
  FBorder := ABorder;
end;

procedure TdxParagraphBorderDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ABorderLineStyle: TdxBorderLineStyle;
  AColor: TdxAlphaColor;
  AFrame, AShadow, AIsDefaultValue: Boolean;
  AValue: Integer;
begin
  ABorderLineStyle := Importer.GetWpEnumValue<TdxBorderLineStyle>(AReader, 'val', BorderStyleTable, TdxBorderLineStyle.None);
  AColor := TdxWordProcessingMLBaseImporter(Importer).GetWpSTColorValue(AReader, 'color');
  AFrame := Importer.GetWpSTOnOffValue(AReader, 'frame', False);
  AShadow := Importer.GetWpSTOnOffValue(AReader, 'shadow', False);

  AIsDefaultValue := (ABorderLineStyle = TdxBorderLineStyle.None) and (AColor = TdxAlphaColors.Empty) and not AFrame and not AShadow;
  if not AIsDefaultValue then
  begin
    FBorder.Style := ABorderLineStyle;
    FBorder.Color := AColor;
    FBorder.Frame := AFrame;
    FBorder.Shadow := AShadow;
  end;

  AValue := Importer.GetWpSTIntegerValue(AReader, 'space');
  if AValue <> MinInt then
    FBorder.Offset := UnitConverter.PointsToModelUnits(AValue);

  AValue := Importer.GetWpSTIntegerValue(AReader, 'sz');
  if AValue <> MinInt then
    FBorder.Width := Trunc(UnitConverter.PointsToModelUnitsF(AValue * 0.125));
end;

{ TdxTabsDestination }

constructor TdxTabsDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ATabs: TdxTabFormattingInfo);
begin
  inherited Create(AImporter);
  Assert(ATabs <> nil);
  FTabs := ATabs;
end;

class constructor TdxTabsDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTabsDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTabsDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('tab', OnTab);
end;

function TdxTabsDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxTabsDestination.GetTabs(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTabFormattingInfo;
var
  AThisObject: TdxTabsDestination;
begin
  AThisObject := TdxTabsDestination(AImporter.PeekDestination);
  Result := AThisObject.FTabs;
end;

class function TdxTabsDestination.OnTab(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTabDestination.Create(AImporter, GetTabs(AImporter));
end;

{ TdxTabsLeafElementDestination }

constructor TdxTabsLeafElementDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ATabs: TdxTabFormattingInfo);
begin
  inherited Create(AImporter);
  Assert(ATabs <> nil);
  FTabs := ATabs;
end;

{ TdxTabDestination }

procedure TdxTabDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  APos: Integer;
  ALeader: TdxTabLeaderType;
  AValue: string;
  AAlign: TdxTabAlignmentType;
begin
  APos := Importer.GetWpSTIntegerValue(AReader, 'pos', MinInt);
  if APos = MinInt then
    Exit;

  ALeader := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValue<TdxTabLeaderType>(AReader, 'leader',
    TdxOpenXmlExporter.tabLeaderTable, TdxTabLeaderType.None);

  AValue := AReader.GetAttribute('val', Importer.WordProcessingNamespaceConst);
  if AValue = '' then
    AAlign := TdxTabAlignmentType.Left
  else
    AAlign := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValueCore<TdxTabAlignmentType>(AValue,
      TdxOpenXmlExporter.tabAlignmentTable, TdxTabAlignmentType.Left);

  Tabs.Add(TdxTabInfo.Create(UnitConverter.TwipsToModelUnits(APos), AAlign, ALeader, AValue = 'clear', False));
end;

end.

