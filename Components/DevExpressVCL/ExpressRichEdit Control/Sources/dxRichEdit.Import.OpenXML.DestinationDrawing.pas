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

unit dxRichEdit.Import.OpenXML.DestinationDrawing;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxGenerics,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.ShapeFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.Types,
  dxXMLReader,
  dxRichEdit.Import.OpenXML,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.DestinationBody,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter,
  dxRichEdit.Import.FloatingObject;

type

  { TdxDrawingDestination }

  TdxDrawingDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FFloatingObjectImportInfo: TdxFloatingObjectImportInfo;
    function GetImage: TdxOfficeImageReference;
    procedure SetImage(const AValue: TdxOfficeImageReference);
    function GetTextBoxContent: TdxTextBoxContentType;
    procedure SetTextBoxContent(const AValue: TdxTextBoxContentType);
    function GetTextBoxProperties: TdxTextBoxProperties;
    function GetWidth: Integer;
    procedure SetWidth(const AValue: Integer);
    function GetHeight: Integer;
    procedure SetHeight(const AValue: Integer);
    function GetFloatingObject: TdxFloatingObjectProperties;
    function GetShape: TdxShape;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
    destructor Destroy; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingDestination; static;
    class function OnInline(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnAnchor(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    function ShouldInsertPicture: Boolean;

    property Image: TdxOfficeImageReference read GetImage write SetImage;
    property TextBoxContent: TdxTextBoxContentType read GetTextBoxContent write SetTextBoxContent;
    property TextBoxProperties: TdxTextBoxProperties read GetTextBoxProperties;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property FloatingObject: TdxFloatingObjectProperties read GetFloatingObject;
    property Shape: TdxShape read GetShape;
    property FloatingObjectImportInfo: TdxFloatingObjectImportInfo read FFloatingObjectImportInfo;
  end;

  { TdxDrawingInlineDestination }

  TdxDrawingInlineDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FloatingObjectImportInfo: TdxFloatingObjectImportInfo;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingInlineDestination; static;
    class function OnExtent(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnGraphic(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
  end;

  { TdxDrawingAnchorDestination }

  TdxDrawingAnchorDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FFloatingObjectImportInfo: TdxFloatingObjectImportInfo;
    FUseSimplePosition: Boolean;
    function GetFloatingObject: TdxFloatingObjectProperties;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
    function IsChoiceNamespaceSupported(const ARequeriesNamespaceUri: string): Boolean; override;
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    function ConvertEmuToDocumentUnits(AValue: Integer): Integer;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingAnchorDestination; static;
    class function OnExtent(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnGraphic(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnSimplePosition(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnAnchorHorizontalRelativeSize(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnAnchorVerticalRelativeSize(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnHorizontalPosition(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnVerticalPosition(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnWrapNone(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnWrapSquare(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnWrapThrough(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnWrapTight(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnWrapTopAndBottom(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnCNvGraphicFramePr(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnAnchorDocumentProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;

    property UseSimplePosition: Boolean read FUseSimplePosition;
    property FloatingObject: TdxFloatingObjectProperties read GetFloatingObject;
    property FloatingObjectImportInfo: TdxFloatingObjectImportInfo read FFloatingObjectImportInfo;
  end;

  { TdxDrawingAnchorGraphicFramePropertyDestination }

  TdxDrawingAnchorGraphicFramePropertyDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FFloatingObject: TdxFloatingObjectProperties;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingAnchorGraphicFramePropertyDestination; static;
    class function OnGraphicFrameLocks(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AAnchorDestination: TdxDrawingAnchorDestination);

    property FloatingObject: TdxFloatingObjectProperties read FFloatingObject;
  end;

  { TdxDrawingGraphicFrameLocksDestination }

  TdxDrawingGraphicFrameLocksDestination = class(TdxLeafElementDestination)
  strict private
    FFloatingObject: TdxFloatingObjectProperties;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AAnchorDestination: TdxDrawingAnchorGraphicFramePropertyDestination);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;

    property FloatingObject: TdxFloatingObjectProperties read FFloatingObject;
  end;

  { TdxDrawingInlineExtentDestination }

  TdxDrawingInlineExtentDestination = class(TdxLeafElementDestination)
  strict private
    FloatingObjectImportInfo: TdxFloatingObjectImportInfo;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxDrawingAnchorDocumentPropertiesDestination }

  TdxDrawingAnchorDocumentPropertiesDestination = class(TdxLeafElementDestination)
  strict private
    FFloatingObjectImportInfo: TdxFloatingObjectImportInfo;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AAnchorDestination: TdxDrawingAnchorDestination);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxDrawingInlineGraphicDestination }

  TdxDrawingInlineGraphicDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FloatingObjectImportInfo: TdxFloatingObjectImportInfo;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingInlineGraphicDestination; static;
    class function OnGraphicData(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;

  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
  end;

  { TdxDrawingInlineGraphicDataDestination }

  TdxDrawingInlineGraphicDataDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FloatingObjectImportInfo: TdxFloatingObjectImportInfo;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingInlineGraphicDataDestination; static;
    class function OnPicture(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnWordProcessingShape(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
  end;

  { TdxDrawingInlineGraphicDataPictureDestination }

  TdxDrawingInlineGraphicDataPictureDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FloatingObjectImportInfo: TdxFloatingObjectImportInfo;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingInlineGraphicDataPictureDestination; static;
    class function OnBlipFill(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnShapeProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnAlternateContent(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnChoice(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnFallback(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
  end;

  { TdxPictureBlipFillDestination }

  TdxPictureBlipFillDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FloatingObjectImportInfo: TdxFloatingObjectImportInfo;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxPictureBlipFillDestination; static;
    class function OnBlip(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
  end;

  { TdxPictureBlipDestination }

  TdxPictureBlipDestination = class(TdxLeafElementDestination)
  strict private
    FloatingObjectImportInfo: TdxFloatingObjectImportInfo;
    function GetImporter: TdxOpenXmlImporter; reintroduce; inline;
  public
    constructor Create(AImporter: TdxOpenXmlImporter; AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;

    property Importer: TdxOpenXmlImporter read GetImporter;
  end;

  { TdxDrawingAnchorSimplePositionDestination }

  TdxDrawingAnchorSimplePositionDestination = class(TdxLeafElementDestination)
  strict private
    FAnchorDestination: TdxDrawingAnchorDestination;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AAnchorDestination: TdxDrawingAnchorDestination);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxDrawingAnchorRelativeSizeBaseDestination }

  TdxDrawingAnchorRelativeSizeBaseDestination = class abstract(TdxElementDestination)
  strict private
    FAnchorDestination: TdxDrawingAnchorDestination;
    FVal: Integer;
  protected
    procedure ProcessElementCloseCore(AReader: TdxXmlReader); virtual; abstract;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AAnchorDestination: TdxDrawingAnchorDestination);
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingAnchorRelativeSizeBaseDestination; static;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;

    property Value: Integer read FVal write FVal;
    property AnchorDestination: TdxDrawingAnchorDestination read FAnchorDestination;
  end;

  { TdxDrawingAnchorHorizontalRelativeSizeDestination }

  TdxDrawingAnchorHorizontalRelativeSizeDestination = class(TdxDrawingAnchorRelativeSizeBaseDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FRelativeFrom: TdxFloatingObjectRelativeFromHorizontal;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingAnchorHorizontalRelativeSizeDestination; static;
    procedure ProcessElementCloseCore(AReader: TdxXmlReader); override;
    class function OnPictureWidth(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;

    property RelativeFrom: TdxFloatingObjectRelativeFromHorizontal read FRelativeFrom write FRelativeFrom;
  end;

  { TdxDrawingAnchorVerticalRelativeSizeDestination }

  TdxDrawingAnchorVerticalRelativeSizeDestination = class(TdxDrawingAnchorRelativeSizeBaseDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FRelativeFrom: TdxFloatingObjectRelativeFromVertical;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingAnchorVerticalRelativeSizeDestination; static;
    procedure ProcessElementCloseCore(AReader: TdxXmlReader); override;
    class function OnPictureHeight(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;

    property RelativeFrom: TdxFloatingObjectRelativeFromVertical read FRelativeFrom write FRelativeFrom;
  end;

  { TdxDrawingAnchorRelativeSizeValueDestination }

  TdxDrawingAnchorRelativeSizeValueDestination = class(TdxLeafElementDestination)
  strict private
    FSizeDestination: TdxDrawingAnchorRelativeSizeBaseDestination;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; APositionDestination: TdxDrawingAnchorRelativeSizeBaseDestination);
    function ProcessText(AReader: TdxXmlReader): Boolean; override;
  end;

  { TdxDrawingAnchorPositionBaseDestination }

  TdxDrawingAnchorPositionBaseDestination = class abstract(TdxElementDestination)
  strict private
    FAnchorDestination: TdxDrawingAnchorDestination;
    FOffset: Integer;
    FPercentOffset: Integer;
  protected
    class function OnPositionOffset(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnPositionPercentOffset(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    procedure ProcessElementCloseCore(AReader: TdxXmlReader); virtual; abstract;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AAnchorDestination: TdxDrawingAnchorDestination);
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingAnchorPositionBaseDestination; static;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;

    property Offset: Integer read FOffset write FOffset;
    property PercentOffset: Integer read FPercentOffset write FPercentOffset;
    property AnchorDestination: TdxDrawingAnchorDestination read FAnchorDestination;
  end;

  { TdxDrawingAnchorHorizontalPositionDestination }

  TdxDrawingAnchorHorizontalPositionDestination = class(TdxDrawingAnchorPositionBaseDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FRelativeTo: TdxFloatingObjectHorizontalPositionType;
    FAlignment: TdxFloatingObjectHorizontalPositionAlignment;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    procedure ProcessElementCloseCore(AReader: TdxXmlReader); override;
    class function OnHorizontalAlignment(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;

  public
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingAnchorHorizontalPositionDestination; static;
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;

    property Alignment: TdxFloatingObjectHorizontalPositionAlignment read FAlignment write FAlignment;
  end;

  { TdxDrawingAnchorVerticalPositionDestination }

  TdxDrawingAnchorVerticalPositionDestination = class(TdxDrawingAnchorPositionBaseDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FRelativeTo: TdxFloatingObjectVerticalPositionType;
    FAlignment: TdxFloatingObjectVerticalPositionAlignment;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    procedure ProcessElementCloseCore(AReader: TdxXmlReader); override;
    class function OnVerticalAlignment(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingAnchorVerticalPositionDestination; static;
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;

    property Alignment: TdxFloatingObjectVerticalPositionAlignment read FAlignment write FAlignment;
  end;

  { TdxDrawingAnchorPositionOffsetDestination }

  TdxDrawingAnchorPositionOffsetDestination = class(TdxLeafElementDestination)
  strict private
    FPositionDestination: TdxDrawingAnchorPositionBaseDestination;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; APositionDestination: TdxDrawingAnchorPositionBaseDestination);
    function ProcessText(AReader: TdxXmlReader): Boolean; override;
  end;

  { TdxDrawingAnchorPositionPercentOffsetDestination }

  TdxDrawingAnchorPositionPercentOffsetDestination = class(TdxLeafElementDestination)
  strict private
    FPositionDestination: TdxDrawingAnchorPositionBaseDestination;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; APositionDestination: TdxDrawingAnchorPositionBaseDestination);
    function ProcessText(AReader: TdxXmlReader): Boolean; override;
  end;

  { TdxDrawingAnchorPositionHorizontalAlignmentDestination }

  TdxDrawingAnchorPositionHorizontalAlignmentDestination = class(TdxLeafElementDestination)
  strict private
    FPositionDestination: TdxDrawingAnchorHorizontalPositionDestination;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; APositionDestination: TdxDrawingAnchorHorizontalPositionDestination);
    function ProcessText(AReader: TdxXmlReader): Boolean; override;
  end;

  { TdxDrawingAnchorPositionVerticalAlignmentDestination }

  TdxDrawingAnchorPositionVerticalAlignmentDestination = class(TdxLeafElementDestination)
  strict private
    FPositionDestination: TdxDrawingAnchorVerticalPositionDestination;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; APositionDestination: TdxDrawingAnchorVerticalPositionDestination);
    function ProcessText(AReader: TdxXmlReader): Boolean; override;
  end;

  { TdxDrawingAnchorWrapNoneDestination }

  TdxDrawingAnchorWrapNoneDestination = class(TdxLeafElementDestination)
  strict private
    FAnchorDestination: TdxDrawingAnchorDestination;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AAnchorDestination: TdxDrawingAnchorDestination);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxDrawingAnchorWrapTopAndBottomDestination }

  TdxDrawingAnchorWrapTopAndBottomDestination = class(TdxLeafElementDestination)
  strict private
    FAnchorDestination: TdxDrawingAnchorDestination;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AAnchorDestination: TdxDrawingAnchorDestination);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxDrawingAnchorPolygonDestinationBase }

  TdxDrawingAnchorPolygonDestinationBase = class abstract(TdxLeafElementDestination)
  strict private
    FAnchorDestination: TdxDrawingAnchorDestination;
  protected
    property AnchorDestination: TdxDrawingAnchorDestination read FAnchorDestination;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AAnchorDestination: TdxDrawingAnchorDestination);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxDrawingAnchorWrapSquareDestination }

  TdxDrawingAnchorWrapSquareDestination = class(TdxDrawingAnchorPolygonDestinationBase)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxDrawingAnchorWrapThroughDestination }

  TdxDrawingAnchorWrapThroughDestination = class(TdxDrawingAnchorPolygonDestinationBase)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxDrawingAnchorWrapTightDestination }

  TdxDrawingAnchorWrapTightDestination = class(TdxDrawingAnchorPolygonDestinationBase)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxWordProcessingShapeDestination }

  TdxWordProcessingShapeDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FloatingObjectImportInfo: TdxFloatingObjectImportInfo;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxWordProcessingShapeDestination; static;
    class function OnTextBox(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnTextBoxProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnShapeProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
  end;

  { IdxColorAccessor }

  IdxColorAccessor = interface
    function GetColor: TdxAlphaColor;
    procedure SetColor(const AValue: TdxAlphaColor);
    property Color: TdxAlphaColor read GetColor write SetColor;
  end;

  { IdxSolidFillAccessor }

  IdxSolidFillAccessor = interface(IdxColorAccessor)
    function GetFillColor: TdxAlphaColor;
    procedure SetFillColor(const AValue: TdxAlphaColor);
    property FillColor: TdxAlphaColor read GetFillColor write SetFillColor;
  end;

  { IdxOutlineAccessor }

  IdxOutlineAccessor = interface(IdxSolidFillAccessor)
    function GetOutlineColor: TdxAlphaColor;
    function GetOutlineWidth: Integer;
    procedure SetOutlineColor(const AValue: TdxAlphaColor);
    procedure SetOutlineWidth(const AValue: Integer);
    property OutlineColor: TdxAlphaColor read GetOutlineColor write SetOutlineColor;
    property OutlineWidth: Integer read GetOutlineWidth write SetOutlineWidth;
  end;

  { TdxWordProcessingShapePropertiesDestination }

  TdxWordProcessingShapePropertiesDestination = class(TdxElementDestination)
  public type
    TdxShapeSolidFillAccessor = class(TInterfacedObject, IdxColorAccessor, IdxSolidFillAccessor)
    strict private
      FShape: TdxShape;
      function GetFillColor: TdxAlphaColor;
      procedure SetFillColor(const AValue: TdxAlphaColor);
      function GetColor: TdxAlphaColor;
      procedure SetColor(const AValue: TdxAlphaColor);
    protected
      property FillColor: TdxAlphaColor read GetFillColor write SetFillColor;
      property Color: TdxAlphaColor read GetColor write SetColor;
    public
      constructor Create(AShape: TdxShape);
    end;

    TdxShapeOutlineAccessor = class(TInterfacedObject, IdxOutlineAccessor)
    strict private
      FShape: TdxShape;
      function GetOutlineColor: TdxAlphaColor;
      procedure SetOutlineColor(const AValue: TdxAlphaColor);
      function GetOutlineWidth: Integer;
      procedure SetOutlineWidth(const AValue: Integer);
      function GetFillColor: TdxAlphaColor;
      procedure SetFillColor(const AValue: TdxAlphaColor);
      function GetColor: TdxAlphaColor;
      procedure SetColor(const AValue: TdxAlphaColor);
    public
      constructor Create(AShape: TdxShape);

      property OutlineColor: TdxAlphaColor read GetOutlineColor write SetOutlineColor;
      property OutlineWidth: Integer read GetOutlineWidth write SetOutlineWidth;
      property FillColor: TdxAlphaColor read GetFillColor write SetFillColor;
      property Color: TdxAlphaColor read GetColor write SetColor;
    end;

  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    Shape: TdxShape;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxWordProcessingShapePropertiesDestination; static;
    class function OnSolidFill(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnNoFill(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnOutline(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnGraphicFrame(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AShape: TdxShape);
  end;

  { TdxSolidFillDestination }

  TdxSolidFillDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FSolidFillAccessor: IdxSolidFillAccessor;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxSolidFillDestination; static;
    class function OnSRgbColor(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnPresetColor(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnSchemeColor(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const ASolidFillAccessor: IdxSolidFillAccessor);

    property SolidFillAccessor: IdxSolidFillAccessor read FSolidFillAccessor;
  end;

  { TdxOutlineDestination }

  TdxOutlineDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FOutlineAccessor: IdxOutlineAccessor;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const AOutlineAccessor: IdxOutlineAccessor);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxOutlineDestination; static;
    class function OnSolidFill(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnNoFill(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;

    property OutlineAccessor: IdxOutlineAccessor read FOutlineAccessor;
  end;

  { TdxSRgbColorDestination }

  TdxSRgbColorDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FColorAccessor: IdxColorAccessor;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxSRgbColorDestination; static;
    class function OnAlphaColor(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    property ColorAccessor: IdxColorAccessor read FColorAccessor;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const AColorAccessor: IdxColorAccessor);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxAlphaColorDestination }

  TdxAlphaColorDestination = class(TdxLeafElementDestination)
  strict private
    FColorAccessor: IdxColorAccessor;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const AColorAccessor: IdxColorAccessor);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxPresetColorDestination }

  TdxPresetColorDestination = class(TdxLeafElementDestination)
  strict private
    FColorAccessor: IdxColorAccessor;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const AColorAccessor: IdxColorAccessor);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxNoFillDestination }

  TdxNoFillDestination = class(TdxLeafElementDestination)
  strict private
    FColorAccessor: IdxColorAccessor;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const AColorAccessor: IdxColorAccessor);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxColorScheme }

  TdxColorScheme = class
  strict private
    FColors: TdxStringColorDictionary;
  public
    constructor Create;
    destructor Destroy; override;
    function GetColorByName(const AName: string): TdxAlphaColor;
  end;

  { TdxSchemeColorDestination }

  TdxSchemeColorDestination = class(TdxElementDestination)
  strict private
    class var
      FColorScheme: TdxColorScheme;
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FColorAccessor: IdxColorAccessor;
    FBaseColor: TdxAlphaColor;
    FAlphaOffset: TdxNullableValue<Single>;
    FAlphaModulation: TdxNullableValue<Single>;
    FAlpha: TdxNullableValue<Single>;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxSchemeColorDestination; static;
    class function OnAlphaOffset(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnAlpha(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnAlphaModulation(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const AColorAccessor: IdxColorAccessor);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    function CalculateActualAlpha(AValue: Integer): Integer;
    function CalculateColorFactor(const AValue, AModulation, AOffset: TdxNullableValue<Single>): Single;
    function Validate(AMin, AMax, AValue: Single): Single; overload;
  end;

  { TdxTextBoxDestination }

  TdxTextBoxDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FFloatingObjectImportInfo: TdxFloatingObjectImportInfo;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    property FloatingObjectImportInfo: TdxFloatingObjectImportInfo read FFloatingObjectImportInfo;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    function GetMargins(const AStrMargins: string): TArray<Integer>;
    function GetResizeShapeToFitText(const AStyle: string): Boolean;
    function GetValidMarginValue(AValue: Integer; ADefaultValue: Integer): Integer;
    function GetFloatValue(const ANumber: string): Integer;
    class function OnTextBoxContent(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxTextBoxPropertiesDestination }

  TdxTextBoxPropertiesDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FTextBoxProperties: TdxTextBoxProperties;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ATextBoxProperties: TdxTextBoxProperties);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTextBoxPropertiesDestination; static;
    class function OnDisableAutoFit(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnEnableAutoFit(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxTextBoxContentDestination }

  TdxTextBoxContentDestination = class(TdxBodyDestinationBase)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnTable(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnParagraph(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnAltChunk(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxGraphicFrameDestination }

  TdxGraphicFrameDestination = class(TdxLeafElementDestination)
  strict private
    Shape: TdxShape;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AShape: TdxShape);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

implementation

uses
  Math, cxGeometry, dxTypeHelpers,
  dxRichEdit.Platform.Font,
  dxRichEdit.Export.OpenXML,
  dxRichEdit.Export.OpenXML.WordProcessingMLBaseExporter,
  dxRichEdit.Import.OpenXML.DestinationTable,
  dxRichEdit.Import.OpenXML.DestinationWebSettings,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.Utils.DXUnit,
  dxStringHelper,
  dxRichEdit.Utils.NumberParser;

{ TdxDrawingDestination }

constructor TdxDrawingDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
begin
  inherited Create(AImporter);
  FFloatingObjectImportInfo := TdxFloatingObjectImportInfo.Create(AImporter.PieceTable);
end;

destructor TdxDrawingDestination.Destroy;
begin
  FFloatingObjectImportInfo.Free;
  inherited Destroy;
end;

class constructor TdxDrawingDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDrawingDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDrawingDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('inline', OnInline);
  Result.Add('anchor', OnAnchor);
end;

function TdxDrawingDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

function TdxDrawingDestination.GetImage: TdxOfficeImageReference;
begin
  Result := FFloatingObjectImportInfo.Image;
end;

procedure TdxDrawingDestination.SetImage(const AValue: TdxOfficeImageReference);
begin
  FFloatingObjectImportInfo.Image := AValue;
end;

function TdxDrawingDestination.GetTextBoxContent: TdxTextBoxContentType;
begin
  Result := FFloatingObjectImportInfo.TextBoxContent;
end;

procedure TdxDrawingDestination.SetTextBoxContent(const AValue: TdxTextBoxContentType);
begin
  FFloatingObjectImportInfo.TextBoxContent := AValue;
end;

function TdxDrawingDestination.GetTextBoxProperties: TdxTextBoxProperties;
begin
  Result := FFloatingObjectImportInfo.TextBoxProperties;
end;

function TdxDrawingDestination.GetWidth: Integer;
begin
  Result := FFloatingObjectImportInfo.Width;
end;

procedure TdxDrawingDestination.SetWidth(const AValue: Integer);
begin
  FFloatingObjectImportInfo.Width := AValue;
end;

function TdxDrawingDestination.GetHeight: Integer;
begin
  Result := FFloatingObjectImportInfo.Height;
end;

procedure TdxDrawingDestination.SetHeight(const AValue: Integer);
begin
  FFloatingObjectImportInfo.Height := AValue;
end;

function TdxDrawingDestination.GetFloatingObject: TdxFloatingObjectProperties;
begin
  Result := FFloatingObjectImportInfo.FloatingObjectProperties;
end;

function TdxDrawingDestination.GetShape: TdxShape;
begin
  Result := FFloatingObjectImportInfo.Shape;
end;

class function TdxDrawingDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingDestination;
begin
  Result := TdxDrawingDestination(AImporter.PeekDestination);
end;

class function TdxDrawingDestination.OnInline(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  GetThis(AImporter).floatingObjectImportInfo.IsFloatingObject := False;
  Result := TdxDrawingInlineDestination.Create(AImporter, GetThis(AImporter).floatingObjectImportInfo);
end;

class function TdxDrawingDestination.OnAnchor(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  GetThis(AImporter).floatingObjectImportInfo.IsFloatingObject := True;

  Result := TdxDrawingAnchorDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), GetThis(AImporter).floatingObjectImportInfo);
end;

procedure TdxDrawingDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  AOriginalSize, ADesiredSize, ASize: TSize;
  AScale: TdxSizeF;
  AWidth, AHeight: Integer;
  AFillColor: TdxAlphaColor;
begin
  if not FFloatingObjectImportInfo.IsFloatingObject then
  begin
    if Image = nil then
      Exit;
    if not ShouldInsertPicture then
    begin
      Importer.PieceTable.InsertTextCore(Importer.Position, ' ');
      Exit;
    end;
    AOriginalSize := Image.CalculateImageSizeInModelUnits(Importer.DocumentModel.UnitConverter);

    ADesiredSize := TSize.Create(FFloatingObjectImportInfo.Width, FFloatingObjectImportInfo.Height);
    AScale := TdxImageScaleCalculator.GetScale(ADesiredSize, AOriginalSize, 100.0);
    if FloatingObjectImportInfo.Shape.UseFillColor then
      AFillColor := FloatingObjectImportInfo.Shape.FillColor
    else
      AFillColor := TdxAlphaColors.Empty;
    PieceTable.AppendImage(Importer.Position, Image, AScale.Width, AScale.Height, AFillColor, False);
  end
  else
  begin
    if (Height <> MinInt) and (Width <> MinInt) then
    begin
      if Image <> nil then
      begin
        ASize := UnitConverter.TwipsToModelUnits(Image.SizeInTwips);
        AWidth := Width;
        if AWidth <= 0 then
          AWidth := Max(1, ASize.Width);
        AHeight := Height;
        if AHeight <= 0 then
          AHeight := Max(1, ASize.Height);
        FloatingObject.ActualSize := TSize.Create(AWidth, AHeight);
      end
      else
        FloatingObject.ActualSize := TSize.Create(Max(1, Width), Max(1, Height));
    end;

    FFloatingObjectImportInfo.InsertFloatingObject(Importer.Position);
  end;
end;

function TdxDrawingDestination.ShouldInsertPicture: Boolean;
begin
  Result := Importer.DocumentModel.DocumentCapabilities.InlinePicturesAllowed;
end;

{ TdxDrawingInlineDestination }

constructor TdxDrawingInlineDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
begin
  inherited Create(AImporter);
  Assert(AFloatingObjectImportInfo <> nil, 'FloatingObjectImportInfo');
  FloatingObjectImportInfo := AFloatingObjectImportInfo;
end;

class constructor TdxDrawingInlineDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDrawingInlineDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDrawingInlineDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('extent', OnExtent);
  Result.Add('graphic', OnGraphic);
end;

function TdxDrawingInlineDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxDrawingInlineDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingInlineDestination;
begin
  Result := TdxDrawingInlineDestination(AImporter.PeekDestination);
end;

class function TdxDrawingInlineDestination.OnExtent(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingInlineExtentDestination.Create(AImporter, GetThis(AImporter).FloatingObjectImportInfo);
end;

class function TdxDrawingInlineDestination.OnGraphic(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingInlineGraphicDestination.Create(AImporter, GetThis(AImporter).FloatingObjectImportInfo);
end;

{ TdxDrawingAnchorDestination }

constructor TdxDrawingAnchorDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
begin
  inherited Create(AImporter);
  Assert(AFloatingObjectImportInfo <> nil, 'floatingObjectImportInfo');
  FFloatingObjectImportInfo := AFloatingObjectImportInfo;
end;

class constructor TdxDrawingAnchorDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDrawingAnchorDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDrawingAnchorDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('extent', OnExtent);
  Result.Add('graphic', OnGraphic);
  Result.Add('simplePos', OnSimplePosition);
  Result.Add('positionH', OnHorizontalPosition);
  Result.Add('positionV', OnVerticalPosition);
  Result.Add('wrapNone', OnWrapNone);
  Result.Add('wrapSquare', OnWrapSquare);
  Result.Add('wrapThrough', OnWrapThrough);
  Result.Add('wrapTight', OnWrapTight);
  Result.Add('wrapTopAndBottom', OnWrapTopAndBottom);
  Result.Add('cNvGraphicFramePr', OnCNvGraphicFramePr);
  Result.Add('docPr', OnAnchorDocumentProperties);
  Result.Add('sizeRelH', OnAnchorHorizontalRelativeSize);
  Result.Add('sizeRelV', OnAnchorVerticalRelativeSize);
end;

function TdxDrawingAnchorDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

function TdxDrawingAnchorDestination.GetFloatingObject: TdxFloatingObjectProperties;
begin
  Result := FFloatingObjectImportInfo.FloatingObjectProperties;
end;

function TdxDrawingAnchorDestination.IsChoiceNamespaceSupported(const ARequeriesNamespaceUri: string): Boolean;
begin
  if CompareText(ARequeriesNamespaceUri, TdxOpenXmlExporter.Wp14Namespace) = 0 then
    Exit(True);
  Result := inherited IsChoiceNamespaceSupported(ARequeriesNamespaceUri);
end;

procedure TdxDrawingAnchorDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AObj: TdxFloatingObjectProperties;
  AValue: Integer;
begin
  AObj := FloatingObject;
  AValue := Importer.GetIntegerValue(AReader, 'distT', MinInt);
  if AValue <> MinInt then
    AObj.TopDistance := ConvertEmuToDocumentUnits(AValue);
  AValue := Importer.GetIntegerValue(AReader, 'distB', MinInt);
  if AValue <> MinInt then
    AObj.BottomDistance := ConvertEmuToDocumentUnits(AValue);
  AValue := Importer.GetIntegerValue(AReader, 'distL', MinInt);
  if AValue <> MinInt then
    AObj.LeftDistance := ConvertEmuToDocumentUnits(AValue);
  AValue := Importer.GetIntegerValue(AReader, 'distR', MinInt);
  if AValue <> MinInt then
    AObj.RightDistance := ConvertEmuToDocumentUnits(AValue);
  AValue := Importer.GetIntegerValue(AReader, 'relativeHeight', MinInt);
  if AValue <> MinInt then
    AObj.ZOrder := AValue;

  FUseSimplePosition := Importer.GetOnOffValue(AReader, 'simplePos', False);

  if Importer.GetOnOffValue(AReader, 'allowOverlap', False) then
    AObj.AllowOverlap := True;
  if Importer.GetOnOffValue(AReader, 'behindDoc', False) then
    AObj.IsBehindDoc := True;
  if Importer.GetOnOffValue(AReader, 'hidden', False) then
    AObj.Hidden := True;
  if Importer.GetOnOffValue(AReader, 'layoutInCell', False) then
    AObj.LayoutInTableCell := True;
  if Importer.GetOnOffValue(AReader, 'locked', False) then
    AObj.Locked := True;
end;

function TdxDrawingAnchorDestination.ConvertEmuToDocumentUnits(AValue: Integer): Integer;
begin
  Result := Round(UnitConverter.MillimetersToModelUnitsF(AValue / 36000.0));
end;

class function TdxDrawingAnchorDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingAnchorDestination;
begin
  Result := TdxDrawingAnchorDestination(AImporter.PeekDestination);
end;

class function TdxDrawingAnchorDestination.OnExtent(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingInlineExtentDestination.Create(AImporter, GetThis(AImporter).floatingObjectImportInfo);
end;

class function TdxDrawingAnchorDestination.OnGraphic(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingInlineGraphicDestination.Create(AImporter, GetThis(AImporter).floatingObjectImportInfo);
end;

class function TdxDrawingAnchorDestination.OnSimplePosition(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingAnchorSimplePositionDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxDrawingAnchorDestination.OnAnchorHorizontalRelativeSize(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingAnchorHorizontalRelativeSizeDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxDrawingAnchorDestination.OnAnchorVerticalRelativeSize(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingAnchorVerticalRelativeSizeDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxDrawingAnchorDestination.OnHorizontalPosition(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingAnchorHorizontalPositionDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxDrawingAnchorDestination.OnVerticalPosition(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingAnchorVerticalPositionDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxDrawingAnchorDestination.OnWrapNone(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingAnchorWrapNoneDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxDrawingAnchorDestination.OnWrapSquare(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingAnchorWrapSquareDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxDrawingAnchorDestination.OnWrapThrough(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingAnchorWrapThroughDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxDrawingAnchorDestination.OnWrapTight(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingAnchorWrapTightDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxDrawingAnchorDestination.OnWrapTopAndBottom(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingAnchorWrapTopAndBottomDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxDrawingAnchorDestination.OnCNvGraphicFramePr(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingAnchorGraphicFramePropertyDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxDrawingAnchorDestination.OnAnchorDocumentProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingAnchorDocumentPropertiesDestination.Create(AImporter, GetThis(AImporter));
end;

{ TdxDrawingAnchorGraphicFramePropertyDestination }

constructor TdxDrawingAnchorGraphicFramePropertyDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AAnchorDestination: TdxDrawingAnchorDestination);
begin
  inherited Create(AImporter);
  Assert(AAnchorDestination <> nil, 'cNvGraphicFramePr');

  FFloatingObject := AAnchorDestination.FloatingObject;
end;

class constructor TdxDrawingAnchorGraphicFramePropertyDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDrawingAnchorGraphicFramePropertyDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDrawingAnchorGraphicFramePropertyDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('graphicFrameLocks', OnGraphicFrameLocks);
end;

function TdxDrawingAnchorGraphicFramePropertyDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxDrawingAnchorGraphicFramePropertyDestination.GetThis(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingAnchorGraphicFramePropertyDestination;
begin
  Result := TdxDrawingAnchorGraphicFramePropertyDestination(AImporter.PeekDestination);
end;

class function TdxDrawingAnchorGraphicFramePropertyDestination.OnGraphicFrameLocks(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingGraphicFrameLocksDestination.Create(AImporter, GetThis(AImporter));
end;

{ TdxDrawingGraphicFrameLocksDestination }

constructor TdxDrawingGraphicFrameLocksDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AAnchorDestination: TdxDrawingAnchorGraphicFramePropertyDestination);
begin
  inherited Create(AImporter);
  Assert(AAnchorDestination <> nil, 'graphicFrameLocks');

  FFloatingObject := AAnchorDestination.FloatingObject;
end;

procedure TdxDrawingGraphicFrameLocksDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AObj: TdxFloatingObjectProperties;
  AValue: string;
begin
  AObj := FloatingObject;
  AValue := AReader.GetAttribute('noChangeAspect', '');
  if AValue <> '' then
    AObj.LockAspectRatio := Importer.ConvertToBool(AValue);
end;

{ TdxDrawingInlineExtentDestination }

constructor TdxDrawingInlineExtentDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
begin
  inherited Create(AImporter);
  Assert(AFloatingObjectImportInfo <> nil, 'FloatingObjectImportInfo');
  FloatingObjectImportInfo := AFloatingObjectImportInfo;
end;

procedure TdxDrawingInlineExtentDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ACx, ACy: string;
begin
  ACx := AReader.GetAttribute('cx', '');
  ACy := AReader.GetAttribute('cy', '');
  FloatingObjectImportInfo.Width := Round(UnitConverter.MillimetersToModelUnitsF(Importer.GetIntegerValue(ACx,
    TdxNumberStyles.Integer, 0) / 36000.0));
  FloatingObjectImportInfo.Height := Round(UnitConverter.MillimetersToModelUnitsF(Importer.GetIntegerValue(ACy,
    TdxNumberStyles.Integer, 0) / 36000.0));
end;

{ TdxDrawingAnchorDocumentPropertiesDestination }

constructor TdxDrawingAnchorDocumentPropertiesDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AAnchorDestination: TdxDrawingAnchorDestination);
begin
  inherited Create(AImporter);
  Assert(AAnchorDestination <> nil, 'anchorDestination');
  FFloatingObjectImportInfo := AAnchorDestination.FloatingObjectImportInfo;
end;

procedure TdxDrawingAnchorDocumentPropertiesDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AName: string;
begin
  AName := AReader.GetAttribute('name');
  if AName <> '' then
    FFloatingObjectImportInfo.Name := AName;
end;

{ TdxDrawingInlineGraphicDestination }

constructor TdxDrawingInlineGraphicDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
begin
  inherited Create(AImporter);
  Assert(AFloatingObjectImportInfo <> nil, 'FloatingObjectImportInfo');
  FloatingObjectImportInfo := AFloatingObjectImportInfo;
end;

class constructor TdxDrawingInlineGraphicDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDrawingInlineGraphicDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDrawingInlineGraphicDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('graphicData', OnGraphicData);
end;

function TdxDrawingInlineGraphicDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxDrawingInlineGraphicDestination.GetThis(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingInlineGraphicDestination;
begin
  Result := TdxDrawingInlineGraphicDestination(AImporter.PeekDestination);
end;

class function TdxDrawingInlineGraphicDestination.OnGraphicData(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingInlineGraphicDataDestination.Create(AImporter, GetThis(AImporter).FloatingObjectImportInfo);
end;

{ TdxDrawingInlineGraphicDataDestination }

constructor TdxDrawingInlineGraphicDataDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
begin
  inherited Create(AImporter);
  Assert(AFloatingObjectImportInfo <> nil, 'FloatingObjectImportInfo');
  FloatingObjectImportInfo := AFloatingObjectImportInfo;
end;

class constructor TdxDrawingInlineGraphicDataDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDrawingInlineGraphicDataDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDrawingInlineGraphicDataDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('pic', OnPicture);
  Result.Add('wsp', OnWordProcessingShape);
end;

function TdxDrawingInlineGraphicDataDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxDrawingInlineGraphicDataDestination.GetThis(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingInlineGraphicDataDestination;
begin
  Result := TdxDrawingInlineGraphicDataDestination(AImporter.PeekDestination);
end;

class function TdxDrawingInlineGraphicDataDestination.OnPicture(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingInlineGraphicDataPictureDestination.Create(AImporter, GetThis(AImporter).FloatingObjectImportInfo);
end;

class function TdxDrawingInlineGraphicDataDestination.OnWordProcessingShape(
 AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingShapeDestination.Create(AImporter, GetThis(AImporter).FloatingObjectImportInfo);
end;

{ TdxDrawingInlineGraphicDataPictureDestination }

constructor TdxDrawingInlineGraphicDataPictureDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
begin
  inherited Create(AImporter);
  Assert(AFloatingObjectImportInfo <> nil, 'FloatingObjectImportInfo');
  FloatingObjectImportInfo := AFloatingObjectImportInfo;
end;

class constructor TdxDrawingInlineGraphicDataPictureDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDrawingInlineGraphicDataPictureDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDrawingInlineGraphicDataPictureDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('blipFill', OnBlipFill);
  Result.Add('AlternateContent', OnAlternateContent);
  Result.Add('Choice', OnChoice);
  Result.Add('Fallback', OnFallback);
  Result.Add('spPr', OnShapeProperties);
end;

function TdxDrawingInlineGraphicDataPictureDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxDrawingInlineGraphicDataPictureDestination.GetThis(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingInlineGraphicDataPictureDestination;
begin
  Result := TdxDrawingInlineGraphicDataPictureDestination(AImporter.PeekDestination);
end;

class function TdxDrawingInlineGraphicDataPictureDestination.OnBlipFill(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxPictureBlipFillDestination.Create(AImporter, GetThis(AImporter).FloatingObjectImportInfo);
end;

class function TdxDrawingInlineGraphicDataPictureDestination.OnShapeProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingShapePropertiesDestination.Create(AImporter, GetThis(AImporter).FloatingObjectImportInfo.Shape);
end;

class function TdxDrawingInlineGraphicDataPictureDestination.OnAlternateContent(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := GetThis(AImporter);
end;

class function TdxDrawingInlineGraphicDataPictureDestination.OnChoice(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := GetThis(AImporter);
end;

class function TdxDrawingInlineGraphicDataPictureDestination.OnFallback(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := GetThis(AImporter);
end;

{ TdxPictureBlipFillDestination }

constructor TdxPictureBlipFillDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
begin
  inherited Create(AImporter);
  Assert(AFloatingObjectImportInfo <> nil, 'FloatingObjectImportInfo');
  FloatingObjectImportInfo := AFloatingObjectImportInfo;
end;

class constructor TdxPictureBlipFillDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxPictureBlipFillDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxPictureBlipFillDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('blip', OnBlip);
end;

function TdxPictureBlipFillDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxPictureBlipFillDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxPictureBlipFillDestination;
begin
  Result := TdxPictureBlipFillDestination(AImporter.PeekDestination);
end;

class function TdxPictureBlipFillDestination.OnBlip(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
var
  AOpenXmlImporter: TdxOpenXmlImporter;
begin
  AOpenXmlImporter := Safe<TdxOpenXmlImporter>.Cast(AImporter);
  if AOpenXmlImporter = nil then
    Exit(nil);
  Result := TdxPictureBlipDestination.Create(AOpenXmlImporter, GetThis(AOpenXmlImporter).FloatingObjectImportInfo);
end;

{ TdxPictureBlipDestination }

constructor TdxPictureBlipDestination.Create(AImporter: TdxOpenXmlImporter; AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
begin
  inherited Create(AImporter);
  Assert(AFloatingObjectImportInfo <> nil, 'FloatingObjectImportInfo');
  FloatingObjectImportInfo := AFloatingObjectImportInfo;
end;

function TdxPictureBlipDestination.GetImporter: TdxOpenXmlImporter;
begin
  Result := TdxOpenXmlImporter(inherited Importer);
end;

procedure TdxPictureBlipDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AImage: TdxOfficeImageReference;
  AId: string;
begin
  AId := AReader.GetAttribute('embed', TdxOpenXmlExporter.RelsNamespace);
  if AId <> '' then
  begin
    AImage := Importer.LookupImageByRelationId(AId, Importer.DocumentRootFolder);
    if AImage <> nil then
      FloatingObjectImportInfo.Image := AImage;
    Exit;
  end;

  AId := AReader.GetAttribute('link', TdxOpenXmlExporter.RelsNamespace);
  if AId = '' then
    Exit;
  AImage := Importer.LookupExternalImageByRelationId(AId, Importer.DocumentRootFolder);
  if AImage <> nil then
    FloatingObjectImportInfo.Image := AImage;
end;

{ TdxDrawingAnchorSimplePositionDestination }

constructor TdxDrawingAnchorSimplePositionDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AAnchorDestination: TdxDrawingAnchorDestination);
begin
  inherited Create(AImporter);
  Assert(AAnchorDestination <> nil, 'anchorDestination');
  FAnchorDestination := AAnchorDestination;
end;

procedure TdxDrawingAnchorSimplePositionDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  X, Y: Integer;
begin
  if not FAnchorDestination.UseSimplePosition then
    Exit;

  X := Importer.GetIntegerValue(AReader, 'x', MinInt);
  Y := Importer.GetIntegerValue(AReader, 'y', MinInt);
  if (X <> MinInt) and (Y <> MinInt) then
  begin
    X := FAnchorDestination.ConvertEmuToDocumentUnits(X);
    Y := FAnchorDestination.ConvertEmuToDocumentUnits(Y);
    FAnchorDestination.FloatingObject.Offset := TPoint.Create(X, Y);
    FAnchorDestination.FloatingObject.HorizontalPositionType := TdxFloatingObjectHorizontalPositionType.Page;
    FAnchorDestination.FloatingObject.VerticalPositionType := TdxFloatingObjectVerticalPositionType.Page;
    FAnchorDestination.FloatingObject.HorizontalPositionAlignment := TdxFloatingObjectHorizontalPositionAlignment.Left;
    FAnchorDestination.FloatingObject.VerticalPositionAlignment := TdxFloatingObjectVerticalPositionAlignment.Top;
  end;
end;

{ TdxDrawingAnchorRelativeSizeBaseDestination }

constructor TdxDrawingAnchorRelativeSizeBaseDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AAnchorDestination: TdxDrawingAnchorDestination);
begin
  inherited Create(AImporter);
  FVal := 100 * 1000;
  Assert(AAnchorDestination <> nil, 'anchorDestination');
  FAnchorDestination := AAnchorDestination;
end;

class function TdxDrawingAnchorRelativeSizeBaseDestination.GetThis(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingAnchorRelativeSizeBaseDestination;
begin
  Result := TdxDrawingAnchorRelativeSizeBaseDestination(AImporter.PeekDestination);
end;

procedure TdxDrawingAnchorRelativeSizeBaseDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  ProcessElementCloseCore(AReader);
end;

{ TdxDrawingAnchorHorizontalRelativeSizeDestination }

class constructor TdxDrawingAnchorHorizontalRelativeSizeDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDrawingAnchorHorizontalRelativeSizeDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDrawingAnchorHorizontalRelativeSizeDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('pctWidth', OnPictureWidth);
end;

function TdxDrawingAnchorHorizontalRelativeSizeDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxDrawingAnchorHorizontalRelativeSizeDestination.GetThis(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingAnchorHorizontalRelativeSizeDestination;
begin
  Result := TdxDrawingAnchorHorizontalRelativeSizeDestination(AImporter.PeekDestination);
end;

procedure TdxDrawingAnchorHorizontalRelativeSizeDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  RelativeFrom := TdxWordProcessingMLBaseImporter(Importer).GetEnumValue<TdxFloatingObjectRelativeFromHorizontal>(
    AReader, 'relativeFrom', TdxOpenXmlExporter.FloatingObjectRelativeFromHorizontalTable,
    TdxFloatingObjectRelativeFromHorizontal.Page);
end;

procedure TdxDrawingAnchorHorizontalRelativeSizeDestination.ProcessElementCloseCore(AReader: TdxXmlReader);
var
  AObj: TdxFloatingObjectProperties;
begin
  AObj := AnchorDestination.FloatingObject;
  AObj.RelativeWidth := TdxFloatingObjectRelativeWidth.Create(RelativeFrom, Value);
end;

class function TdxDrawingAnchorHorizontalRelativeSizeDestination.OnPictureWidth(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingAnchorRelativeSizeValueDestination.Create(AImporter, GetThis(AImporter));
end;

{ TdxDrawingAnchorVerticalRelativeSizeDestination }

class constructor TdxDrawingAnchorVerticalRelativeSizeDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDrawingAnchorVerticalRelativeSizeDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDrawingAnchorVerticalRelativeSizeDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('pctHeight', OnPictureHeight);
end;

function TdxDrawingAnchorVerticalRelativeSizeDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxDrawingAnchorVerticalRelativeSizeDestination.GetThis(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingAnchorVerticalRelativeSizeDestination;
begin
  Result := TdxDrawingAnchorVerticalRelativeSizeDestination(AImporter.PeekDestination);
end;

procedure TdxDrawingAnchorVerticalRelativeSizeDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  RelativeFrom := TdxWordProcessingMLBaseImporter(Importer).GetEnumValue<TdxFloatingObjectRelativeFromVertical>(AReader,
    'relativeFrom', TdxOpenXmlExporter.floatingObjectRelativeFromVerticalTable, TdxFloatingObjectRelativeFromVertical.Page);
end;

procedure TdxDrawingAnchorVerticalRelativeSizeDestination.ProcessElementCloseCore(AReader: TdxXmlReader);
var
  AObj: TdxFloatingObjectProperties;
begin
  AObj := AnchorDestination.FloatingObject;
  AObj.RelativeHeight := TdxFloatingObjectRelativeHeight.Create(RelativeFrom, Value);
end;

class function TdxDrawingAnchorVerticalRelativeSizeDestination.OnPictureHeight(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingAnchorRelativeSizeValueDestination.Create(AImporter, GetThis(AImporter));
end;

{ TdxDrawingAnchorRelativeSizeValueDestination }

constructor TdxDrawingAnchorRelativeSizeValueDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  APositionDestination: TdxDrawingAnchorRelativeSizeBaseDestination);
begin
  inherited Create(AImporter);
  Assert(APositionDestination <> nil, 'positionDestination');
  FSizeDestination := APositionDestination;
end;

function TdxDrawingAnchorRelativeSizeValueDestination.ProcessText(AReader: TdxXmlReader): Boolean;
var
  AText: string;
begin
  AText := AReader.Value;
  if AText <> '' then
    FSizeDestination.Value := Importer.GetIntegerValue(AText, TdxNumberStyles.Integer, 100 * 1000);
  Result := True;
end;

{ TdxDrawingAnchorPositionBaseDestination }

constructor TdxDrawingAnchorPositionBaseDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AAnchorDestination: TdxDrawingAnchorDestination);
begin
  inherited Create(AImporter);
  FOffset := MinInt;
  FPercentOffset := MinInt;
  Assert(AAnchorDestination <> nil, 'anchorDestination');
  FAnchorDestination := AAnchorDestination;
end;

class function TdxDrawingAnchorPositionBaseDestination.GetThis(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingAnchorPositionBaseDestination;
begin
  Result := TdxDrawingAnchorPositionBaseDestination(AImporter.PeekDestination);
end;

procedure TdxDrawingAnchorPositionBaseDestination.ProcessElementClose(AReader: TdxXmlReader);
begin
  if FAnchorDestination.UseSimplePosition then
    Exit;

  ProcessElementCloseCore(AReader);
end;

class function TdxDrawingAnchorPositionBaseDestination.OnPositionOffset(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingAnchorPositionOffsetDestination.Create(AImporter, GetThis(AImporter));
end;

class function TdxDrawingAnchorPositionBaseDestination.OnPositionPercentOffset(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingAnchorPositionPercentOffsetDestination.Create(AImporter, GetThis(AImporter));
end;

{ TdxDrawingAnchorHorizontalPositionDestination }

class constructor TdxDrawingAnchorHorizontalPositionDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDrawingAnchorHorizontalPositionDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDrawingAnchorHorizontalPositionDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('posOffset', OnPositionOffset);
  Result.Add('pctPosHOffset', OnPositionPercentOffset);
  Result.Add('align', OnHorizontalAlignment);
end;

function TdxDrawingAnchorHorizontalPositionDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxDrawingAnchorHorizontalPositionDestination.GetThis(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingAnchorHorizontalPositionDestination;
begin
  Result := TdxDrawingAnchorHorizontalPositionDestination(AImporter.PeekDestination);
end;

procedure TdxDrawingAnchorHorizontalPositionDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FRelativeTo := TdxWordProcessingMLBaseImporter(Importer).GetEnumValue<TdxFloatingObjectHorizontalPositionType>(AReader,
    'relativeFrom', TdxOpenXmlExporter.FloatingObjectHorizontalPositionTypeTable, TdxFloatingObjectHorizontalPositionType.Page);
end;

procedure TdxDrawingAnchorHorizontalPositionDestination.ProcessElementCloseCore(AReader: TdxXmlReader);
var
  AObj: TdxFloatingObjectProperties;
  APoint, APercentOffset: TPoint;
begin
  AObj := AnchorDestination.FloatingObject;
  APoint := AObj.Offset;
  APoint.X := AnchorDestination.ConvertEmuToDocumentUnits(Offset);
  if Offset <> MinInt then
    AObj.Offset := APoint;
  APercentOffset := AObj.PercentOffset;
  APercentOffset.X := PercentOffset;
  if PercentOffset <> MinInt then
    AObj.PercentOffset := APercentOffset;
  if AObj.HorizontalPositionAlignment <> Alignment then
    AObj.HorizontalPositionAlignment := Alignment;
  if AObj.HorizontalPositionType <> FRelativeTo then
    AObj.HorizontalPositionType := FRelativeTo;
end;

class function TdxDrawingAnchorHorizontalPositionDestination.OnHorizontalAlignment(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingAnchorPositionHorizontalAlignmentDestination.Create(AImporter, GetThis(AImporter));
end;

{ TdxDrawingAnchorVerticalPositionDestination }

class constructor TdxDrawingAnchorVerticalPositionDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxDrawingAnchorVerticalPositionDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxDrawingAnchorVerticalPositionDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('posOffset', OnPositionOffset);
  Result.Add('pctPosVOffset', OnPositionPercentOffset);
  Result.Add('align', OnVerticalAlignment);
end;

function TdxDrawingAnchorVerticalPositionDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxDrawingAnchorVerticalPositionDestination.GetThis(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDrawingAnchorVerticalPositionDestination;
begin
  Result := TdxDrawingAnchorVerticalPositionDestination(AImporter.PeekDestination);
end;

procedure TdxDrawingAnchorVerticalPositionDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FRelativeTo := TdxWordProcessingMLBaseImporter(Importer).GetEnumValue<TdxFloatingObjectVerticalPositionType>(AReader,
    'relativeFrom', TdxOpenXmlExporter.floatingObjectVerticalPositionTypeTable,
    TdxFloatingObjectVerticalPositionType.Page);
end;

procedure TdxDrawingAnchorVerticalPositionDestination.ProcessElementCloseCore(AReader: TdxXmlReader);
var
  AObj: TdxFloatingObjectProperties;
  APoint, APercentOffset: TPoint;
begin
  AObj := AnchorDestination.FloatingObject;
  APoint := AObj.Offset;
  APoint.Y := AnchorDestination.ConvertEmuToDocumentUnits(Offset);
  if Offset <> MinInt then
    AObj.Offset := APoint;
  APercentOffset := AObj.PercentOffset;
  APercentOffset.Y := PercentOffset;
  if PercentOffset <> MinInt then
    AObj.PercentOffset := APercentOffset;
  if AObj.VerticalPositionAlignment <> Alignment then
    AObj.VerticalPositionAlignment := Alignment;
  if AObj.VerticalPositionType <> FRelativeTo then
    AObj.VerticalPositionType := FRelativeTo;
end;

class function TdxDrawingAnchorVerticalPositionDestination.OnVerticalAlignment(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxDrawingAnchorPositionVerticalAlignmentDestination.Create(AImporter, GetThis(AImporter));
end;

{ TdxDrawingAnchorPositionOffsetDestination }

constructor TdxDrawingAnchorPositionOffsetDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  APositionDestination: TdxDrawingAnchorPositionBaseDestination);
begin
  inherited Create(AImporter);
  Assert(APositionDestination <> nil, 'positionDestination');
  FPositionDestination := APositionDestination;
end;

function TdxDrawingAnchorPositionOffsetDestination.ProcessText(AReader: TdxXmlReader): Boolean;
var
  AText: string;
begin
  AText := AReader.Value;
  FPositionDestination.Offset := Importer.GetIntegerValue(AText, TdxNumberStyles.Number, 0);
  Result := True;
end;

{ TdxDrawingAnchorPositionPercentOffsetDestination }

constructor TdxDrawingAnchorPositionPercentOffsetDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  APositionDestination: TdxDrawingAnchorPositionBaseDestination);
begin
  inherited Create(AImporter);
  Assert(APositionDestination <> nil, 'positionDestination');
  FPositionDestination := APositionDestination;
end;

function TdxDrawingAnchorPositionPercentOffsetDestination.ProcessText(AReader: TdxXmlReader): Boolean;
var
  AText: string;
begin
  AText := AReader.Value;
  if AText <> '' then
    FPositionDestination.PercentOffset := Importer.GetIntegerValue(AText, TdxNumberStyles.Integer, 0);
  Result := True;
end;

{ TdxDrawingAnchorPositionHorizontalAlignmentDestination }

constructor TdxDrawingAnchorPositionHorizontalAlignmentDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  APositionDestination: TdxDrawingAnchorHorizontalPositionDestination);
begin
  inherited Create(AImporter);
  Assert(APositionDestination <> nil, 'positionDestination');
  FPositionDestination := APositionDestination;
end;

function TdxDrawingAnchorPositionHorizontalAlignmentDestination.ProcessText(AReader: TdxXmlReader): Boolean;
var
  AText: string;
begin
  AText := AReader.Value;
  if AText <> '' then
    FPositionDestination.Alignment := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValueCore<TdxFloatingObjectHorizontalPositionAlignment>(AText,
      TdxOpenXmlExporter.FloatingObjectHorizontalPositionAlignmentTable, TdxFloatingObjectHorizontalPositionAlignment.Left);
  Result := True;
end;

{ TdxDrawingAnchorPositionVerticalAlignmentDestination }

constructor TdxDrawingAnchorPositionVerticalAlignmentDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  APositionDestination: TdxDrawingAnchorVerticalPositionDestination);
begin
  inherited Create(AImporter);
  Assert(APositionDestination <> nil, 'positionDestination');
  FPositionDestination := APositionDestination;
end;

function TdxDrawingAnchorPositionVerticalAlignmentDestination.ProcessText(AReader: TdxXmlReader): Boolean;
var
  AText: string;
begin
  AText := AReader.Value;
  if AText <> '' then
    FPositionDestination.Alignment := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValueCore<TdxFloatingObjectVerticalPositionAlignment>(AText,
      TdxOpenXmlExporter.FloatingObjectVerticalPositionAlignmentTable, TdxFloatingObjectVerticalPositionAlignment.Top);
  Result := True;
end;

{ TdxDrawingAnchorWrapNoneDestination }

constructor TdxDrawingAnchorWrapNoneDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AAnchorDestination: TdxDrawingAnchorDestination);
begin
  inherited Create(AImporter);
  Assert(AAnchorDestination <> nil, 'anchorDestination');
  FAnchorDestination := AAnchorDestination;
end;

procedure TdxDrawingAnchorWrapNoneDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FAnchorDestination.FloatingObject.TextWrapType := TdxFloatingObjectTextWrapType.None;
end;

{ TdxDrawingAnchorWrapTopAndBottomDestination }

constructor TdxDrawingAnchorWrapTopAndBottomDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AAnchorDestination: TdxDrawingAnchorDestination);
begin
  inherited Create(AImporter);
  Assert(AAnchorDestination <> nil, 'anchorDestination');
  FAnchorDestination := AAnchorDestination;
end;

procedure TdxDrawingAnchorWrapTopAndBottomDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FAnchorDestination.FloatingObject.TextWrapType := TdxFloatingObjectTextWrapType.TopAndBottom;
end;

{ TdxDrawingAnchorPolygonDestinationBase }

constructor TdxDrawingAnchorPolygonDestinationBase.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AAnchorDestination: TdxDrawingAnchorDestination);
begin
  inherited Create(AImporter);
  Assert(AAnchorDestination <> nil, 'anchorDestination');
  FAnchorDestination := AAnchorDestination;
end;

procedure TdxDrawingAnchorPolygonDestinationBase.ProcessElementOpen(AReader: TdxXmlReader);
begin
  if AReader.GetAttribute('wrapText', '') <> '' then
    AnchorDestination.FloatingObject.TextWrapSide := TdxWordProcessingMLBaseImporter(Importer).GetEnumValue<TdxFloatingObjectTextWrapSide>(
      AReader, 'wrapText', TdxOpenXmlExporter.FloatingObjectTextWrapSideTable, TdxFloatingObjectTextWrapSide.Both);
end;

{ TdxDrawingAnchorWrapSquareDestination }

procedure TdxDrawingAnchorWrapSquareDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  inherited ProcessElementOpen(AReader);
  AnchorDestination.FloatingObject.TextWrapType := TdxFloatingObjectTextWrapType.Square;
end;

{ TdxDrawingAnchorWrapThroughDestination }

procedure TdxDrawingAnchorWrapThroughDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  inherited ProcessElementOpen(AReader);
  AnchorDestination.FloatingObject.TextWrapType := TdxFloatingObjectTextWrapType.Through;
end;

{ TdxDrawingAnchorWrapTightDestination }

procedure TdxDrawingAnchorWrapTightDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  inherited ProcessElementOpen(AReader);
  AnchorDestination.FloatingObject.TextWrapType := TdxFloatingObjectTextWrapType.Tight;
end;

{ TdxWordProcessingShapeDestination }

constructor TdxWordProcessingShapeDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
begin
  inherited Create(AImporter);
  Assert(AFloatingObjectImportInfo <> nil, 'FloatingObjectImportInfo');
  FloatingObjectImportInfo := AFloatingObjectImportInfo;
end;

class constructor TdxWordProcessingShapeDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxWordProcessingShapeDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxWordProcessingShapeDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('txbx', OnTextBox);
  Result.Add('bodyPr', OnTextBoxProperties);
  Result.Add('spPr', OnShapeProperties);
end;

function TdxWordProcessingShapeDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxWordProcessingShapeDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxWordProcessingShapeDestination;
begin
  Result := TdxWordProcessingShapeDestination(AImporter.PeekDestination);
end;

class function TdxWordProcessingShapeDestination.OnTextBox(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTextBoxDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), GetThis(AImporter).FloatingObjectImportInfo);
end;

class function TdxWordProcessingShapeDestination.OnTextBoxProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTextBoxPropertiesDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), GetThis(AImporter).FloatingObjectImportInfo.TextBoxProperties);
end;

class function TdxWordProcessingShapeDestination.OnShapeProperties(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingShapePropertiesDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), GetThis(AImporter).FloatingObjectImportInfo.Shape);
end;

{ TdxWordProcessingShapePropertiesDestination }

constructor TdxWordProcessingShapePropertiesDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AShape: TdxShape);
begin
  inherited Create(AImporter);
  Assert(AShape <> nil, 'shape');
  Shape := AShape;
end;

class constructor TdxWordProcessingShapePropertiesDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxWordProcessingShapePropertiesDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxWordProcessingShapePropertiesDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('solidFill', OnSolidFill);
  Result.Add('noFill', OnNoFill);
  Result.Add('ln', OnOutline);
  Result.Add('xfrm', OnGraphicFrame);
end;

function TdxWordProcessingShapePropertiesDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxWordProcessingShapePropertiesDestination.GetThis(
  AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxWordProcessingShapePropertiesDestination;
begin
  Result := TdxWordProcessingShapePropertiesDestination(AImporter.PeekDestination);
end;

class function TdxWordProcessingShapePropertiesDestination.OnSolidFill(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSolidFillDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), TdxShapeSolidFillAccessor.Create(GetThis(AImporter).Shape));
end;

class function TdxWordProcessingShapePropertiesDestination.OnNoFill(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxNoFillDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), TdxShapeSolidFillAccessor.Create(GetThis(AImporter).Shape));
end;

class function TdxWordProcessingShapePropertiesDestination.OnOutline(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxOutlineDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), TdxShapeOutlineAccessor.Create(GetThis(AImporter).Shape));
end;

class function TdxWordProcessingShapePropertiesDestination.OnGraphicFrame(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxGraphicFrameDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), GetThis(AImporter).Shape);
end;

{ TdxWordProcessingShapePropertiesDestination.TdxShapeSolidFillAccessor }

constructor TdxWordProcessingShapePropertiesDestination.TdxShapeSolidFillAccessor.Create(AShape: TdxShape);
begin
  inherited Create;
  FShape := AShape;
end;

function TdxWordProcessingShapePropertiesDestination.TdxShapeSolidFillAccessor.GetFillColor: TdxAlphaColor;
begin
  Result := FShape.FillColor;
end;

procedure TdxWordProcessingShapePropertiesDestination.TdxShapeSolidFillAccessor.SetFillColor(const AValue: TdxAlphaColor);
begin
  FShape.FillColor := AValue;
end;

function TdxWordProcessingShapePropertiesDestination.TdxShapeSolidFillAccessor.GetColor: TdxAlphaColor;
begin
  Result := FShape.FillColor;
end;

procedure TdxWordProcessingShapePropertiesDestination.TdxShapeSolidFillAccessor.SetColor(const AValue: TdxAlphaColor);
begin
  FShape.FillColor := AValue;
end;

{ TdxShapeOutlineAccessor }

constructor TdxWordProcessingShapePropertiesDestination.TdxShapeOutlineAccessor.Create(AShape: TdxShape);
begin
  inherited Create;
  FShape := AShape;
end;

function TdxWordProcessingShapePropertiesDestination.TdxShapeOutlineAccessor.GetOutlineColor: TdxAlphaColor;
begin
  Result := FShape.OutlineColor;
end;

procedure TdxWordProcessingShapePropertiesDestination.TdxShapeOutlineAccessor.SetOutlineColor(const AValue: TdxAlphaColor);
begin
  FShape.OutlineColor := AValue;
end;

function TdxWordProcessingShapePropertiesDestination.TdxShapeOutlineAccessor.GetOutlineWidth: Integer;
begin
  Result := FShape.OutlineWidth;
end;

procedure TdxWordProcessingShapePropertiesDestination.TdxShapeOutlineAccessor.SetOutlineWidth(const AValue: Integer);
begin
  FShape.OutlineWidth := AValue;
end;

function TdxWordProcessingShapePropertiesDestination.TdxShapeOutlineAccessor.GetFillColor: TdxAlphaColor;
begin
  Result := FShape.OutlineColor;
end;

procedure TdxWordProcessingShapePropertiesDestination.TdxShapeOutlineAccessor.SetFillColor(const AValue: TdxAlphaColor);
begin
  FShape.OutlineColor := AValue;
end;

function TdxWordProcessingShapePropertiesDestination.TdxShapeOutlineAccessor.GetColor: TdxAlphaColor;
begin
  Result := FShape.OutlineColor;
end;

procedure TdxWordProcessingShapePropertiesDestination.TdxShapeOutlineAccessor.SetColor(const AValue: TdxAlphaColor);
begin
  FShape.OutlineColor := AValue;
end;

{ TdxSolidFillDestination }

constructor TdxSolidFillDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  const ASolidFillAccessor: IdxSolidFillAccessor);
begin
  inherited Create(AImporter);
  Assert(ASolidFillAccessor <> nil, 'solidFillAccessor');
  FSolidFillAccessor := ASolidFillAccessor;
end;

class constructor TdxSolidFillDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxSolidFillDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxSolidFillDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('srgbClr', OnSRgbColor);
  Result.Add('schemeClr', OnSchemeColor);
  Result.Add('prstClr', OnPresetColor);
end;

function TdxSolidFillDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxSolidFillDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxSolidFillDestination;
begin
  Result := TdxSolidFillDestination(AImporter.PeekDestination);
end;

class function TdxSolidFillDestination.OnSRgbColor(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSRgbColorDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), GetThis(AImporter).SolidFillAccessor);
end;

class function TdxSolidFillDestination.OnPresetColor(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxPresetColorDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), GetThis(AImporter).SolidFillAccessor);
end;

class function TdxSolidFillDestination.OnSchemeColor(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSchemeColorDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), GetThis(AImporter).SolidFillAccessor);
end;

{ TdxOutlineDestination }

constructor TdxOutlineDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  const AOutlineAccessor: IdxOutlineAccessor);
begin
  inherited Create(AImporter);
  Assert(AOutlineAccessor <> nil, 'outlineAccessor');
  FOutlineAccessor := AOutlineAccessor;
end;

class constructor TdxOutlineDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxOutlineDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxOutlineDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('solidFill', OnSolidFill);
  Result.Add('noFill', OnNoFill);
end;

function TdxOutlineDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

procedure TdxOutlineDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: Integer;
begin
  AValue := Importer.GetIntegerValue(AReader, 'w', MinInt);
  if AValue <> MinInt then
    FOutlineAccessor.OutlineWidth := Importer.UnitConverter.EmuToModelUnits(AValue);
end;

class function TdxOutlineDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxOutlineDestination;
begin
  Result := TdxOutlineDestination(AImporter.PeekDestination);
end;

class function TdxOutlineDestination.OnSolidFill(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxSolidFillDestination.Create(AImporter, GetThis(AImporter).outlineAccessor);
end;

class function TdxOutlineDestination.OnNoFill(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxNoFillDestination.Create(AImporter, GetThis(AImporter).outlineAccessor);
end;

{ TdxSRgbColorDestination }

constructor TdxSRgbColorDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  const AColorAccessor: IdxColorAccessor);
begin
  inherited Create(AImporter);
  Assert(AColorAccessor <> nil, 'colorAccessor');
  FColorAccessor := AColorAccessor;
end;

class constructor TdxSRgbColorDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxSRgbColorDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxSRgbColorDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('alpha', OnAlphaColor);
end;

function TdxSRgbColorDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxSRgbColorDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxSRgbColorDestination;
begin
  Result := TdxSRgbColorDestination(AImporter.PeekDestination);
end;

class function TdxSRgbColorDestination.OnAlphaColor(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxAlphaColorDestination.Create(AImporter, GetThis(AImporter).ColorAccessor);
end;

procedure TdxSRgbColorDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AColor: TdxAlphaColor;
begin
  AColor := TdxWordProcessingMLBaseImporter(Importer).GetColorValue(AReader, 'val');
  if AColor <> TdxAlphaColors.Empty then
    FColorAccessor.Color := AColor;
end;

{ TdxAlphaColorDestination }

constructor TdxAlphaColorDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; const AColorAccessor: IdxColorAccessor);
begin
  inherited Create(AImporter);
  FColorAccessor := AColorAccessor;
end;

procedure TdxAlphaColorDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  APercentageValue: Integer;
  AAlpha: Byte;
begin
  APercentageValue := Importer.GetIntegerValue(AReader, 'val');
  AAlpha := Byte(Round(255 * APercentageValue * 0.00001));
  FColorAccessor.Color := TdxAlphaColors.FromArgb(AAlpha, FColorAccessor.Color);
end;

{ TdxPresetColorDestination }

constructor TdxPresetColorDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  const AColorAccessor: IdxColorAccessor);
begin
  inherited Create(AImporter);
  Assert(AColorAccessor <> nil, 'colorAccessor');
  FColorAccessor := AColorAccessor;
end;

procedure TdxPresetColorDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AColor: TdxAlphaColor;
begin
  AColor := TdxWordProcessingMLBaseImporter(Importer).GetPresetColorValue(AReader, 'val');
  if AColor <> TdxAlphaColors.Empty then
    FColorAccessor.Color := AColor;
end;

{ TdxNoFillDestination }

constructor TdxNoFillDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  const AColorAccessor: IdxColorAccessor);
begin
  inherited Create(AImporter);
  Assert(AColorAccessor <> nil, 'colorAccessor');
  FColorAccessor := AColorAccessor;
end;

procedure TdxNoFillDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FColorAccessor.Color := TdxAlphaColors.Empty;
end;

{ TdxColorScheme }

constructor TdxColorScheme.Create;
begin
  FColors := TdxStringColorDictionary.Create;
  FColors.Add('accent1', TdxAlphaColors.FromArgb($FF, $4F, $81, $BD));
  FColors.Add('accent2', TdxAlphaColors.FromArgb($FF, $C0, $50, $4D));
  FColors.Add('accent3', TdxAlphaColors.FromArgb($FF, $9B, $BB, $59));
  FColors.Add('accent4', TdxAlphaColors.FromArgb($FF, $80, $64, $A2));
  FColors.Add('accent5', TdxAlphaColors.FromArgb($FF, $4B, $AC, $C6));
  FColors.Add('accent6', TdxAlphaColors.FromArgb($FF, $F7, $96, $46));

  FColors.Add('bg1',     TdxAlphaColors.FromArgb($FF, $FF, $FF, $FF));
  FColors.Add('bg2',     TdxAlphaColors.FromArgb($FF, $FF, $FF, $FF));
  FColors.Add('dk1',     TdxAlphaColors.FromArgb($FF, $00, $00, $00));
  FColors.Add('dk2',     TdxAlphaColors.FromArgb($FF, $1F, $49, $7D));
  FColors.Add('folHlink',TdxAlphaColors.FromArgb($FF, $80, $00, $80));
  FColors.Add('hlink',   TdxAlphaColors.FromArgb($FF, $00, $00, $FF));
  FColors.Add('lt1',     TdxAlphaColors.FromArgb($FF, $FF, $FF, $FF));
  FColors.Add('lt2',     TdxAlphaColors.FromArgb($FF, $EE, $EC, $E1));
  FColors.Add('phClr',   TdxAlphaColors.FromArgb($FF, $00, $00, $00));
  FColors.Add('tx1',     TdxAlphaColors.FromArgb($FF, $00, $00, $00));
  FColors.Add('tx2',     TdxAlphaColors.FromArgb($FF, $00, $00, $00));
end;

destructor TdxColorScheme.Destroy;
begin
  FColors.Free;
  inherited Destroy;
end;

function TdxColorScheme.GetColorByName(const AName: string): TdxAlphaColor;
begin
  if not FColors.TryGetValue(AName, Result) then
    Result := TdxAlphaColors.Empty;
end;

{ TdxSchemeColorDestination }

constructor TdxSchemeColorDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  const AColorAccessor: IdxColorAccessor);
begin
  inherited Create(AImporter);
  Assert(AColorAccessor <> nil, 'colorAccessor');
  FColorAccessor := AColorAccessor;
end;

class constructor TdxSchemeColorDestination.Initialize;
begin
  FColorScheme := TdxColorScheme.Create;
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxSchemeColorDestination.Finalize;
begin
  FHandlerTable.Free;
  FColorScheme.Free;
end;

class function TdxSchemeColorDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('alphaOff', OnAlphaOffset);
  Result.Add('alphaMod', OnAlphaModulation);
  Result.Add('alpha', OnAlpha);
end;

class function TdxSchemeColorDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxSchemeColorDestination;
begin
  Result := TdxSchemeColorDestination(AImporter.PeekDestination);
end;

class function TdxSchemeColorDestination.OnAlphaOffset(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
var
  ASetter: TdxAction<Integer>;
begin
  ASetter := procedure (const Value: Integer)
    begin
      GetThis(AImporter).FAlphaOffset := Value / 1000;
    end;
  Result := TdxPropertyDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), ASetter);
end;

class function TdxSchemeColorDestination.OnAlpha(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
var
  ASetter: TdxAction<Integer>;
begin
  ASetter := procedure (const Value: Integer)
    begin
      GetThis(AImporter).FAlpha := Min(100, Abs(Value) / 1000.0);
    end;
  Result := TdxPropertyDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), ASetter);
end;

class function TdxSchemeColorDestination.OnAlphaModulation(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
var
  ASetter: TdxAction<Integer>;
begin
  ASetter := procedure (const Value: Integer)
    begin
      GetThis(AImporter).FAlphaModulation := Abs(Value) / 1000.0;
    end;
  Result := TdxPropertyDestination.Create(TdxWordProcessingMLBaseImporter(AImporter), ASetter);
end;

function TdxSchemeColorDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

procedure TdxSchemeColorDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AColorName: string;
begin

  AColorName := AReader.GetAttribute('val');
  if AColorName = '' then
    Exit;

  FBaseColor := FColorScheme.GetColorByName(AColorName);
end;

procedure TdxSchemeColorDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  AActualAlpha: Integer;
begin
  inherited ProcessElementClose(AReader);
  if TdxAlphaColors.IsTransparentOrEmpty(FBaseColor) then
    Exit;

  AActualAlpha := CalculateActualAlpha(TdxAlphaColors.Alpha(FBaseColor));
  FColorAccessor.Color := TdxAlphaColors.FromArgb(AActualAlpha, TdxAlphaColors.R(FBaseColor),
    TdxAlphaColors.G(FBaseColor), TdxAlphaColors.B(FBaseColor));
end;

function TdxSchemeColorDestination.CalculateActualAlpha(AValue: Integer): Integer;
var
  AAlphaFactor: Single;
begin
  AAlphaFactor := CalculateColorFactor(FAlpha, FAlphaModulation, FAlphaOffset);
  Result := Trunc(AValue * AAlphaFactor);
end;

function TdxSchemeColorDestination.CalculateColorFactor(const AValue, AModulation, AOffset: TdxNullableValue<Single>): Single;
var
  AModulationFactor, AActualValue: Single;
begin
  if AModulation.IsNull then
    AModulationFactor := 1.0
  else
    AModulationFactor := AModulation.Value / 100.0;
  if AValue.IsNull then
    AActualValue := 100
  else
    AActualValue := AValue.Value;
  AActualValue := Min(100, AActualValue * AModulationFactor);
  if AOffset.HasValue then
    AActualValue := AActualValue + AOffset.Value;
  AActualValue := Validate(0, 100, AActualValue);
  Result := AActualValue / 100.0;
end;

function TdxSchemeColorDestination.Validate(AMin, AMax, AValue: Single): Single;
begin
  if AValue < AMin then
    Exit(AMin);
  if AValue < AMax then
    Result := AValue
  else
    Result := AMax;
end;

{ TdxTextBoxDestination }

constructor TdxTextBoxDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AFloatingObjectImportInfo: TdxFloatingObjectImportInfo);
begin
  inherited Create(AImporter);
  Assert(AFloatingObjectImportInfo <> nil, 'floatingObjectImportInfo');
  FFloatingObjectImportInfo := AFloatingObjectImportInfo;
  FFloatingObjectImportInfo.ShapeType := TdxShapeType.TextBox;
  Importer.PushCurrentPieceTable(TdxTextBoxContentType.Create(AImporter.DocumentModel).PieceTable);
end;

class constructor TdxTextBoxDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTextBoxDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTextBoxDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('txbxContent', OnTextBoxContent);
end;

function TdxTextBoxDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

procedure TdxTextBoxDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AStyle, AInset: string;
  ATextBoxProperties: TdxTextBoxProperties;
  AMargins: TArray<Integer>;
begin
  AStyle := AReader.GetAttribute('style');
  AInset := AReader.GetAttribute('inset');

  ATextBoxProperties := FloatingObjectImportInfo.TextBoxProperties;
  if AStyle <> '' then
    ATextBoxProperties.ResizeShapeToFitText := GetResizeShapeToFitText(AStyle);

  if AInset = '' then
    Exit;

  AMargins := GetMargins(AInset);
  ATextBoxProperties.LeftMargin := GetValidMarginValue(AMargins[0], UnitConverter.DocumentsToModelUnits(30));
  ATextBoxProperties.RightMargin := GetValidMarginValue(AMargins[1], UnitConverter.DocumentsToModelUnits(30));
  ATextBoxProperties.TopMargin := GetValidMarginValue(AMargins[2], UnitConverter.DocumentsToModelUnits(15));
  ATextBoxProperties.BottomMargin := GetValidMarginValue(AMargins[3], UnitConverter.DocumentsToModelUnits(15));
end;

procedure TdxTextBoxDestination.ProcessElementClose(AReader: TdxXmlReader);
var
  ATextBoxContent: TdxTextBoxContentType;
begin
  ATextBoxContent := TdxTextBoxContentType(Importer.PieceTable.ContentType);
  Importer.PieceTable.FixLastParagraph;
  Importer.InsertBookmarks;
  Importer.InsertRangePermissions;
  Importer.PieceTable.FixTables;
  Importer.PopCurrentPieceTable;
  FloatingObjectImportInfo.TextBoxContent := ATextBoxContent;
end;

function TdxTextBoxDestination.GetMargins(const AStrMargins: string): TArray<Integer>;
var
  AMargins: TArray<string>;
  I: Integer;
begin
  SetLength(Result, 4);
  AMargins := TdxStringHelper.Split(AStrMargins, [',']);
  for I := 0 to Length(AMargins) - 1 do
    Result[I] := GetFloatValue(AMargins[I]);

  if Length(AMargins) < 4 then
    for I := Length(AMargins) to Length(Result) - 1 do
      Result[I] := UnitConverter.DocumentsToModelUnits(15 + IfThen(i < 2, 15, 0));
end;

function TdxTextBoxDestination.GetResizeShapeToFitText(const AStyle: string): Boolean;
begin
  Result := AStyle[Length(AStyle)] = 't';
end;

function TdxTextBoxDestination.GetValidMarginValue(AValue: Integer; ADefaultValue: Integer): Integer;
begin
  if AValue < 0 then
    Result := ADefaultValue
  else
    Result := AValue;
end;

function TdxTextBoxDestination.GetFloatValue(const ANumber: string): Integer;
var
  AValueUnit: TdxValueInfo;
  AUnitsConverter: TdxUnitsConverter;
begin
  AValueUnit := TdxStringValueParser.TryParse(ANumber);
  if not AValueUnit.IsValidNumber then
    Exit(MinInt);

  AUnitsConverter := TdxUnitsConverter.Create(UnitConverter);
  Result := Trunc(AUnitsConverter.ValueUnitToModelUnitsF(AValueUnit));
end;

class function TdxTextBoxDestination.OnTextBoxContent(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxTextBoxContentDestination.Create(AImporter);
end;

{ TdxTextBoxPropertiesDestination }

constructor TdxTextBoxPropertiesDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  ATextBoxProperties: TdxTextBoxProperties);
begin
  inherited Create(AImporter);
  Assert(ATextBoxProperties <> nil, 'textBoxProperties');
  FTextBoxProperties := ATextBoxProperties;
end;

class constructor TdxTextBoxPropertiesDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTextBoxPropertiesDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTextBoxPropertiesDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('noAutofit', OnDisableAutoFit);
  Result.Add('spAutoFit', OnEnableAutoFit);
end;

function TdxTextBoxPropertiesDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

procedure TdxTextBoxPropertiesDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: Integer;
  AWrapType: string;
  AInvalidValue, AVerticalAlignment: TdxVerticalAlignment;
begin
  AValue := Importer.GetIntegerValue(AReader, 'lIns', MinInt);
  if AValue <> MinInt then
    FTextBoxProperties.LeftMargin := UnitConverter.EmuToModelUnits(AValue);
  AValue := Importer.GetIntegerValue(AReader, 'rIns', MinInt);
  if AValue <> MinInt then
    FTextBoxProperties.RightMargin := UnitConverter.EmuToModelUnits(AValue);
  AValue := Importer.GetIntegerValue(AReader, 'tIns', MinInt);
  if AValue <> MinInt then
    FTextBoxProperties.TopMargin := UnitConverter.EmuToModelUnits(AValue);
  AValue := Importer.GetIntegerValue(AReader, 'bIns', MinInt);
  if AValue <> MinInt then
    FTextBoxProperties.BottomMargin := UnitConverter.EmuToModelUnits(AValue);

  AWrapType := AReader.GetAttribute('wrap');
  if AWrapType = 'square' then
    FTextBoxProperties.WrapText := True
  else
    if AWrapType = 'none' then
      FTextBoxProperties.WrapText := False;
  AValue := Importer.GetIntegerValue(AReader, 'upright', 0);
  if AValue = 1 then
    FTextBoxProperties.Upright := True;

  AInvalidValue := TdxVerticalAlignment((-1));
  AVerticalAlignment := TdxWordProcessingMLBaseImporter(Importer).GetEnumValue<TdxVerticalAlignment>(AReader, 'anchor',
    TdxWordProcessingMLBaseExporter.TextBoxVerticalAlignmentTable, AInvalidValue);
  if AVerticalAlignment <> AInvalidValue then
    FTextBoxProperties.VerticalAlignment := AVerticalAlignment;
end;

class function TdxTextBoxPropertiesDestination.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxTextBoxPropertiesDestination;
begin
  Result := TdxTextBoxPropertiesDestination(AImporter.PeekDestination);
end;

class function TdxTextBoxPropertiesDestination.OnDisableAutoFit(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  GetThis(AImporter).FTextBoxProperties.ResizeShapeToFitText := False;
  Result := nil;
end;

class function TdxTextBoxPropertiesDestination.OnEnableAutoFit(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  GetThis(AImporter).FTextBoxProperties.ResizeShapeToFitText := True;
  Result := nil;
end;

{ TdxTextBoxContentDestination }

class constructor TdxTextBoxContentDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxTextBoxContentDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxTextBoxContentDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('p', OnParagraph);
  Result.Add('tbl', OnTable);

  Result.Add('bookmarkStart', OnBookmarkStart);
  Result.Add('bookmarkEnd', OnBookmarkEnd);
  Result.Add('permStart', OnRangePermissionStart);
  Result.Add('permEnd', OnRangePermissionEnd);
  Result.Add('sdt', OnStructuredDocument);
  Result.Add('altChunk', OnAltChunk);
  Result.Add('customXml', OnCustomXml);
end;

function TdxTextBoxContentDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxTextBoxContentDestination.OnTable(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  if AImporter.DocumentModel.DocumentCapabilities.TablesAllowed then
    Result := TdxTableDestination.Create(AImporter)
  else
    Result := TdxTableDisabledDestination.Create(AImporter);
end;

class function TdxTextBoxContentDestination.OnParagraph(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateParagraphDestination;
end;

class function TdxTextBoxContentDestination.OnAltChunk(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxAltChunkDestination.Create(AImporter);
end;

{ TdxGraphicFrameDestination }

constructor TdxGraphicFrameDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AShape: TdxShape);
begin
  inherited Create(AImporter);
  Assert(AShape <> nil, 'shape');
  Shape := AShape;
end;

procedure TdxGraphicFrameDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AValue: Integer;
begin
  AValue := Importer.GetIntegerValue(AReader, 'rot', MinInt);
  if AValue <> MinInt then
    Shape.Rotation := UnitConverter.AdjAngleToModelUnits(AValue);
end;

end.
