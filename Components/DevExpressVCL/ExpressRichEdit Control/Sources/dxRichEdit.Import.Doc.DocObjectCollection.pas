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
unit dxRichEdit.Import.Doc.DocObjectCollection;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore,
  dxCoreClasses, dxCoreGraphics, dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.ShapeFormatting,
  dxRichEdit.DocumentFormats.DocumentFormatUtils,
  dxRichEdit.Import,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.BlipContainer,
  dxRichEdit.Import.Doc.OfficeArtContent,
  dxRichEdit.Import.Doc.BorderDescriptor,
  dxRichEdit.Import.Doc.DCO,
  dxRichEdit.Import.Doc.DocFieldsImportHelper,
  dxRichEdit.Import.Doc.FileShapeAddress;

type
  TdxDocObjectType = (
    PictureFloatingObject,
    TextBoxFloatingObject,
    InlineImage,
    ImageCollection,
    AutoNumberedFootnoteReference,
    NoteNumber,
    AnnotationReference,
    EndnoteReference,
    FieldBegin,
    FieldSeparator,
    FieldEnd,
    HyperlinkFieldData,
    TextRun,
    TableCell,
    TableRow,
    Paragraph,
    Section,
    ExpectedFieldBegin,
    ExpectedFieldSeparator,
    ExpectedFieldEnd,
    UnsupportedObject
  );

  { IdxDocObject }

  IdxDocObject = interface
  ['{0B205926-4FE5-47F6-BEE8-EBAB7BC6C937}']
    function GetDocObjectType: TdxDocObjectType;
    function GetPropertyContainer: TdxDocPropertyContainer;
    function GetPosition: Integer;
    function GetLength: Integer;
    function GetObject: TObject;

    property DocObjectType: TdxDocObjectType read GetDocObjectType;
    property PropertyContainer: TdxDocPropertyContainer read GetPropertyContainer;
    property Position: Integer read GetPosition;
    property Length: Integer read GetLength;
  end;

  TdxDocObjectCollection = class(TList<IdxDocObject>);
  TdxDocObjectCollectionDictionary = class(TObjectDictionary<Integer, TdxDocObjectCollection>);

  { TdxDocObjectInfo }

  TdxDocObjectInfo = record
  public
    Position: Integer;
    Text: string;
    constructor Create(APosition: Integer; const AText: string);
  end;

  { TdxDocObjectBase }

  TdxDocObjectBase = class abstract(TInterfacedObject, IdxDocObject)
  strict private
    FPosition: Integer;
    FPropertyContainer: TdxDocPropertyContainer;
  protected
    function GetObject: TObject;
    function GetDocObjectType: TdxDocObjectType; virtual; abstract;
    function GetLength: Integer; virtual;
    function GetPropertyContainer: TdxDocPropertyContainer;
    function GetPosition: Integer;
  public
    constructor Create(const AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer); reintroduce; overload; virtual;

    property DocObjectType: TdxDocObjectType read GetDocObjectType;
    property PropertyContainer: TdxDocPropertyContainer read FPropertyContainer;
    property Position: Integer read FPosition;
    property Length: Integer read GetLength;
  end;
  TdxDocObjectBaseClass = class of TdxDocObjectBase;

  { TdxDocTextRun }

  TdxDocTextRun = class(TdxDocObjectBase)
  strict private
    FText: string;
  protected
    function GetDocObjectType: TdxDocObjectType; override;
    function GetLength: Integer; override;
  public
    constructor Create(const AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer); override;
    property Text: string read FText;
  end;

  { TdxDocNoteNumber }

  TdxDocNoteNumber = class(TdxDocObjectBase)
  protected
    function GetDocObjectType: TdxDocObjectType; override;
  end;

  { TdxDocAutoNumberedFootnoteReference }

  TdxDocAutoNumberedFootnoteReference = class(TdxDocObjectBase)
  protected
    function GetDocObjectType: TdxDocObjectType; override;
  end;

  { TdxDocEndnoteReference }

  TdxDocEndnoteReference = class(TdxDocObjectBase)
  protected
    function GetDocObjectType: TdxDocObjectType; override;
  end;

  { TdxDocAnnotationReference }

  TdxDocAnnotationReference = class(TdxDocObjectBase)
  protected
    function GetDocObjectType: TdxDocObjectType; override;
  end;

  { TdxDocParagraph }

  TdxDocParagraph = class(TdxDocObjectBase)
  protected
    function GetDocObjectType: TdxDocObjectType; override;
  end;

  { TdxDocSection }

  TdxDocSection = class(TdxDocObjectBase)
  protected
    function GetDocObjectType: TdxDocObjectType; override;
  end;

  TdxPictureDataFormat = (
    TiffFile    = $0062,
    BmpFile     = $0063,
    ShapeObject = $0064,
    ShapeFile   = $0066
  );

  { TdxPictureDescriptor }

  TdxPictureDescriptor = class
  public const
    InnerHeaderSize        = Integer($14);
    ObsoletePropertiesSize = Integer($6);
    ReservedDataSize       = Integer($a);
    PicturePropertiesSize  = SmallInt($44);
    MaxImageSize           = Integer($7bc0);
  strict private class var
    FEmptyBorder: TdxBorderDescriptor97;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FDataFormat: TdxPictureDataFormat;
    FWidth: Word;
    FHeight: Word;
    FHorizontalScaleFactor: Word;
    FVerticalScaleFactor: Word;
    FInlineShapeContainer: TdxOfficeArtInlineShapeContainer;
    FTop: TdxBorderDescriptor97;
    FLeft: TdxBorderDescriptor97;
    FBottom: TdxBorderDescriptor97;
    FRight: TdxBorderDescriptor97;
    FPictureName: string;
    FMetafileHeaders: TdxList<TdxDocMetafileHeader>;
    FImages: TdxObjectList<TdxOfficeImageReference>;
    function GetProperties: TdxOfficeArtProperties;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer; const AImageCreator: IdxDocOfficeImageCreator);
    function CheckReader(AReader: TBinaryReader; AOffset: Integer): Boolean;
    procedure ReadCore(AReader: TBinaryReader; ASize: Integer; const AImageCreator: IdxDocOfficeImageCreator);
    procedure ReadImageSize(AReader: TBinaryReader);
    procedure InitializeBorders(AReader: TBinaryReader);
    function ReadPictureName(AReader: TBinaryReader): Integer;
    procedure InitializeShapeContainer(AReader: TBinaryReader; ASize: Integer;
      const AImageCreator: IdxDocOfficeImageCreator);
  public
    constructor Create; overload;
    constructor Create(ARun: TdxInlinePictureRun; AShapeId: Integer; ABlipIndex: Integer); overload;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; const AImageCreator: IdxDocOfficeImageCreator): TdxPictureDescriptor; static;
    procedure CalculateImageSize(ARun: TdxInlinePictureRun);
    procedure Write(AWriter: TBinaryWriter);
    procedure WriteDefaults(AWriter: TBinaryWriter);
    procedure WriteImageSize(AWriter: TBinaryWriter);
    procedure WritePictureDescriptorSize(AWriter: TBinaryWriter; AStart: Int64);
    procedure WriteEmptyBorders(AWriter: TBinaryWriter);

    property DataFormat: TdxPictureDataFormat read FDataFormat;
    property Width: Word read FWidth;
    property Height: Word read FHeight;
    property HorizontalScaleFactor: Word read FHorizontalScaleFactor;
    property VerticalScaleFactor: Word read FVerticalScaleFactor;
    property Top: TdxBorderDescriptor97 read FTop;
    property Left: TdxBorderDescriptor97 read FLeft;
    property Bottom: TdxBorderDescriptor97 read FBottom;
    property Right: TdxBorderDescriptor97 read FRight;
    property PictureName: string read FPictureName;
    property MetafileHeaders: TdxList<TdxDocMetafileHeader> read FMetafileHeaders;
    property Properties: TdxOfficeArtProperties read GetProperties;
    property Images: TdxObjectList<TdxOfficeImageReference> read FImages;
  end;

  { TdxDocImage }

  TdxDocImage = class(TdxDocObjectBase)
  strict private
    FImage: TdxOfficeImageReference;
    FScaleX: Integer;
    FScaleY: Integer;
    FMetafileHeader: TdxDocMetafileHeader;
  protected
    function GetDocObjectType: TdxDocObjectType; override;
  public
    constructor Create(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer;
      ADescriptor: TdxPictureDescriptor; APictureIndex: Integer); overload;
    destructor Destroy; override;
    property Image: TdxOfficeImageReference read FImage;
    property ScaleX: Integer read FScaleX;
    property ScaleY: Integer read FScaleY;
    property MetafileHeader: TdxDocMetafileHeader read FMetafileHeader;
  end;

  { TdxDocImageCollection }

  TdxDocImageCollection = class(TList<TdxDocImage>, IdxDocObject)
  strict private
    FPosition: Integer;
    FPropertyContainer: TdxDocPropertyContainer;
    function GetDocObjectType: TdxDocObjectType;
    function GetLength: Integer;
    function GetObject: TObject;
  protected
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetPosition: Integer;
    function GetPropertyContainer: TdxDocPropertyContainer;
  public
    constructor Create(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer;
       ADescriptor: TdxPictureDescriptor);

    property DocObjectType: TdxDocObjectType read GetDocObjectType;
    property PropertyContainer: TdxDocPropertyContainer read GetPropertyContainer;
    property Position: Integer read GetPosition;
    property Length: Integer read GetLength;
  end;

  { TdxDocFieldBegin }

  TdxDocFieldBegin = class(TdxDocObjectBase)
  protected
    function GetDocObjectType: TdxDocObjectType; override;
  end;

  { TdxDocFieldEnd }

  TdxDocFieldEnd = class(TdxDocObjectBase)
  protected
    function GetDocObjectType: TdxDocObjectType; override;
  end;

  { TdxDocFieldSeparator }

  TdxDocFieldSeparator = class(TdxDocObjectBase)
  strict private
    FOleObject: Boolean;
  protected
    function GetDocObjectType: TdxDocObjectType; override;
  public
    constructor Create(const AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer); override;

    property OleObject: Boolean read FOleObject;
  end;

  { TdxExpectedDocObject }

  TdxExpectedDocObject = class abstract(TdxDocObjectBase)
  strict private
    FType: TdxDocObjectType;
    FText: string;
  protected
    function GetActualType: TdxDocObjectType; virtual; abstract;
    function GetExpectedType: TdxDocObjectType; virtual; abstract;
    function GetDocObjectType: TdxDocObjectType; override;

    property ActualType: TdxDocObjectType read GetActualType;
  public
    constructor Create(const AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetActualType;

    property Text: string read FText;
  end;

  { TdxExpectedFieldBegin }

  TdxExpectedFieldBegin = class(TdxExpectedDocObject)
  protected
    function GetActualType: TdxDocObjectType; override;
    function GetExpectedType: TdxDocObjectType; override;
  end;

  { TdxExpectedFieldEnd }

  TdxExpectedFieldEnd = class(TdxExpectedDocObject)
  protected
    function GetActualType: TdxDocObjectType; override;
    function GetExpectedType: TdxDocObjectType; override;
  end;

  { TdxExpectedFieldSeparator }

  TdxExpectedFieldSeparator = class(TdxExpectedDocObject)
  protected
    function GetActualType: TdxDocObjectType; override;
    function GetExpectedType: TdxDocObjectType; override;
  end;

  { TdxDocHyperlinkFieldData }

  TdxDocHyperlinkFieldData = class(TdxDocObjectBase)
  strict private
    FHyperlinkInfo: TdxDocHyperlinkInfo;
  protected
    function GetDocObjectType: TdxDocObjectType; override;
  public
    constructor Create(const AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer;
      AHyperlinkInfo: TdxDocHyperlinkInfo);
    destructor Destroy; override;

    property HyperlinkInfo: TdxDocHyperlinkInfo read FHyperlinkInfo;
  end;

  { TdxDocTableCell }

  TdxDocTableCell = class(TdxDocObjectBase)
  protected
    function GetDocObjectType: TdxDocObjectType; override;
  end;

  { TdxDocTableRow }

  TdxDocTableRow = class(TdxDocObjectBase)
  protected
    function GetDocObjectType: TdxDocObjectType; override;
  end;

  { TdxDocFloatingObjectBase }

  TdxDocFloatingObjectBase = class abstract(TdxDocObjectBase)
  strict private
    FFormatting: TdxFloatingObjectFormatting;
    FRotation: Integer;
    FLineWidth: Integer;
    FFillColor: TdxAlphaColor;
    FLineColor: TdxAlphaColor;
    FWrapLeftDistance: Integer;
    FWrapRightDistance: Integer;
    FWrapTopDistance: Integer;
    FWrapBottomDistance: Integer;
    procedure SetFormatting(const Value: TdxFloatingObjectFormatting);
  protected
    function GetApplyBorderByDefault: Boolean; virtual; abstract;
    function GetHorizontalPositionAlignment(AMsoph: TdxDrawingGroupShapePosH.TMsoph): TdxFloatingObjectHorizontalPositionAlignment;
    function GetHorizontalPositionType(AMsoprh: TdxDrawingGroupShapePosRelH.TMsoprh): TdxFloatingObjectHorizontalPositionType;
    function GetVerticalPositionAlignment(AMsopv: TdxDrawingGroupShapePosV.TMsopv): TdxFloatingObjectVerticalPositionAlignment;
    function GetVerticalPositionType(AMsoprv: TdxDrawingGroupShapePosRelV.TMsoprv): TdxFloatingObjectVerticalPositionType;
    function GetRelativeFrom(ARelativeFrom: TdxDrawingGroupShape2SizeRelH.TRelativeFrom): TdxFloatingObjectRelativeFromHorizontal; overload;
    function GetRelativeFrom(ARelativeFrom: TdxDrawingGroupShape2SizeRelV.TRelativeFrom): TdxFloatingObjectRelativeFromVertical; overload;
    procedure SetOfficeArtPropertiesCore(AProperties: TdxOfficeArtProperties; AUnitConverter: TdxDocumentModelUnitConverter); virtual;

    property ApplyBorderByDefault: Boolean read GetApplyBorderByDefault;
  public
    destructor Destroy; override;
    procedure ApplyFileShapeAddress(AAddress: TdxFileShapeAddress); virtual;
    procedure ApplyShapeProperties(AShape: TdxShape);
    procedure SetOfficeArtProperties(AProperties: TdxOfficeArtProperties); virtual;
    procedure SetOfficeArtTertiaryProperties(AProperties: TdxOfficeArtTertiaryProperties); virtual;

    property Formatting: TdxFloatingObjectFormatting read FFormatting write SetFormatting;
    property Rotation: Integer read FRotation write FRotation;
    property LineWidth: Integer read FLineWidth write FLineWidth;
    property FillColor: TdxAlphaColor read FFillColor write FFillColor;
    property LineColor: TdxAlphaColor read FLineColor write FLineColor;
    property WrapLeftDistance: Integer read FWrapLeftDistance write FWrapLeftDistance;
    property WrapRightDistance: Integer read FWrapRightDistance write FWrapRightDistance;
    property WrapTopDistance: Integer read FWrapTopDistance write FWrapTopDistance;
    property WrapBottomDistance: Integer read FWrapBottomDistance write FWrapBottomDistance;
  end;

  { TdxDocPictureFloatingObject }

  TdxDocPictureFloatingObject = class(TdxDocFloatingObjectBase)
  strict private
    FImage: TdxOfficeImageReference;
  protected
    function GetDocObjectType: TdxDocObjectType; override;
    function GetApplyBorderByDefault: Boolean; override;
  public
    constructor Create(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer; ABlip: TdxBlipBase); overload;
    destructor Destroy; override;

    property Image: TdxOfficeImageReference read FImage;
  end;

  { TdxDocTextBoxFloatingObject }

  TdxDocTextBoxFloatingObject = class(TdxDocFloatingObjectBase)
  strict private
    FShapeId: Integer;
    FUseTextTop: Boolean;
    FUseTextBottom: Boolean;
    FUseTextRight: Boolean;
    FUseTextLeft: Boolean;
    FUseFitShapeToText: Boolean;
    FFitShapeToText: Boolean;
    FTextTop: Integer;
    FTextBottom: Integer;
    FTextLeft: Integer;
    FTextRight: Integer;
  protected
    function GetDocObjectType: TdxDocObjectType; override;
    function GetApplyBorderByDefault: Boolean; override;
    procedure SetOfficeArtPropertiesCore(AProperties: TdxOfficeArtProperties; AUnitConverter: TdxDocumentModelUnitConverter); override;
  public
    constructor Create(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer; AShapeId: Integer); overload;
    procedure ApplyTextBoxProperties(AProperties: TdxTextBoxProperties);

    property ShapeId: Integer read FShapeId;
    property UseTextTop: Boolean read FUseTextTop write FUseTextTop;
    property UseTextBottom: Boolean read FUseTextBottom write FUseTextBottom;
    property UseTextRight: Boolean read FUseTextRight write FUseTextRight;
    property UseTextLeft: Boolean read FUseTextLeft write FUseTextLeft;
    property UseFitShapeToText: Boolean read FUseFitShapeToText write FUseFitShapeToText;
    property FitShapeToText: Boolean read FFitShapeToText write FFitShapeToText;
    property TextTop: Integer read FTextTop write FTextTop;
    property TextBottom: Integer read FTextBottom write FTextBottom;
    property TextLeft: Integer read FTextLeft write FTextLeft;
    property TextRight: Integer read FTextRight write FTextRight;
  end;

  { TdxDocFieldsImporter }

  TdxDocFieldsImporter = class
  strict private
    FImportFieldHelper: TdxImportFieldHelper;
    FFieldInfoStack: TdxObjectStack<TdxImportFieldInfo>;
  protected
    property ImportFieldHelper: TdxImportFieldHelper read FImportFieldHelper;
  public
    constructor Create(APieceTable: TdxPieceTable; AFieldInfoStack: TdxObjectStack<TdxImportFieldInfo>);
    destructor Destroy; override;
    procedure ProcessFieldBegin(APosition: TdxInputPosition); virtual;
    procedure ProcessFieldSeparator(APosition: TdxInputPosition); virtual;
    procedure ProcessFieldEnd(APosition: TdxInputPosition; APropertyContainer: TdxDocPropertyContainer); virtual;
    procedure ProcessHyperlinkData(AHyperlinkData: TdxDocHyperlinkFieldData); virtual;

    property ImportFieldStack: TdxObjectStack<TdxImportFieldInfo> read FFieldInfoStack;
  end;

  { TdxDocObjectFactory }

  TdxDocObjectFactory = class
  strict private const
    MapClassTable: array[TdxDocObjectType] of TdxDocObjectBaseClass = (
      nil,                                 // PictureFloatingObject
      nil,                                 // TextBoxFloatingObject
      TdxDocImage,                         // InlineImage
      nil,                                 // ImageCollection
      TdxDocAutoNumberedFootnoteReference, // AutoNumberedFootnoteReference
      TdxDocNoteNumber,                    // NoteNumber
      TdxDocAnnotationReference,           // AnnotationReference
      TdxDocEndnoteReference,              // EndnoteReference
      TdxDocFieldBegin,                    // FieldBegin
      TdxDocFieldSeparator,                // FieldSeparator
      TdxDocFieldEnd,                      // FieldEnd
      nil,                                 // HyperlinkFieldData
      TdxDocTextRun,                       // TextRun
      TdxDocTableCell,                     // TableCell
      TdxDocTableRow,                      // TableRow
      TdxDocParagraph,                     // Paragraph
      TdxDocSection,                       // Section
      TdxExpectedFieldBegin,               // ExpectedFieldBegin
      TdxExpectedFieldSeparator,           // ExpectedFieldSeparator
      TdxExpectedFieldEnd,                 // ExpectedFieldEnd
      nil);                                // UnsupportedObject
    class var
      FInstance: TdxDocObjectFactory;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FObjects: TdxFastObjectList;
    constructor Create;
  public
    destructor Destroy; override;
    function CreateDocObject(AObjectType: TdxDocObjectType; var AObjectInfo: TdxDocObjectInfo;
      APropertyContainer: TdxDocPropertyContainer): IdxDocObject;

    class property Instance: TdxDocObjectFactory read FInstance;
  end;

implementation

uses
  Contnrs,
  dxEncoding, Math, dxTypeHelpers;

procedure WriteZeros(AWriter: TBinaryWriter; ACount: Integer);
var
  I: Integer;
begin
  for I := 0 to ACount - 1 do
    AWriter.Write(Byte(0));
end;

{ TdxDocObjectInfo }

constructor TdxDocObjectInfo.Create(APosition: Integer; const AText: string);
begin
  Position := APosition;
  Text := AText;
end;

{ TdxDocObjectBase }

constructor TdxDocObjectBase.Create(const AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
begin
  FPosition := AObjectInfo.Position;
  FPropertyContainer := APropertyContainer;
end;

function TdxDocObjectBase.GetLength: Integer;
begin
  Result := 1;
end;

function TdxDocObjectBase.GetObject: TObject;
begin
  Result := Self;
end;

function TdxDocObjectBase.GetPosition: Integer;
begin
  Result := FPosition;
end;

function TdxDocObjectBase.GetPropertyContainer: TdxDocPropertyContainer;
begin
  Result := FPropertyContainer;
end;

{ TdxDocTextRun }

constructor TdxDocTextRun.Create(const AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
begin
  inherited Create(AObjectInfo, APropertyContainer);
  FText := AObjectInfo.Text;
end;

function TdxDocTextRun.GetDocObjectType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.TextRun;
end;

function TdxDocTextRun.GetLength: Integer;
begin
  Result := System.Length(FText);
end;

{ TdxDocNoteNumber }

function TdxDocNoteNumber.GetDocObjectType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.NoteNumber;
end;

{ TdxDocAutoNumberedFootnoteReference }

function TdxDocAutoNumberedFootnoteReference.GetDocObjectType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.AutoNumberedFootnoteReference;
end;

{ TdxDocEndnoteReference }

function TdxDocEndnoteReference.GetDocObjectType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.EndnoteReference;
end;

{ TdxDocAnnotationReference }

function TdxDocAnnotationReference.GetDocObjectType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.AnnotationReference;
end;

{ TdxDocParagraph }

function TdxDocParagraph.GetDocObjectType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.Paragraph;
end;

{ TdxDocSection }

function TdxDocSection.GetDocObjectType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.Section;
end;

{ TdxPictureDescriptor }

constructor TdxPictureDescriptor.Create(ARun: TdxInlinePictureRun; AShapeId: Integer; ABlipIndex: Integer);
var
  AShapeContainer: TdxOfficeArtShapeContainer;
begin
  inherited Create;
  FHorizontalScaleFactor := Min(Round(ARun.ScaleX * 10), 32767{Int16.MaxValue});
  FVerticalScaleFactor := Min(Round(ARun.ScaleY * 10), 32767{Int16.MaxValue});
  FInlineShapeContainer := TdxOfficeArtInlineShapeContainer.Create;
  AShapeContainer := FInlineShapeContainer.ShapeContainer;
  AShapeContainer.ShapeRecord.ShapeIdentifier := AShapeId;
  AShapeContainer.ArtProperties.BlipIndex := ABlipIndex;
  AShapeContainer.ArtProperties.ZOrder := ABlipIndex;
  AShapeContainer.ArtProperties.CreateProperties;
  FInlineShapeContainer.Blips.Add(TdxFileBlipStoreEntry.Create(ARun.Image, False));
  CalculateImageSize(ARun);
end;

constructor TdxPictureDescriptor.Create;
begin
  inherited Create;
  FMetafileHeaders := TdxObjectList<TdxDocMetafileHeader>.Create;
  FImages := TdxObjectList<TdxOfficeImageReference>.Create;
end;

destructor TdxPictureDescriptor.Destroy;
begin
  FInlineShapeContainer.Free;
  FTop.Free;
  FLeft.Free;
  FBottom.Free;
  FRight.Free;
  FMetafileHeaders.Free;
  FImages.Free;
  inherited Destroy;
end;

class constructor TdxPictureDescriptor.Initialize;
begin
  FEmptyBorder := TdxBorderDescriptor97.Create;
end;

class destructor TdxPictureDescriptor.Finalize;
begin
  FEmptyBorder.Free;
end;

class function TdxPictureDescriptor.FromStream(AReader: TBinaryReader;
  AOffset: Integer; const AImageCreator: IdxDocOfficeImageCreator): TdxPictureDescriptor;
begin
  Result := TdxPictureDescriptor.Create;
  Result.Read(AReader, AOffset, AImageCreator);
end;

function TdxPictureDescriptor.GetProperties: TdxOfficeArtProperties;
begin
  if FInlineShapeContainer <> nil then
    Result := FInlineShapeContainer.ShapeContainer.ArtProperties
  else
    Result := nil;
end;

procedure TdxPictureDescriptor.CalculateImageSize(ARun: TdxInlinePictureRun);
var
  AImage: TdxOfficeImageReference;
begin
  AImage := ARun.Image;
  if AImage.SizeInTwips.cx <= MaxImageSize then
    FWidth := Word(AImage.SizeInTwips.cx)
  else
  begin
    FWidth := MaxImageSize;
    FHorizontalScaleFactor := Trunc(ARun.ScaleX * (AImage.SizeInTwips.cx / MaxImageSize) * 10);
  end;

  if ARun.Image.SizeInTwips.cy <= MaxImageSize then
    FHeight := Word(AImage.SizeInTwips.cy)
  else
  begin
    FHeight := MaxImageSize;
    FVerticalScaleFactor := Trunc(ARun.ScaleY * (AImage.SizeInTwips.cy / MaxImageSize) * 10);
  end;
end;

procedure TdxPictureDescriptor.Read(AReader: TBinaryReader; AOffset: Integer;
  const AImageCreator: IdxDocOfficeImageCreator);
var
  ASize: Integer;
begin
  if not CheckReader(AReader, AOffset) then
    Exit;
  ASize := AReader.ReadInt32;
  if AReader.ReadSmallInt <> PicturePropertiesSize then
    Exit;
  FDataFormat := TdxPictureDataFormat(AReader.ReadSmallInt);

  ReadCore(AReader, ASize, AImageCreator);
end;

function TdxPictureDescriptor.CheckReader(AReader: TBinaryReader; AOffset: Integer): Boolean;
begin
  Assert(AReader <> nil, 'reader');
  if AReader.BaseStream.Size <= AOffset then
    Exit(False);
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  Result := True;
end;

procedure TdxPictureDescriptor.ReadCore(AReader: TBinaryReader; ASize: Integer; const AImageCreator: IdxDocOfficeImageCreator);
begin
  AReader.BaseStream.Seek(InnerHeaderSize, TSeekOrigin.soCurrent);
  ReadImageSize(AReader);
  if (Width = 0) and (Height = 0) then
    Exit;
  AReader.BaseStream.Seek(ReservedDataSize, TSeekOrigin.soCurrent);
  InitializeBorders(AReader);
  AReader.BaseStream.Seek(ObsoletePropertiesSize, TSeekOrigin.soCurrent);
  Dec(ASize, PicturePropertiesSize);
  if DataFormat = TdxPictureDataFormat.ShapeFile then
    Dec(ASize, ReadPictureName(AReader));
  if (FDataFormat = TdxPictureDataFormat.ShapeFile) or (FDataFormat = TdxPictureDataFormat.ShapeObject) then
    InitializeShapeContainer(AReader, ASize, AImageCreator);
end;

procedure TdxPictureDescriptor.ReadImageSize(AReader: TBinaryReader);
begin
  FWidth := AReader.ReadUInt16;
  FHeight := AReader.ReadUInt16;
  FHorizontalScaleFactor := AReader.ReadUInt16;
  FVerticalScaleFactor := AReader.ReadUInt16;
end;

procedure TdxPictureDescriptor.InitializeBorders(AReader: TBinaryReader);
begin
  FTop := TdxBorderDescriptor97.FromStream(AReader);
  FLeft := TdxBorderDescriptor97.FromStream(AReader);
  FBottom := TdxBorderDescriptor97.FromStream(AReader);
  FRight := TdxBorderDescriptor97.FromStream(AReader);
end;

function TdxPictureDescriptor.ReadPictureName(AReader: TBinaryReader): Integer;
var
  APictureNameLength: Byte;
  ABytes: TBytes;
begin
  Result := 0;
  APictureNameLength := AReader.ReadByte;
  Inc(Result);
  ABytes := AReader.ReadBytes(APictureNameLength);
  FPictureName := TdxEncoding.ASCII.GetString(ABytes, 0, Length(ABytes));
  Inc(Result, APictureNameLength);
end;

procedure TdxPictureDescriptor.InitializeShapeContainer(AReader: TBinaryReader;
  ASize: Integer; const AImageCreator: IdxDocOfficeImageCreator);
var
  ABlips: TdxList<TdxBlipBase>;
  ABlip: TdxBlipBase;
  ACount, I: Integer;
begin
  FInlineShapeContainer.Free;
  FInlineShapeContainer := TdxOfficeArtInlineShapeContainer.FromStream(AReader, ASize, AImageCreator);
  if FInlineShapeContainer.Blips <> nil then
  begin
    ABlips := FInlineShapeContainer.Blips;
    ACount := ABlips.Count;
    for I := 0 to ACount - 1 do
    begin
      ABlip := ABlips[I];
      FImages.Add(ABlip.Image.Clone);
      FMetafileHeaders.Add(ABlip.MetafileHeader.Clone);
    end;
  end;
end;

procedure TdxPictureDescriptor.Write(AWriter: TBinaryWriter);
var
  AStart: Int64;
begin
  Assert(AWriter <> nil);
  AStart := AWriter.BaseStream.Position;
  WriteDefaults(AWriter);
  WriteImageSize(AWriter);
  WriteZeros(AWriter, ReservedDataSize);
  WriteEmptyBorders(AWriter);
  WriteZeros(AWriter, ObsoletePropertiesSize);
  FInlineShapeContainer.Write(AWriter);
  WritePictureDescriptorSize(AWriter, AStart);
end;

procedure TdxPictureDescriptor.WriteDefaults(AWriter: TBinaryWriter);
begin
  WriteZeros(AWriter, 4);
  AWriter.Write(PicturePropertiesSize);
  AWriter.Write(SmallInt(TdxPictureDataFormat.ShapeObject));
  WriteZeros(AWriter, InnerHeaderSize);
end;

procedure TdxPictureDescriptor.WriteImageSize(AWriter: TBinaryWriter);
begin
  AWriter.Write(FWidth);
  AWriter.Write(FHeight);
  AWriter.Write(FHorizontalScaleFactor);
  AWriter.Write(FVerticalScaleFactor);
end;

procedure TdxPictureDescriptor.WritePictureDescriptorSize(AWriter: TBinaryWriter; AStart: Int64);
var
  ASize: Integer;
begin
  ASize := AWriter.BaseStream.Position - AStart;
  AWriter.BaseStream.Seek(AStart, TSeekOrigin.soBeginning);
  AWriter.Write(ASize);
  AWriter.BaseStream.Seek(AStart + ASize, TSeekOrigin.soBeginning);
end;

procedure TdxPictureDescriptor.WriteEmptyBorders(AWriter: TBinaryWriter);
var
  I: Integer;
begin
  for I := 0 to 4 - 1 do
    FEmptyBorder.Write(AWriter);
end;

{ TdxDocImage }

constructor TdxDocImage.Create(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer;
  ADescriptor: TdxPictureDescriptor; APictureIndex: Integer);
begin
  inherited Create(AObjectInfo, APropertyContainer);
  FImage := ADescriptor.Images[APictureIndex].Clone;

  FScaleX := Max(1, Round((ADescriptor.Width * ADescriptor.HorizontalScaleFactor) / (Image.SizeInTwips.cx * 10.0)));
  FScaleY := Max(1, Round((ADescriptor.Height * ADescriptor.VerticalScaleFactor) / (Image.SizeInTwips.cy * 10.0)));



  FMetafileHeader := ADescriptor.MetafileHeaders[APictureIndex];
end;

destructor TdxDocImage.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

function TdxDocImage.GetDocObjectType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.InlineImage;
end;

{ TdxDocImageCollection }

constructor TdxDocImageCollection.Create(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer; ADescriptor: TdxPictureDescriptor);
var
  I: Integer;
  AItem: TdxDocImage;
begin
  inherited Create;
  Capacity := 256;
  FPosition := AObjectInfo.Position;
  FPropertyContainer := APropertyContainer;
  for I := 0 to ADescriptor.Images.Count - 1 do
  begin
    if ADescriptor.Images[I] = nil then
      Continue;
    AItem := TdxDocImage.Create(AObjectInfo, APropertyContainer, ADescriptor, I);
    Add(AItem);
  end;
end;

function TdxDocImageCollection.GetDocObjectType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.ImageCollection;
end;

function TdxDocImageCollection.GetLength: Integer;
begin
  Result := 1;
end;

function TdxDocImageCollection.GetObject: TObject;
begin
  Result := Self;
end;

function TdxDocImageCollection.GetPosition: Integer;
begin
  Result := FPosition;
end;

function TdxDocImageCollection.GetPropertyContainer: TdxDocPropertyContainer;
begin
  Result := FPropertyContainer;
end;

function TdxDocImageCollection.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TdxDocImageCollection._AddRef: Integer;
begin
  Result := -1;
end;

function TdxDocImageCollection._Release: Integer;
begin
  Result := -1;
end;

{ TdxDocFieldBegin }

function TdxDocFieldBegin.GetDocObjectType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.FieldBegin;
end;

{ TdxDocFieldEnd }

function TdxDocFieldEnd.GetDocObjectType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.FieldEnd;
end;

{ TdxDocFieldSeparator }

constructor TdxDocFieldSeparator.Create(const AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
begin
  inherited Create(AObjectInfo, APropertyContainer);
  if APropertyContainer.CharacterInfo <> nil then
    FOleObject := APropertyContainer.CharacterInfo.EmbeddedObject or APropertyContainer.CharacterInfo.Ole2Object;
end;

function TdxDocFieldSeparator.GetDocObjectType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.FieldSeparator;
end;

{ TdxExpectedDocObject }

constructor TdxExpectedDocObject.Create(const AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer);
begin
  inherited Create(AObjectInfo, APropertyContainer);
  FType := GetExpectedType;
  FText := AObjectInfo.Text;
end;

function TdxExpectedDocObject.GetDocObjectType: TdxDocObjectType;
begin
  Result := FType;
end;

procedure TdxExpectedDocObject.SetActualType;
begin
  FType := ActualType;
end;

{ TdxExpectedFieldBegin }

function TdxExpectedFieldBegin.GetActualType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.FieldBegin;
end;

function TdxExpectedFieldBegin.GetExpectedType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.ExpectedFieldBegin;
end;

{ TdxExpectedFieldEnd }

function TdxExpectedFieldEnd.GetActualType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.FieldEnd;
end;

function TdxExpectedFieldEnd.GetExpectedType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.ExpectedFieldEnd;
end;

{ TdxExpectedFieldSeparator }

function TdxExpectedFieldSeparator.GetActualType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.FieldSeparator;
end;

function TdxExpectedFieldSeparator.GetExpectedType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.ExpectedFieldSeparator;
end;

{ TdxDocHyperlinkFieldData }

constructor TdxDocHyperlinkFieldData.Create(const AObjectInfo: TdxDocObjectInfo;
  APropertyContainer: TdxDocPropertyContainer; AHyperlinkInfo: TdxDocHyperlinkInfo);
begin
  inherited Create(AObjectInfo, APropertyContainer);
  FHyperlinkInfo := AHyperlinkInfo;
end;

destructor TdxDocHyperlinkFieldData.Destroy;
begin
  FreeAndNil(FHyperlinkInfo);
  inherited Destroy;
end;

function TdxDocHyperlinkFieldData.GetDocObjectType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.HyperlinkFieldData;
end;

{ TdxDocTableCell }

function TdxDocTableCell.GetDocObjectType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.TableCell;
end;

{ TdxDocTableRow }

function TdxDocTableRow.GetDocObjectType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.TableRow;
end;

{ TdxDocFloatingObjectBase }

destructor TdxDocFloatingObjectBase.Destroy;
begin
  FreeAndNil(FFormatting);
  inherited Destroy;
end;

procedure TdxDocFloatingObjectBase.ApplyFileShapeAddress(AAddress: TdxFileShapeAddress);
var
  X, Y, AWidth, AHeight: Integer;
begin
  Formatting.Locked := AAddress.Locked;
  X := PropertyContainer.UnitConverter.TwipsToModelUnits(AAddress.Left);
  Y := PropertyContainer.UnitConverter.TwipsToModelUnits(AAddress.Top);
  AWidth := PropertyContainer.UnitConverter.TwipsToModelUnits(AAddress.Right) - X;
  AHeight := PropertyContainer.UnitConverter.TwipsToModelUnits(AAddress.Bottom) - Y;

  if X < MinInt / 2 then
    X := 0;
  if Y < MinInt / 2 then
    Y := 0;
  Formatting.Offset := TPoint.Create(X, Y);
  Formatting.ActualSize := TSize.Create(AWidth, AHeight);
  Formatting.HorizontalPositionType := AAddress.HorisontalPositionType;
  Formatting.VerticalPositionType := AAddress.VericalPositionType;
  Formatting.TextWrapSide := AAddress.TextWrapSide;
  Formatting.TextWrapType := AAddress.TextWrapType;
  if AAddress.UseIsBehindDoc then
    Formatting.IsBehindDoc := AAddress.IsBehindDoc;
end;

procedure TdxDocFloatingObjectBase.ApplyShapeProperties(AShape: TdxShape);
begin
  if AShape.Rotation <> Rotation then
    AShape.Rotation := Rotation;
  if AShape.OutlineWidth <> LineWidth then
    AShape.OutlineWidth := LineWidth;
  if AShape.OutlineColor <> LineColor then
    AShape.OutlineColor := LineColor;
  if AShape.FillColor <> FillColor then
    AShape.FillColor := FillColor;
end;

procedure TdxDocFloatingObjectBase.SetOfficeArtProperties(AProperties: TdxOfficeArtProperties);
var
  AUnitConverter: TdxDocumentModelUnitConverter;
begin
  AUnitConverter := PropertyContainer.UnitConverter;
  SetOfficeArtPropertiesCore(AProperties, AUnitConverter);
end;

procedure TdxDocFloatingObjectBase.SetOfficeArtTertiaryProperties(AProperties: TdxOfficeArtTertiaryProperties);
var
  AActualHorizPos, AActualVertPos: Integer;
  APt: TPoint;
begin
  if AProperties.UseRelativeWidth then
    Formatting.RelativeWidth := TdxFloatingObjectRelativeWidth.Create(GetRelativeFrom(AProperties.SizeRelH), AProperties.PctHoriz * 100);
  if AProperties.UseRelativeHeight then
    Formatting.RelativeHeight := TdxFloatingObjectRelativeHeight.Create(GetRelativeFrom(AProperties.SizeRelV), AProperties.PctVert * 100);
  AActualHorizPos := 0;
  AActualVertPos := 0;
  if AProperties.PctHorizPosValid or AProperties.PctVertPosValid then
  begin
    if AProperties.PctHorizPosValid then
      AActualHorizPos := AProperties.PctHorizPos * 100
    else
      AActualHorizPos := 0;
    if AProperties.PctVertPosValid then
      AActualVertPos := AProperties.PctVertPos * 100
    else
      AActualVertPos := 0;
  end;

  if AProperties.UsePosH then
  begin
    Formatting.HorizontalPositionAlignment := GetHorizontalPositionAlignment(AProperties.PosH);
    Formatting.HorizontalPositionType := GetHorizontalPositionType(AProperties.PosRelH);
    APt := Formatting.PercentOffset;
    APt.X := AActualHorizPos;
    Formatting.PercentOffset := APt;
  end;
  if AProperties.UsePosV then
  begin
    Formatting.VerticalPositionAlignment := GetVerticalPositionAlignment(AProperties.PosV);
    Formatting.VerticalPositionType := GetVerticalPositionType(AProperties.PosRelV);
    APt := Formatting.PercentOffset;
    APt.Y := AActualVertPos;
    Formatting.PercentOffset := APt;
  end;

  if AProperties.UseLayoutInCell then
    Formatting.LayoutInTableCell := AProperties.LayoutInCell
  else
    Formatting.LayoutInTableCell := True;
end;

function TdxDocFloatingObjectBase.GetHorizontalPositionAlignment(AMsoph: TdxDrawingGroupShapePosH.TMsoph): TdxFloatingObjectHorizontalPositionAlignment;
begin
  case AMsoph of
    TdxDrawingGroupShapePosH.TMsoph.msophAbs:
      Result := TdxFloatingObjectHorizontalPositionAlignment.None;
    TdxDrawingGroupShapePosH.TMsoph.msophCenter:
      Result := TdxFloatingObjectHorizontalPositionAlignment.Center;
    TdxDrawingGroupShapePosH.TMsoph.msophInside:
      Result := TdxFloatingObjectHorizontalPositionAlignment.Inside;
    TdxDrawingGroupShapePosH.TMsoph.msophLeft:
      Result := TdxFloatingObjectHorizontalPositionAlignment.Left;
    TdxDrawingGroupShapePosH.TMsoph.msophOutside:
      Result := TdxFloatingObjectHorizontalPositionAlignment.Outside;
    TdxDrawingGroupShapePosH.TMsoph.msophRight:
      Result := TdxFloatingObjectHorizontalPositionAlignment.Right;
    else
      Result := TdxFloatingObjectHorizontalPositionAlignment.None;
  end;
end;

function TdxDocFloatingObjectBase.GetHorizontalPositionType(AMsoprh: TdxDrawingGroupShapePosRelH.TMsoprh): TdxFloatingObjectHorizontalPositionType;
begin
  case AMsoprh of
    TdxDrawingGroupShapePosRelH.TMsoprh.msoprhChar:
      Result := TdxFloatingObjectHorizontalPositionType.Character;
    TdxDrawingGroupShapePosRelH.TMsoprh.msoprhMargin:
      Result := TdxFloatingObjectHorizontalPositionType.Margin;
    TdxDrawingGroupShapePosRelH.TMsoprh.msoprhPage:
      Result := TdxFloatingObjectHorizontalPositionType.Page;
    TdxDrawingGroupShapePosRelH.TMsoprh.msoprhText:
      Result := TdxFloatingObjectHorizontalPositionType.Column;
    else
      Result := TdxFloatingObjectHorizontalPositionType.Column;
  end;
end;

function TdxDocFloatingObjectBase.GetVerticalPositionAlignment(AMsopv: TdxDrawingGroupShapePosV.TMsopv): TdxFloatingObjectVerticalPositionAlignment;
begin
  case AMsopv of
    TdxDrawingGroupShapePosV.TMsopv.msopvAbs:
      Result := TdxFloatingObjectVerticalPositionAlignment.None;
    TdxDrawingGroupShapePosV.TMsopv.msopvBottom:
      Result := TdxFloatingObjectVerticalPositionAlignment.Bottom;
    TdxDrawingGroupShapePosV.TMsopv.msopvCenter:
      Result := TdxFloatingObjectVerticalPositionAlignment.Center;
    TdxDrawingGroupShapePosV.TMsopv.msopvInside:
      Result := TdxFloatingObjectVerticalPositionAlignment.Inside;
    TdxDrawingGroupShapePosV.TMsopv.msopvOutside:
      Result := TdxFloatingObjectVerticalPositionAlignment.Outside;
    TdxDrawingGroupShapePosV.TMsopv.msopvTop:
      Result := TdxFloatingObjectVerticalPositionAlignment.Top;
    else
      Result := TdxFloatingObjectVerticalPositionAlignment.None;
  end;
end;

function TdxDocFloatingObjectBase.GetVerticalPositionType(AMsoprv: TdxDrawingGroupShapePosRelV.TMsoprv): TdxFloatingObjectVerticalPositionType;
begin
  case AMsoprv of
    TdxDrawingGroupShapePosRelV.TMsoprv.msoprvLine:
      Result := TdxFloatingObjectVerticalPositionType.Line;
    TdxDrawingGroupShapePosRelV.TMsoprv.msoprvMargin:
      Result := TdxFloatingObjectVerticalPositionType.Margin;
    TdxDrawingGroupShapePosRelV.TMsoprv.msoprvPage:
      Result := TdxFloatingObjectVerticalPositionType.Page;
    TdxDrawingGroupShapePosRelV.TMsoprv.msoprvText:
      Result := TdxFloatingObjectVerticalPositionType.Paragraph;
    else
      Result := TdxFloatingObjectVerticalPositionType.Paragraph;
  end;
end;

function TdxDocFloatingObjectBase.GetRelativeFrom(ARelativeFrom: TdxDrawingGroupShape2SizeRelH.TRelativeFrom): TdxFloatingObjectRelativeFromHorizontal;
begin
  Result := TdxFloatingObjectRelativeFromHorizontal(ARelativeFrom);
end;

function TdxDocFloatingObjectBase.GetRelativeFrom(ARelativeFrom: TdxDrawingGroupShape2SizeRelV.TRelativeFrom): TdxFloatingObjectRelativeFromVertical;
begin
  Result := TdxFloatingObjectRelativeFromVertical(ARelativeFrom);
end;

procedure TdxDocFloatingObjectBase.SetOfficeArtPropertiesCore(AProperties: TdxOfficeArtProperties;
  AUnitConverter: TdxDocumentModelUnitConverter);
var
  ARotation, ALineWidth: Integer;
begin
  if AProperties.ZOrder <> 0 then
    Formatting.ZOrder := AProperties.ZOrder;
  if AProperties.UseIsBehindDoc then
    Formatting.IsBehindDoc := AProperties.IsBehindDoc;

  ARotation := AUnitConverter.DegreeToModelUnits(Trunc(AProperties.Rotation));
  if ARotation <> 0 then
    Rotation := ARotation;
  if (ApplyBorderByDefault and not AProperties.UseLine) or (AProperties.UseLine and AProperties.Line) then
  begin
    LineWidth := AUnitConverter.EmuToModelUnits(TdxOfficeArtConstants.DefaultLineWidthInEmus);
    LineColor := TdxAlphaColors.Black;
  end;
  if AProperties.UseFilled then
    if AProperties.Filled then
      FillColor := TdxAlphaColors.White
    else
      FillColor := TdxAlphaColors.Empty;

  ALineWidth := AUnitConverter.EmuToModelUnits(Trunc(AProperties.LineWidth));
  if ALineWidth <> 0 then
    LineWidth := ALineWidth;
  Formatting.LeftDistance := AUnitConverter.EmuToModelUnits(Trunc(IfThen(AProperties.UseWrapLeftDistance, AProperties.WrapLeftDistance, TdxDrawingWrapLeftDistance.DefaultValue)));
  Formatting.RightDistance := AUnitConverter.EmuToModelUnits(Trunc(IfThen(AProperties.UseWrapRightDistance, AProperties.WrapRightDistance, TdxDrawingWrapRightDistance.DefaultValue)));
  Formatting.TopDistance := AUnitConverter.EmuToModelUnits(Trunc(IfThen(AProperties.UseWrapTopDistance, AProperties.WrapTopDistance, TdxDrawingWrapTopDistance.DefaultValue)));
  Formatting.BottomDistance := AUnitConverter.EmuToModelUnits(Trunc(IfThen(AProperties.UseWrapBottomDistance, AProperties.WrapBottomDistance, TdxDrawingWrapBottomDistance.DefaultValue)));

  if not TdxAlphaColors.IsEmpty(AProperties.LineColor) then
    LineColor := AProperties.LineColor;
  if not TdxAlphaColors.IsEmpty(AProperties.FillColor) and AProperties.UseFilled and AProperties.Filled then
    FillColor := AProperties.FillColor;
end;

procedure TdxDocFloatingObjectBase.SetFormatting(const Value: TdxFloatingObjectFormatting);
begin
  if FFormatting = Value then
    Exit;
  FFormatting.Free;
  FFormatting := Value;
end;

{ TdxDocPictureFloatingObject }

constructor TdxDocPictureFloatingObject.Create(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer;
   ABlip: TdxBlipBase);
begin
  inherited Create(AObjectInfo, APropertyContainer);
  FImage := ABlip.Image.Clone;
end;

destructor TdxDocPictureFloatingObject.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

function TdxDocPictureFloatingObject.GetDocObjectType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.PictureFloatingObject;
end;

function TdxDocPictureFloatingObject.GetApplyBorderByDefault: Boolean;
begin
  Result := False;
end;

{ TdxDocTextBoxFloatingObject }

constructor TdxDocTextBoxFloatingObject.Create(var AObjectInfo: TdxDocObjectInfo;
  APropertyContainer: TdxDocPropertyContainer; AShapeId: Integer);
begin
  inherited Create(AObjectInfo, APropertyContainer);
  FShapeId := AShapeId;
end;

function TdxDocTextBoxFloatingObject.GetDocObjectType: TdxDocObjectType;
begin
  Result := TdxDocObjectType.TextBoxFloatingObject;
end;

function TdxDocTextBoxFloatingObject.GetApplyBorderByDefault: Boolean;
begin
  Result := True;
end;

procedure TdxDocTextBoxFloatingObject.SetOfficeArtPropertiesCore(AProperties: TdxOfficeArtProperties;
  AUnitConverter: TdxDocumentModelUnitConverter);
begin
  inherited SetOfficeArtPropertiesCore(AProperties, AUnitConverter);
  if AProperties.UseTextTop then
  begin
    UseTextTop := True;
    TextTop := AUnitConverter.EmuToModelUnits(AProperties.TextTop);
  end;
  if AProperties.UseTextBottom then
  begin
    UseTextBottom := True;
    TextBottom := AUnitConverter.EmuToModelUnits(AProperties.TextBottom);
  end;
  if AProperties.UseTextLeft then
  begin
    UseTextLeft := True;
    TextLeft := AUnitConverter.EmuToModelUnits(AProperties.TextLeft);
  end;
  if AProperties.UseTextRight then
  begin
    UseTextRight := True;
    TextRight := AUnitConverter.EmuToModelUnits(AProperties.TextRight);
  end;
  if AProperties.UseFitShapeToText then
  begin
    UseFitShapeToText := True;
    FitShapeToText := AProperties.FitShapeToText;
  end;
end;

procedure TdxDocTextBoxFloatingObject.ApplyTextBoxProperties(AProperties: TdxTextBoxProperties);
begin
  if UseTextTop then
    AProperties.TopMargin := TextTop;
  if UseTextBottom then
    AProperties.BottomMargin := TextBottom;
  if UseTextLeft then
    AProperties.LeftMargin := TextLeft;
  if UseTextRight then
    AProperties.RightMargin := TextRight;
  if UseFitShapeToText then
    AProperties.ResizeShapeToFitText := FitShapeToText;
end;

{ TdxDocFieldsImporter }

constructor TdxDocFieldsImporter.Create(APieceTable: TdxPieceTable; AFieldInfoStack: TdxObjectStack<TdxImportFieldInfo>);
begin
  FImportFieldHelper := TdxImportFieldHelper.Create(APieceTable);
  FFieldInfoStack := AFieldInfoStack;
end;

destructor TdxDocFieldsImporter.Destroy;
begin
  FImportFieldHelper.Free;
  inherited Destroy;
end;

procedure TdxDocFieldsImporter.ProcessFieldBegin(APosition: TdxInputPosition);
var
  AFieldInfo: TdxImportFieldInfo;
begin
  AFieldInfo := TdxImportFieldInfo.Create(ImportFieldHelper.PieceTable);
  ImportFieldHelper.ProcessFieldBegin(AFieldInfo, APosition);
  ImportFieldStack.Push(AFieldInfo);
end;

procedure TdxDocFieldsImporter.ProcessFieldSeparator(APosition: TdxInputPosition);
var
  AFieldInfo: TdxImportFieldInfo;
begin
  if ImportFieldStack.Count = 0 then
    Exit;
  AFieldInfo := ImportFieldStack.Peek;
  ImportFieldHelper.ProcessFieldSeparator(AFieldInfo, APosition);
end;

procedure TdxDocFieldsImporter.ProcessFieldEnd(APosition: TdxInputPosition; APropertyContainer: TdxDocPropertyContainer);
var
  AFieldInfo: TdxImportFieldInfo;
begin
  if ImportFieldStack.Count = 0 then
    Exit;
  AFieldInfo := ImportFieldStack.Extract;
  try
    AFieldInfo.Locked := (APropertyContainer.FieldProperties and TdxFieldProperties.Locked) <> 0;
    FImportFieldHelper.ProcessFieldEnd(AFieldInfo, APosition);
    if ImportFieldStack.Count > 0 then
      AFieldInfo.Field.Parent := ImportFieldStack.Peek.Field;
  finally
    AFieldInfo.Free;
  end;
end;

procedure TdxDocFieldsImporter.ProcessHyperlinkData(AHyperlinkData: TdxDocHyperlinkFieldData);
begin
end;

{ TdxDocObjectFactory }

constructor TdxDocObjectFactory.Create;
begin
  FObjects := TdxFastObjectList.Create(True, 1024 * 16);
end;

destructor TdxDocObjectFactory.Destroy;
begin
  FObjects.Free;
  inherited Destroy;
end;

class constructor TdxDocObjectFactory.Initialize;
begin
  FInstance := TdxDocObjectFactory.Create;
end;

class destructor TdxDocObjectFactory.Finalize;
begin
  FInstance.Free;
end;

function TdxDocObjectFactory.CreateDocObject(AObjectType: TdxDocObjectType;
  var AObjectInfo: TdxDocObjectInfo;
  APropertyContainer: TdxDocPropertyContainer): IdxDocObject;
var
  AClass: TdxDocObjectBaseClass;
begin
  AClass := MapClassTable[AObjectType];
  if AClass = nil then
    Exit(nil);
  Result := AClass.Create(AObjectInfo, APropertyContainer);
end;

end.
