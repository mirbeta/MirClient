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

unit dxRichEdit.Export.OpenXML;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, SysUtils, Classes, StrUtils, Graphics, Generics.Defaults, Generics.Collections, ZLIB,
  dxCore, dxCoreClasses, dxCoreGraphics, dxZipUtils, dxStringHelper,

  dxRichEdit.NativeApi,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.Utils.OfficeImage,
  dxXMLWriter,
  dxRichEdit.Platform.Font,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Exporter,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.DocumentModel.NotesRange,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.ShapeFormatting,
  dxRichEdit.DocumentModel.DocumentProperties,
  dxRichEdit.DocumentFormats.DocumentFormatUtils,
  dxRichEdit.Export.Formats,
  dxRichEdit.Export.Core,
  dxRichEdit.Export.OpenXML.WordProcessingMLBaseExporter;

type

  { IdxOpenXMLDrawingObject }

  IdxOpenXMLDrawingObject = interface
  ['{907CF3B3-37FC-4B42-9EA2-35E3B208CA8F}']
    function GetIsFloatingObject: Boolean;
    function GetName: string;
    function GetActualSize: TSize;
    function GetRotation: Integer;
    function GetLockAspectRatio: Boolean;
    function GetAllowOverlap: Boolean;
    function GetIsBehindDoc: Boolean;
    function GetLayoutInTableCell: Boolean;
    function GetLocked: Boolean;
    function GetZOrder: Integer;
    function GetUseBottomDistance: Boolean;
    function GetBottomDistance: Integer;
    function GetUseLeftDistance: Boolean;
    function GetLeftDistance: Integer;
    function GetUseRightDistance: Boolean;
    function GetRightDistance: Integer;
    function GetUseTopDistance: Boolean;
    function GetTopDistance: Integer;
    function GetUseHidden: Boolean;
    function GetHidden: Boolean;
    function GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
    function GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
    function GetUsePercentOffset: Boolean;
    function GetPercentOffsetX: Integer;
    function GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
    function GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
    function GetPercentOffsetY: Integer;
    function GetUseLockAspectRatio: Boolean;
    function GetUseRotation: Boolean;
    function GetTextWrapType: TdxFloatingObjectTextWrapType;
    function GetTextWrapSide: TdxFloatingObjectTextWrapSide;
    function GetShape: TdxShape;
    function GetUseRelativeWidth: Boolean;
    function GetUseRelativeHeight: Boolean;
    function GetRelativeWidth: TdxFloatingObjectRelativeWidth;
    function GetRelativeHeight: TdxFloatingObjectRelativeHeight;
    function GetOffset: TPoint;
    function GetPercentOffset: TPoint;

    property IsFloatingObject: Boolean read GetIsFloatingObject;
    property Name: string read GetName;
    property ActualSize: TSize read GetActualSize;
    property Rotation: Integer read GetRotation;
    property LockAspectRatio: Boolean read GetLockAspectRatio;
    property AllowOverlap: Boolean read GetAllowOverlap;
    property IsBehindDoc: Boolean read GetIsBehindDoc;
    property LayoutInTableCell: Boolean read GetLayoutInTableCell;
    property Locked: Boolean read GetLocked;
    property ZOrder: Integer read GetZOrder;
    property UseBottomDistance: Boolean read GetUseBottomDistance;
    property BottomDistance: Integer read GetBottomDistance;
    property UseLeftDistance: Boolean read GetUseLeftDistance;
    property LeftDistance: Integer read GetLeftDistance;
    property UseRightDistance: Boolean read GetUseRightDistance;
    property RightDistance: Integer read GetRightDistance;
    property UseTopDistance: Boolean read GetUseTopDistance;
    property TopDistance: Integer read GetTopDistance;
    property UseHidden: Boolean read GetUseHidden;
    property Hidden: Boolean read GetHidden;
    property HorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment read GetHorizontalPositionAlignment;
    property HorizontalPositionType: TdxFloatingObjectHorizontalPositionType read GetHorizontalPositionType;
    property UsePercentOffset: Boolean read GetUsePercentOffset;
    property PercentOffsetX: Integer read GetPercentOffsetX;
    property VerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment read GetVerticalPositionAlignment;
    property VerticalPositionType: TdxFloatingObjectVerticalPositionType read GetVerticalPositionType;
    property PercentOffsetY: Integer read GetPercentOffsetY;
    property UseLockAspectRatio: Boolean read GetUseLockAspectRatio;
    property UseRotation: Boolean read GetUseRotation;
    property TextWrapType: TdxFloatingObjectTextWrapType read GetTextWrapType;
    property TextWrapSide: TdxFloatingObjectTextWrapSide read GetTextWrapSide;
    property Shape: TdxShape read GetShape;
    property UseRelativeWidth: Boolean read GetUseRelativeWidth;
    property UseRelativeHeight: Boolean read GetUseRelativeHeight;
    property RelativeWidth: TdxFloatingObjectRelativeWidth read GetRelativeWidth;
    property RelativeHeight: TdxFloatingObjectRelativeHeight read GetRelativeHeight;
    property Offset: TPoint read GetOffset;
    property PercentOffset: TPoint read GetPercentOffset;
  end;

  { IdxOpenXMLDrawingObjectContent }

  IdxOpenXMLDrawingObjectContent = interface
  ['{060297B4-0B7B-4D8C-9901-29ADFCEA7B06}']
    function GetImage: TdxOfficeImageReference;

    property Image: TdxOfficeImageReference read GetImage;
  end;

  { TdxInlineDrawingObject }

  TdxInlineDrawingObject = class(TInterfacedObject, IdxOpenXMLDrawingObject)
  strict private
    FRun: TdxInlinePictureRun;
    function GetActualSize: TSize;
    function GetAllowOverlap: Boolean;
    function GetBottomDistance: Integer;
    function GetFillColor: TdxAlphaColor;
    function GetHidden: Boolean;
    function GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
    function GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
    function GetIsBehindDoc: Boolean;
    function GetIsFloatingObject: Boolean;
    function GetLayoutInTableCell: Boolean;
    function GetLeftDistance: Integer;
    function GetLockAspectRatio: Boolean;
    function GetLocked: Boolean;
    function GetName: string;
    function GetOffset: TPoint;
    function GetPercentOffset: TPoint;
    function GetPercentOffsetX: Integer;
    function GetPercentOffsetY: Integer;
    function GetRelativeHeight: TdxFloatingObjectRelativeHeight;
    function GetRelativeWidth: TdxFloatingObjectRelativeWidth;
    function GetRightDistance: Integer;
    function GetRotation: Integer;
    function GetShape: TdxShape;
    function GetTextWrapSide: TdxFloatingObjectTextWrapSide;
    function GetTextWrapType: TdxFloatingObjectTextWrapType;
    function GetTopDistance: Integer;
    function GetUseBottomDistance: Boolean;
    function GetUseHidden: Boolean;
    function GetUseLeftDistance: Boolean;
    function GetUseLockAspectRatio: Boolean;
    function GetUsePercentOffset: Boolean;
    function GetUseRelativeHeight: Boolean;
    function GetUseRelativeWidth: Boolean;
    function GetUseRightDistance: Boolean;
    function GetUseRotation: Boolean;
    function GetUseTopDistance: Boolean;
    function GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
    function GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
    function GetZOrder: Integer;
  public
    constructor Create(ARun: TdxInlinePictureRun);

    property ActualSize: TSize read GetActualSize;
    property AllowOverlap: Boolean read GetAllowOverlap;
    property BottomDistance: Integer read GetBottomDistance;
    property FillColor: TdxAlphaColor read GetFillColor;
    property Hidden: Boolean read GetHidden;
    property HorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment read GetHorizontalPositionAlignment;
    property HorizontalPositionType: TdxFloatingObjectHorizontalPositionType read GetHorizontalPositionType;
    property IsBehindDoc: Boolean read GetIsBehindDoc;
    property IsFloatingObject: Boolean read GetIsFloatingObject;
    property LayoutInTableCell: Boolean read GetLayoutInTableCell;
    property LeftDistance: Integer read GetLeftDistance;
    property LockAspectRatio: Boolean read GetLockAspectRatio;
    property Locked: Boolean read GetLocked;
    property Name: string read GetName;
    property Offset: TPoint read GetOffset;
    property PercentOffset: TPoint read GetPercentOffset;
    property PercentOffsetX: Integer read GetPercentOffsetX;
    property PercentOffsetY: Integer read GetPercentOffsetY;
    property RelativeHeight: TdxFloatingObjectRelativeHeight read GetRelativeHeight;
    property RelativeWidth: TdxFloatingObjectRelativeWidth read GetRelativeWidth;
    property RightDistance: Integer read GetRightDistance;
    property Rotation: Integer read GetRotation;
    property Shape: TdxShape read GetShape;
    property TextWrapSide: TdxFloatingObjectTextWrapSide read GetTextWrapSide;
    property TextWrapType: TdxFloatingObjectTextWrapType read GetTextWrapType;
    property TopDistance: Integer read GetTopDistance;
    property UseBottomDistance: Boolean read GetUseBottomDistance;
    property UseHidden: Boolean read GetUseHidden;
    property UseLeftDistance: Boolean read GetUseLeftDistance;
    property UseLockAspectRatio: Boolean read GetUseLockAspectRatio;
    property UsePercentOffset: Boolean read GetUsePercentOffset;
    property UseRelativeHeight: Boolean read GetUseRelativeHeight;
    property UseRelativeWidth: Boolean read GetUseRelativeWidth;
    property UseRightDistance: Boolean read GetUseRightDistance;
    property UseRotation: Boolean read GetUseRotation;
    property UseTopDistance: Boolean read GetUseTopDistance;
    property VerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment read GetVerticalPositionAlignment;
    property VerticalPositionType: TdxFloatingObjectVerticalPositionType read GetVerticalPositionType;
    property ZOrder: Integer read GetZOrder;
  end;

  { TdxInlineDrawingObjectContent }

  TdxInlineDrawingObjectContent = class(TInterfacedObject, IdxOpenXMLDrawingObjectContent)
  strict private
    FImage: TdxOfficeImageReference;
  private
    function GetImage: TdxOfficeImageReference;
  public
    constructor Create(AImage: TdxOfficeImageReference);

    property Image: TdxOfficeImageReference read GetImage;
  end;

  { TdxFloatingObjectDrawingObject }

  TdxFloatingObjectDrawingObject = class(TInterfacedObject, IdxOpenXMLDrawingObject)
  strict private
    FRun: TdxFloatingObjectAnchorRun;
    function GetName: string;
    function GetActualSize: TSize;
    function GetRotation: Integer;
    function GetLockAspectRatio: Boolean;
    function GetAllowOverlap: Boolean;
    function GetIsBehindDoc: Boolean;
    function GetIsFloatingObject: Boolean;
    function GetLayoutInTableCell: Boolean;
    function GetLocked: Boolean;
    function GetZOrder: Integer;
    function GetUseBottomDistance: Boolean;
    function GetBottomDistance: Integer;
    function GetUseLeftDistance: Boolean;
    function GetLeftDistance: Integer;
    function GetUseRightDistance: Boolean;
    function GetRightDistance: Integer;
    function GetUseTopDistance: Boolean;
    function GetTopDistance: Integer;
    function GetUseHidden: Boolean;
    function GetHidden: Boolean;
    function GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
    function GetUsePercentOffset: Boolean;
    function GetPercentOffsetX: Integer;
    function GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
    function GetPercentOffsetY: Integer;
    function GetUseLockAspectRatio: Boolean;
    function GetUseRotation: Boolean;
    function GetTextWrapType: TdxFloatingObjectTextWrapType;
    function GetTextWrapSide: TdxFloatingObjectTextWrapSide;
    function GetShape: TdxShape;
    function GetUseRelativeWidth: Boolean;
    function GetUseRelativeHeight: Boolean;
    function GetRelativeWidth: TdxFloatingObjectRelativeWidth;
    function GetRelativeHeight: TdxFloatingObjectRelativeHeight;
    function GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
    function GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
    function GetOffset: TPoint;
    function GetPercentOffset: TPoint;
  public
    constructor Create(ARun: TdxFloatingObjectAnchorRun);

    property IsFloatingObject: Boolean read GetIsFloatingObject;
    property Name: string read GetName;
    property ActualSize: TSize read GetActualSize;
    property Rotation: Integer read GetRotation;
    property LockAspectRatio: Boolean read GetLockAspectRatio;
    property AllowOverlap: Boolean read GetAllowOverlap;
    property IsBehindDoc: Boolean read GetIsBehindDoc;
    property LayoutInTableCell: Boolean read GetLayoutInTableCell;
    property Locked: Boolean read GetLocked;
    property ZOrder: Integer read GetZOrder;
    property UseBottomDistance: Boolean read GetUseBottomDistance;
    property BottomDistance: Integer read GetBottomDistance;
    property UseLeftDistance: Boolean read GetUseLeftDistance;
    property LeftDistance: Integer read GetLeftDistance;
    property UseRightDistance: Boolean read GetUseRightDistance;
    property RightDistance: Integer read GetRightDistance;
    property UseTopDistance: Boolean read GetUseTopDistance;
    property TopDistance: Integer read GetTopDistance;
    property UseHidden: Boolean read GetUseHidden;
    property Hidden: Boolean read GetHidden;
    property HorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment read GetHorizontalPositionAlignment;
    property UsePercentOffset: Boolean read GetUsePercentOffset;
    property PercentOffsetX: Integer read GetPercentOffsetX;
    property VerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment read GetVerticalPositionAlignment;
    property PercentOffsetY: Integer read GetPercentOffsetY;
    property UseLockAspectRatio: Boolean read GetUseLockAspectRatio;
    property UseRotation: Boolean read GetUseRotation;
    property TextWrapType: TdxFloatingObjectTextWrapType read GetTextWrapType;
    property TextWrapSide: TdxFloatingObjectTextWrapSide read GetTextWrapSide;
    property Shape: TdxShape read GetShape;
    property UseRelativeWidth: Boolean read GetUseRelativeWidth;
    property UseRelativeHeight: Boolean read GetUseRelativeHeight;
    property RelativeWidth: TdxFloatingObjectRelativeWidth read GetRelativeWidth;
    property RelativeHeight: TdxFloatingObjectRelativeHeight read GetRelativeHeight;
    property HorizontalPositionType: TdxFloatingObjectHorizontalPositionType read GetHorizontalPositionType;
    property VerticalPositionType: TdxFloatingObjectVerticalPositionType read GetVerticalPositionType;
    property Offset: TPoint read GetOffset;
    property PercentOffset: TPoint read GetPercentOffset;
  end;

  { TdxFloatingObjectDrawingObjectContent }

  TdxFloatingObjectDrawingObjectContent = class(TInterfacedObject, IdxOpenXMLDrawingObjectContent)
  strict private
    FContent: TdxPictureFloatingObjectContent;
    function GetImage: TdxOfficeImageReference;
  public
    constructor Create(AContent: TdxPictureFloatingObjectContent);

    property Image: TdxOfficeImageReference read GetImage;
  end;

  { TdxPairStringList }

  TdxPairString = TPair<string, string>;
  TdxPairStringList = class(TList<TdxPairString>)
  private
    function GetItems(const AKey: string): string; inline;
  public
    procedure Add(const AKey, AValue: string); overload;
    function TryGetValue(const AKey: string; out AValue: string): Boolean;
    property Items[const AKey: string]: string read GetItems; default;
  end;

  { TdxEffectExtent }

  TdxEffectExtent = record
  strict private
    FHorizontalIndent: Integer;
    FVerticalIndent: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FRotation: Integer;
    FUnitConverter: TdxDocumentModelUnitConverter;
  public
    constructor Create(AWidth: Integer; AHeight: Integer; ARotation: Integer; AUnitConverter: TdxDocumentModelUnitConverter);
    procedure Calculate;

    property HorizontalIndent: Integer read FHorizontalIndent;
    property VerticalIndent: Integer read FVerticalIndent;
  end;

  { TdxOpenXmlExporter }

  TdxOpenXmlExporter = class(TdxWordProcessingMLBaseExporter)
  public
    const
      WordProcessingNamespace = 'http://schemas.openxmlformats.org/wordprocessingml/2006/main';
      WordProcessingDrawingNamespace = 'http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing';
      WordProcessingDrawing14Namespace = 'http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing';
      WordProcessingDrawingPrefix = 'wp';
      WordProcessingDrawingPrefix14 = 'wp14';
      WordProcessingDrawingNamespaceConst = 'http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing';
      WordProcessingDrawing14NamespaceConst = 'http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing';
      DrawingMLPrefix = 'a';
      DrawingMLNamespace = 'http://schemas.openxmlformats.org/drawingml/2006/main';
      DrawingMLPicturePrefix = 'pic';
      DrawingMLPictureNamespace = 'http://schemas.openxmlformats.org/drawingml/2006/picture';
      RelsPrefix = 'r';
      RelsNamespace = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships';
      OfficeDocumentType = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument';
      OfficeStylesType = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles';
      OfficeWebSettingsType = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/webSettings';
      OfficeNumberingType = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/numbering';
      OfficeDocumentSettings = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/settings';
      OfficeHyperlinkType = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink';
      OfficeFootNoteType = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/footnotes';
      OfficeEndNoteType = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/endnotes';
      OfficeCommentType = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/comments';
      PackageRelsNamespace = 'http://schemas.openxmlformats.org/package/2006/relationships';
      RelsImage = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/image';
      RelsHeader = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/header';
      RelsFooter = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer';
      RelsFootNote = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/footnotes';
      RelsEndNote = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/endnotes';
      RelsComment = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships/comments';
      MCPrefix = 'mc';
      MCNamespace = 'http://schemas.openxmlformats.org/markup-compatibility/2006';
      WpsPrefix = 'wps';
      WpsNamespace = 'http://schemas.microsoft.com/office/word/2010/wordprocessingShape';
      Wp14Prefix = 'wp14';
      Wp14Namespace = 'http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing';
      MaxBookmarkNameLength = 40;
      MaxStyleNameLength = 253;
      MaxFontNameLength = 31;
  strict private
    FOutputStream: TStream;
    FPackage: TdxInternalZipArchive;
    FImageRelationsTableStack: TdxObjectStack<TdxStringsDictionary>;
    FExportedImageTableStack: TdxObjectStack<TDictionary<TdxOfficeImage, string>>;
    FHyperlinkRelationsTable: TdxPairStringList;
    FHeaderRelationsTable: TdxPairStringList;
    FFooterRelationsTable: TdxPairStringList;
    FFootNoteRelationsTable: TdxPairStringList;
    FEndNoteRelationsTable: TdxPairStringList;
    FCommentRelationsTable: TdxPairStringList;
    FUsedContentTypes: TdxPairStringList;
    FOverriddenContentTypes: TdxPairStringList;
    FFootNotes: TdxList<TdxFootNote>;
    FEndNotes: TdxList<TdxEndNote>;
    FComments: TList<TdxComment>;
    FBookmarkNames: TdxStringSet;
    FStyleNames: TdxStringSet;
    FDocumentRelationId: string;
    FHeaderCounter: Integer;
    FFooterCounter: Integer;
    FIgnorableNamespaces: TdxStringList;
    function GetOptions: TdxOpenXmlDocumentExporterOptions;
    function GetImageRelationsTable: TdxStringsDictionary;
  protected
    function GetUsedContentTypes: TdxPairStringList; virtual;
    function GetOverriddenContentTypes: TdxPairStringList; virtual;
    function GetExportedImageTable: TDictionary<TdxOfficeImage, string>; override;
    function GetPackage: TdxInternalZipArchive; override;
    function ShouldExportHiddenText: Boolean; override;
    function GetWordProcessingNamespace: string; override;
    function GetWordProcessingPrefix: string; override;
    function GetHorizontalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectHorizontalPositionType>; override;
    function GetVerticalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectVerticalPositionType>; override;
    procedure AddCompressedPackages; virtual;
    procedure SetPackage(APackage: TdxInternalZipArchive);
    procedure ExportFootNotesAndEndNotes; virtual;
    procedure ExportWebSettings; virtual;
    function CreateWebSettingsContent: TdxCompressedStream; virtual;
    procedure GenerateWebSettingsContent(AXmlWriter: TdxXmlWriter); virtual;
    procedure InitializeExport; virtual;
    function CalcDocumentRelationId: string; virtual;

    function ExportDocumentContent: TdxCompressedStream; virtual;
    function ExportPackageRelations: TdxCompressedStream; virtual;
    function ExportDocumentRelations: TdxCompressedStream; virtual;
    function ExportHeaderFooterRelations: TdxCompressedStream; virtual;
    function ExportCommentRelations: TdxCompressedStream; virtual;
    function ExportContentTypes: TdxCompressedStream; virtual;
    function ExportStyles: TdxCompressedStream; virtual;
    function ExportNumbering: TdxCompressedStream; virtual;
    function ExportSettings: TdxCompressedStream; virtual;

    procedure GenerateDocumentRelationsContent(AWriter: TdxXmlWriter); virtual;
    procedure GenerateDocumentRelations(AWriter: TdxXmlWriter); virtual;
    procedure GenerateHeaderFooterRelationsContent(AWriter: TdxXmlWriter); virtual;
    procedure GenerateCommentRelationsContent(AWriter: TdxXmlWriter); virtual;
    procedure GenerateFileRelationsCore(AWriter: TdxXmlWriter; ARelationTable: TdxPairStringList; const ARelationType: string); overload;
    procedure GenerateFileRelationsCore(AWriter: TdxXmlWriter; ARelationTable: TdxStringsDictionary; const ARelationType: string); overload;
    procedure GenerateDocumentXmlContent(AWriter: TdxXmlWriter); virtual;
    procedure GeneratePackageRelationsContent(AWriter: TdxXmlWriter); virtual;
    procedure GenerateContentTypesContent(AWriter: TdxXmlWriter); virtual;
    procedure RegisterContentTypeOverride(const APartName, AContentType: string); virtual;
    procedure GenerateDocumentContent; virtual;
    procedure ClearIgnorableNamespaces;
    procedure RegisterNamespaces; virtual;
    procedure RegisterDefaultNamespaces;
    procedure RegisterNamespace(const APrefix, ANs: string; AIgnorable: Boolean);
    procedure RegisterIgnorableNamespaces(const APrefixes: TArray<string>); overload;
    procedure RegisterIgnorableNamespaces; overload;
    procedure ExportDocumentBackground; virtual;
    procedure ExportDocumentVersion; virtual;
    procedure ExportParagraphListReference(AParagraph: TdxParagraph); override;
    procedure ExportImageReference(ARun: TdxInlinePictureRun); overload; override;
    procedure ExportImageReferenceCore(ARun: TdxInlinePictureRun; const AStartElement: string);
    function ExportImageData(AImage: TdxOfficeImageReference): string; overload; virtual;
    procedure ExportImageData(ARun: TdxInlinePictureRun); overload; virtual;
    function ExportImageData(AImage: TdxOfficeImage; const AImageId: string): string; overload; virtual;
    function GetImageFileName(const AImageId, AExtension: string): string;
    function GetImagePath(const AImageId, AExtension: string): string; overload;
    procedure ExportImageReference(const AImageRelationId: string); overload; virtual;
    procedure GenerateStylesContent(AWriter: TdxXmlWriter); virtual;
    procedure ExportDocumentDefaults; override;
    procedure ExportDocumentCharacterDefaults; override;
    procedure ExportDocumentParagraphDefaults; override;
    procedure ExportParagraphStyleListReference(ANumberingListIndex: TdxNumberingListIndex; AListLevelIndex: Integer); override;
    function HasNumberingProperties(ANumberingListIndex: TdxNumberingListIndex; AListLevelIndex: Integer): Boolean;
    procedure GenerateNumberingContent(AWriter: TdxXmlWriter); virtual;
    procedure ExportAbstractNumberingList(AList: TdxAbstractNumberingList; AId: Integer); override;
    procedure ExportNumberingList(AList: TdxNumberingList; AId: Integer); override;
    procedure ExportNumberFormatValue(AProperties: TdxListLevelProperties); override;
    procedure GenerateSettingsContent(AWriter: TdxXmlWriter); virtual;
    procedure ExportSettingsCore; virtual;
    procedure ExportDocumentProtectionSettingsCore; override;
    procedure ExportBookmarkStart(ABookmark: TdxBookmark); override;
    function PrepareFontName(const AName: string): string; override;
    function PrepareBookmarkName(const AName: string): string;
    function PrepareStyleName(const AName: string): string;
    function CutNameToPermittedLength(const AName: string; ANamesHashSet: TdxStringSet; AMaxLength: Integer): string;
    procedure ExportBookmarkEnd(ABookmark: TdxBookmark); override;
    function GenerateBookmarkId(ABookmark: TdxBookmark): string;
    procedure ExportFirstPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean); override;
    procedure ExportOddPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean); override;
    procedure ExportEvenPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean); override;
    procedure ExportFirstPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean); override;
    procedure ExportOddPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean); override;
    procedure ExportEvenPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean); override;
    function ExportHeader(AHeader: TdxSectionHeader): string; virtual;
    function ExportFooter(AFooter: TdxSectionFooter): string; virtual;
    function GenerateHeaderRelationId: string; virtual;
    function GenerateFooterRelationId: string; virtual;
    procedure ExportHeaderCore(AWriter: TdxXmlWriter; AHeader: TdxSectionHeaderFooterBase); virtual;
    procedure ExportFooterCore(AWriter: TdxXmlWriter; AFooter: TdxSectionFooter); virtual;
    procedure ExportHeaderReference(const AType: string; const AHeaderRelationId: string); virtual;
    procedure ExportFooterReference(const AType: string; const AFooterRelationId: string); virtual;
    procedure ExportFootNoteReference(ARun: TdxFootNoteRun); override;
    procedure PopulateExportedFootNotes; virtual;
    function ExportFootNotes: TdxCompressedStream; virtual;
    procedure GenerateFootNotesContent(AWriter: TdxXmlWriter); virtual;
    procedure ExportFootNotesCore; virtual;
    procedure ExportEndNoteReference(ARun: TdxEndNoteRun); override;
    procedure PopulateExportedEndNotes; virtual;
    function ExportEndNotes: TdxCompressedStream; virtual;
    procedure GenerateEndNotesContent(AWriter: TdxXmlWriter); virtual;
    procedure ExportEndNotesCore; virtual;
    procedure WriteWp14DrawingStartElement(const ATag: string); virtual;
    procedure WriteWpDrawingStartElement(const ATag: string); virtual;
    procedure WriteADrawingStartElement(const ATag: string); virtual;
    procedure WritePicDrawingStartElement(const ATag: string); virtual;
    procedure WriteMcStartElement(const ATag: string); virtual;
    procedure WriteWpsStartElement(const ATag: string); virtual;
    procedure WriteWpDrawingEndElement; virtual;
    procedure WriteWp14DrawingEndElement; virtual;
    procedure WriteADrawingEndElement; virtual;
    procedure WritePicDrawingEndElement; virtual;
    procedure WriteMcEndElement; virtual;
    procedure WriteWpsEndElement; virtual;
    procedure ExportFloatingObjectAnchorRun(ARun: TdxFloatingObjectAnchorRun); override;
    procedure WriteFloatingObjectAlternateContent(ARun: TdxFloatingObjectAnchorRun; ATextBoxContent: TdxTextBoxFloatingObjectContent);
    procedure WriteFloatingObjectAnchor(const ADrawingObject: IdxOpenXMLDrawingObject; const APictureContent: IdxOpenXMLDrawingObjectContent); overload;
    procedure ExportImageAsDrawing(ARun: TdxInlinePictureRun); virtual;
    procedure WriteFloatingObjectDrawing(const ADrawingObject: IdxOpenXMLDrawingObject; const APictureContent: IdxOpenXMLDrawingObjectContent); overload;
    function WriteFloatingObjectTextBoxContent2010(ARun: TdxFloatingObjectAnchorRun; ATextBoxContent: TdxTextBoxFloatingObjectContent): Integer;
    function WriteFloatingObjectDrawing(const ADrawingObject: IdxOpenXMLDrawingObject; ATextBoxContent: TdxTextBoxFloatingObjectContent): Integer; overload;
    function WriteFloatingObjectAnchor(const ADrawingObject: IdxOpenXMLDrawingObject; ATextBoxContent: TdxTextBoxFloatingObjectContent): Integer; overload;
    procedure WriteFloatingObjectTextBoxContent2007(ARun: TdxFloatingObjectAnchorRun; ATextBoxContent: TdxTextBoxFloatingObjectContent; ATextBoxId: Integer);
    procedure WriteFloatingObjectTextBoxContent(AContent: TdxTextBoxFloatingObjectContent; const ADrawingObject: IdxOpenXMLDrawingObject);
    procedure WriteFloatingObjectGraphicData(AContent: TdxTextBoxFloatingObjectContent; const ADrawingObject: IdxOpenXMLDrawingObject); overload;
    procedure WriteFloatingObjectWsp(AContent: TdxTextBoxFloatingObjectContent; const ADrawingObject: IdxOpenXMLDrawingObject);
    procedure WriteFloatingObjectBodyPr(AProperties: TdxTextBoxProperties);
    function ConvertTextBoxVerticalAlignment(AValue: TdxVerticalAlignment): string;
    procedure WriteFloatingObjectTxbx(AContent: TdxTextBoxFloatingObjectContent);
    procedure WriteFloatingObjectWpsSpPr(const ADrawingObject: IdxOpenXMLDrawingObject);
    procedure WriteFloatingObjectLn(AShape: TdxShape);
    procedure WriteFloatingObjectSolidFill(AColor: TdxAlphaColor);
    procedure WriteFloatingObjectSrgbClr(AColor: TdxAlphaColor);
    procedure WriteFloatingObjectCNvSpPr;
    procedure WriteFloatingObjectGraphicData(const AContent: IdxOpenXMLDrawingObjectContent; const ARun: IdxOpenXMLDrawingObject; AId: Integer); overload;
    procedure WriteFloatingObjectCNvPicPr;
    procedure WriteFloatingObjectCNvPr(AId: Integer);
    procedure WriteFloatingObjectNvPicPr(AId: Integer);
    procedure WriteFloatingObjectExt(const ARun: IdxOpenXMLDrawingObject);
    procedure WriteFloatingObjectOff;
    procedure WriteFloatingObjectBlip(const AContent: IdxOpenXMLDrawingObjectContent);
    procedure WriteFloatingObjectStretch;
    procedure WriteFloatingObjectBlipFill(const AContent: IdxOpenXMLDrawingObjectContent);
    procedure WriteFloatingObjectXfrm(const ARun: IdxOpenXMLDrawingObject);
    procedure WriteFloatingObjectPrstGeom;
    procedure WriteFloatingObjectPicSpPr(const ADrawingObject: IdxOpenXMLDrawingObject);
    procedure WriteFloatingObjectSpPr(const ADrawingObject: IdxOpenXMLDrawingObject);
    procedure WriteFloatingObjectPic(const AContent: IdxOpenXMLDrawingObjectContent; const ADrawingObject: IdxOpenXMLDrawingObject; AId: Integer);
    procedure WriteInlinePictureRunContent(ARun: TdxInlinePictureRun; AId: Integer);
    procedure WriteFloatingObjectPictureContent(const AContent: IdxOpenXMLDrawingObjectContent; const ADrawingObject: IdxOpenXMLDrawingObject; AId: Integer);
    procedure ExportFloatingObjectProperties(const ADrawingObject: IdxOpenXMLDrawingObject; const AName: string; AId: Integer);
    procedure ExportInlinePictureRun(ARun: TdxInlinePictureRun; const AName: string; AId: Integer); overload;
    procedure ExportRelativeSize(const ARun: IdxOpenXMLDrawingObject);
    procedure WriteInlinePictureCNvGraphicFramePr(ARun: TdxInlinePictureRun);
    procedure WriteInlinePictureGraphicFrameLocks(ARun: TdxInlinePictureRun);
    procedure WriteFloatingObjectCNvGraphicFramePr(const ARun: IdxOpenXMLDrawingObject);
    procedure WriteFloatingObjectGraphicFrameLocks(const ARun: IdxOpenXMLDrawingObject);
    procedure WriteInlinePictureDocPr(const AName: string; AId: Integer);
    procedure WriteFloatingObjectDocPr(const AName: string; AId: Integer);
    procedure WriteElementDocPrCore(const AName: string; AId: Integer);
    procedure WriteFloatingObjectSimplePosition;
    procedure WriteFloatingObjectWrap(const ARun: IdxOpenXMLDrawingObject);
    procedure WriteWpWrapPolygonElement(const ARun: IdxOpenXMLDrawingObject);
    procedure WriteWpLineToDrawingElement(X: Integer; Y: Integer);
    procedure WriteWpDrawingStart;
    procedure WriteWpDrawingElement(const ARun: IdxOpenXMLDrawingObject; const AElementName: string; AUseTextWrapSide: Boolean);
    procedure WriteFloatingObjectPercentWidth(ARelativeWidth: TdxFloatingObjectRelativeWidth);
    function ConvertFloatingObjectRelativeFromHorizontalType(AValue: TdxFloatingObjectRelativeFromHorizontal): string;
    procedure WriteFloatingObjectPercentHeight(ARelativeHeight: TdxFloatingObjectRelativeHeight);
    function ConvertFloatingObjectRelativeFromVerticalType(AValue: TdxFloatingObjectRelativeFromVertical): string;
    procedure WriteFloatingObjectPercentPositionV2010(const ADrawingObject: IdxOpenXMLDrawingObject);
    procedure WriteFloatingObjectPositionV(const ADrawingObject: IdxOpenXMLDrawingObject);
    procedure WriteFloatingObjectPositionVCore(const ADrawingObject: IdxOpenXMLDrawingObject;
      const AWriteVerticalOffsetAction: TdxAction<IdxOpenXMLDrawingObject>);
    procedure WriteFloatingObjectVerticalOffset(const ADrawingObject: IdxOpenXMLDrawingObject);
    procedure WriteFloatingObjectPercentVerticalOffset(const ADrawingObject: IdxOpenXMLDrawingObject);
    procedure WriteFloatingObjectVerticalPositionAlignment(const ADrawingObject: IdxOpenXMLDrawingObject);
    function ConvertFloatingObjectVerticalPositionAlignment(AValue: TdxFloatingObjectVerticalPositionAlignment): string;
    function ConvertFloatingObjectHorizontalPositionAlignment(AValue: TdxFloatingObjectHorizontalPositionAlignment): string;
    function ConvertFloatingObjectVerticalPositionType(AValue: TdxFloatingObjectVerticalPositionType): string;
    function ConvertFloatingObjectHorizontalPositionType(AValue: TdxFloatingObjectHorizontalPositionType): string;
    function ConvertFloatingObjectTextWrapSide(AValue: TdxFloatingObjectTextWrapSide): string;
    procedure WriteFloatingObjectPercentPositionH2010(const ADrawingObject: IdxOpenXMLDrawingObject);
    procedure WriteFloatingObjectPositionH(const ADrawingObject: IdxOpenXMLDrawingObject);
    procedure WriteFloatingObjectPositionHCore(const ADrawingObject: IdxOpenXMLDrawingObject;
      const AWriteHorizontalOffsetAction: TdxAction<IdxOpenXMLDrawingObject>);
    procedure WriteFloatingObjectHorizontalPositionAlignment(const ADrawingObject: IdxOpenXMLDrawingObject);
    procedure WriteFloatingObjectHorizontalOffset(const ADrawingObject: IdxOpenXMLDrawingObject);
    procedure WriteFloatingObjectPercentHorizontalOffset(const ADrawingObject: IdxOpenXMLDrawingObject);
    procedure WriteFloatingObjectExtent(const ADrawingObject: IdxOpenXMLDrawingObject);
    procedure WriteInlinePictureRunExtent(ARun: TdxInlinePictureRun);
    procedure WriteElementExtentCore(const AActualSize: TSize);
    procedure WriteInlinePictureEffectExtent(ARun: TdxInlinePictureRun);
    procedure WriteFloatingObjectEffectExtent(const ARun: IdxOpenXMLDrawingObject);
    function ModelUnitsToEMU(AModelUnits: Integer): Integer;
    function ConvertBoolToString(AValue: Boolean): string; override;
    function GetAbstractNumberingId(AStyleIndex: Integer): string; virtual;
    function GetWordProcessingMLValue(AValue: TdxWordProcessingMLValue): string; override;
    procedure ExportImageReference(ARun: TdxFloatingObjectAnchorRun); override;
    procedure ExportStyleName(const AStyle: IdxStyle); override;

    property HyperlinkRelationsTable: TdxPairStringList read FHyperlinkRelationsTable;
    property FootNotes: TdxList<TdxFootNote> read FFootNotes;
    property EndNotes: TdxList<TdxEndNote> read FEndNotes;
    property Comments: TList<TdxComment> read FComments;
    property BookmarkNames: TdxStringSet read FBookmarkNames;
    property StyleNames: TdxStringSet read FStyleNames;
    property IgnorableNamespaces: TdxStringList read FIgnorableNamespaces;
    property UsedContentTypes: TdxPairStringList read GetUsedContentTypes;
    property OverriddenContentTypes: TdxPairStringList read GetOverriddenContentTypes;
    property ImageRelationsTableStack: TdxObjectStack<TdxStringsDictionary> read FImageRelationsTableStack;
    property ExportedImageTableStack: TdxObjectStack<TDictionary<TdxOfficeImage, string>> read FExportedImageTableStack;
    property ImageRelationsTable: TdxStringsDictionary read GetImageRelationsTable;
    property HeaderRelationsTable: TdxPairStringList read FHeaderRelationsTable;
    property FooterRelationsTable: TdxPairStringList read FFooterRelationsTable;
    property FootNoteRelationsTable: TdxPairStringList read FFootNoteRelationsTable;
    property EndNoteRelationsTable: TdxPairStringList read FEndNoteRelationsTable;
    property CommentRelationsTable: TdxPairStringList read FCommentRelationsTable;
    property OutputStream: TStream read FOutputStream;
    property DocumentRelationId: string read FDocumentRelationId;
    property PictureId: Integer read FDrawingElementId;
  public
    destructor Destroy; override;
    procedure Export(AStream: TStream); overload; override;
    procedure Export; overload; override;

    property Options: TdxOpenXmlDocumentExporterOptions read GetOptions;
  end;

  { TdxExportOpenXmlFormat }

  TdxExportOpenXmlFormat = class(TdxExportFileFormat)
  public
    class function GetDocumentFormat: TdxRichEditDocumentFormat; override;
    function GetExporter(ADocumentModel: TdxCustomDocumentModel;
      const AOptions: IdxExporterOptions): TdxCustomDocumentModelExporter; override;
    function GetDocumentExporter: IdxExporter; override;
  end;

implementation

uses
  Windows,
  Contnrs, RTLConsts, Math, dxTypeHelpers, cxGeometry,
  dxBase64,
  dxOLECryptoContainer,
  dxRichEdit.DocumentModel.ProtectionFormatting,
  dxCharacters,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Import.OpenXML.DestinationPicture,
  dxRichEdit.Export.OpenXML.DocumentExporter;

{ TdxInlineDrawingObject }

constructor TdxInlineDrawingObject.Create(ARun: TdxInlinePictureRun);
begin
  FRun := ARun;
end;

function TdxInlineDrawingObject.GetName: string;
begin
  Result := '';
end;

function TdxInlineDrawingObject.GetFillColor: TdxAlphaColor;
begin
  Result := FRun.Properties.FillColor;
end;

function TdxInlineDrawingObject.GetActualSize: TSize;
begin
  Result := FRun.ActualSize;
end;

function TdxInlineDrawingObject.GetRotation: Integer;
begin
  Result := 0;
end;

function TdxInlineDrawingObject.GetLockAspectRatio: Boolean;
begin
  Result := FRun.LockAspectRatio;
end;

function TdxInlineDrawingObject.GetAllowOverlap: Boolean;
begin
  Result := False;
end;

function TdxInlineDrawingObject.GetIsBehindDoc: Boolean;
begin
  Result := False;
end;

function TdxInlineDrawingObject.GetIsFloatingObject: Boolean;
begin
  Result := False;
end;

function TdxInlineDrawingObject.GetLayoutInTableCell: Boolean;
begin
  Result := False;
end;

function TdxInlineDrawingObject.GetLocked: Boolean;
begin
  Result := False;
end;

function TdxInlineDrawingObject.GetZOrder: Integer;
begin
  Result := 0;
end;

function TdxInlineDrawingObject.GetUseBottomDistance: Boolean;
begin
  Result := False;
end;

function TdxInlineDrawingObject.GetBottomDistance: Integer;
begin
  Result := 0;
end;

function TdxInlineDrawingObject.GetUseLeftDistance: Boolean;
begin
  Result := False;
end;

function TdxInlineDrawingObject.GetLeftDistance: Integer;
begin
  Result := 0;
end;

function TdxInlineDrawingObject.GetUseRightDistance: Boolean;
begin
  Result := False;
end;

function TdxInlineDrawingObject.GetRightDistance: Integer;
begin
  Result := 0;
end;

function TdxInlineDrawingObject.GetUseTopDistance: Boolean;
begin
  Result := False;
end;

function TdxInlineDrawingObject.GetTopDistance: Integer;
begin
  Result := 0;
end;

function TdxInlineDrawingObject.GetUseHidden: Boolean;
begin
  Result := False;
end;

function TdxInlineDrawingObject.GetHidden: Boolean;
begin
  Result := False;
end;

function TdxInlineDrawingObject.GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
begin
  Result := TdxFloatingObjectHorizontalPositionAlignment.None;
end;

function TdxInlineDrawingObject.GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
begin
  Result := TdxFloatingObjectHorizontalPositionType.Page;
end;

function TdxInlineDrawingObject.GetUsePercentOffset: Boolean;
begin
  Result := False;
end;

function TdxInlineDrawingObject.GetPercentOffsetX: Integer;
begin
  Result := 0;
end;

function TdxInlineDrawingObject.GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
begin
  Result := TdxFloatingObjectVerticalPositionAlignment.None;
end;

function TdxInlineDrawingObject.GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
begin
  Result := TdxFloatingObjectVerticalPositionType.Page;
end;

function TdxInlineDrawingObject.GetPercentOffsetY: Integer;
begin
  Result := 0;
end;

function TdxInlineDrawingObject.GetUseLockAspectRatio: Boolean;
begin
  Result := False;
end;

function TdxInlineDrawingObject.GetUseRotation: Boolean;
begin
  Result := False;
end;

function TdxInlineDrawingObject.GetTextWrapType: TdxFloatingObjectTextWrapType;
begin
  Result := TdxFloatingObjectTextWrapType.None;
end;

function TdxInlineDrawingObject.GetTextWrapSide: TdxFloatingObjectTextWrapSide;
begin
  Result := TdxFloatingObjectTextWrapSide.Left;
end;

function TdxInlineDrawingObject.GetShape: TdxShape;
begin
  Result := nil;
end;

function TdxInlineDrawingObject.GetUseRelativeWidth: Boolean;
begin
  Result := False;
end;

function TdxInlineDrawingObject.GetUseRelativeHeight: Boolean;
begin
  Result := False;
end;

function TdxInlineDrawingObject.GetRelativeWidth: TdxFloatingObjectRelativeWidth;
begin
  Result := TdxFloatingObjectRelativeWidth.CreateEmpty;
end;

function TdxInlineDrawingObject.GetRelativeHeight: TdxFloatingObjectRelativeHeight;
begin
  Result := TdxFloatingObjectRelativeHeight.CreateEmpty;
end;

function TdxInlineDrawingObject.GetOffset: TPoint;
begin
  Result.Init(0, 0);
end;

function TdxInlineDrawingObject.GetPercentOffset: TPoint;
begin
  Result.Init(0, 0);
end;

{ TdxInlineDrawingObjectContent }

constructor TdxInlineDrawingObjectContent.Create(AImage: TdxOfficeImageReference);
begin
  inherited Create;
  FImage := AImage;
end;

function TdxInlineDrawingObjectContent.GetImage: TdxOfficeImageReference;
begin
  Result := FImage;
end;

{ TdxFloatingObjectDrawingObject }

constructor TdxFloatingObjectDrawingObject.Create(ARun: TdxFloatingObjectAnchorRun);
begin
  FRun := ARun;
end;

function TdxFloatingObjectDrawingObject.GetName: string;
begin
  Result := FRun.Name;
end;

function TdxFloatingObjectDrawingObject.GetActualSize: TSize;
begin
  Result := FRun.FloatingObjectProperties.ActualSize;
end;

function TdxFloatingObjectDrawingObject.GetRotation: Integer;
begin
  Result := FRun.Shape.Rotation;
end;

function TdxFloatingObjectDrawingObject.GetLockAspectRatio: Boolean;
begin
  Result := FRun.FloatingObjectProperties.LockAspectRatio;
end;

function TdxFloatingObjectDrawingObject.GetAllowOverlap: Boolean;
begin
  Result := FRun.FloatingObjectProperties.AllowOverlap;
end;

function TdxFloatingObjectDrawingObject.GetIsBehindDoc: Boolean;
begin
  Result := FRun.FloatingObjectProperties.IsBehindDoc;
end;

function TdxFloatingObjectDrawingObject.GetIsFloatingObject: Boolean;
begin
  Result := True;
end;

function TdxFloatingObjectDrawingObject.GetLayoutInTableCell: Boolean;
begin
  Result := FRun.FloatingObjectProperties.LayoutInTableCell;
end;

function TdxFloatingObjectDrawingObject.GetLocked: Boolean;
begin
  Result := FRun.FloatingObjectProperties.Locked;
end;

function TdxFloatingObjectDrawingObject.GetZOrder: Integer;
begin
  Result := FRun.FloatingObjectProperties.ZOrder;
end;

function TdxFloatingObjectDrawingObject.GetUseBottomDistance: Boolean;
begin
  Result := FRun.FloatingObjectProperties.UseBottomDistance;
end;

function TdxFloatingObjectDrawingObject.GetBottomDistance: Integer;
begin
  Result := FRun.FloatingObjectProperties.BottomDistance;
end;

function TdxFloatingObjectDrawingObject.GetUseLeftDistance: Boolean;
begin
  Result := FRun.FloatingObjectProperties.UseLeftDistance;
end;

function TdxFloatingObjectDrawingObject.GetLeftDistance: Integer;
begin
  Result := FRun.FloatingObjectProperties.LeftDistance;
end;

function TdxFloatingObjectDrawingObject.GetUseRightDistance: Boolean;
begin
  Result := FRun.FloatingObjectProperties.UseRightDistance;
end;

function TdxFloatingObjectDrawingObject.GetRightDistance: Integer;
begin
  Result := FRun.FloatingObjectProperties.RightDistance;
end;

function TdxFloatingObjectDrawingObject.GetUseTopDistance: Boolean;
begin
  Result := FRun.FloatingObjectProperties.UseTopDistance;
end;

function TdxFloatingObjectDrawingObject.GetTopDistance: Integer;
begin
  Result := FRun.FloatingObjectProperties.TopDistance;
end;

function TdxFloatingObjectDrawingObject.GetUseHidden: Boolean;
begin
  Result := FRun.FloatingObjectProperties.UseHidden;
end;

function TdxFloatingObjectDrawingObject.GetHidden: Boolean;
begin
  Result := FRun.FloatingObjectProperties.Hidden;
end;

function TdxFloatingObjectDrawingObject.GetHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment;
begin
  Result := FRun.FloatingObjectProperties.HorizontalPositionAlignment;
end;

function TdxFloatingObjectDrawingObject.GetUsePercentOffset: Boolean;
begin
  Result := FRun.FloatingObjectProperties.UsePercentOffset;
end;

function TdxFloatingObjectDrawingObject.GetPercentOffsetX: Integer;
begin
  Result := FRun.FloatingObjectProperties.PercentOffsetX;
end;

function TdxFloatingObjectDrawingObject.GetVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
begin
  Result := FRun.FloatingObjectProperties.VerticalPositionAlignment;
end;

function TdxFloatingObjectDrawingObject.GetPercentOffsetY: Integer;
begin
  Result := FRun.FloatingObjectProperties.PercentOffsetY;
end;

function TdxFloatingObjectDrawingObject.GetUseLockAspectRatio: Boolean;
begin
  Result := FRun.FloatingObjectProperties.UseLockAspectRatio;
end;

function TdxFloatingObjectDrawingObject.GetUseRotation: Boolean;
begin
  Result := FRun.Shape.UseRotation;
end;

function TdxFloatingObjectDrawingObject.GetTextWrapType: TdxFloatingObjectTextWrapType;
begin
  Result := FRun.FloatingObjectProperties.TextWrapType;
end;

function TdxFloatingObjectDrawingObject.GetTextWrapSide: TdxFloatingObjectTextWrapSide;
begin
  Result := FRun.FloatingObjectProperties.TextWrapSide;
end;

function TdxFloatingObjectDrawingObject.GetShape: TdxShape;
begin
  Result := FRun.Shape;
end;

function TdxFloatingObjectDrawingObject.GetUseRelativeWidth: Boolean;
begin
  Result := FRun.FloatingObjectProperties.UseRelativeWidth;
end;

function TdxFloatingObjectDrawingObject.GetUseRelativeHeight: Boolean;
begin
  Result := FRun.FloatingObjectProperties.UseRelativeHeight;
end;

function TdxFloatingObjectDrawingObject.GetRelativeWidth: TdxFloatingObjectRelativeWidth;
begin
  Result := FRun.FloatingObjectProperties.RelativeWidth;
end;

function TdxFloatingObjectDrawingObject.GetRelativeHeight: TdxFloatingObjectRelativeHeight;
begin
  Result := FRun.FloatingObjectProperties.RelativeHeight;
end;

function TdxFloatingObjectDrawingObject.GetHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
begin
  Result := FRun.FloatingObjectProperties.HorizontalPositionType;
end;

function TdxFloatingObjectDrawingObject.GetVerticalPositionType: TdxFloatingObjectVerticalPositionType;
begin
  Result := FRun.FloatingObjectProperties.VerticalPositionType;
end;

function TdxFloatingObjectDrawingObject.GetOffset: TPoint;
begin
  Result := FRun.FloatingObjectProperties.Offset;
end;

function TdxFloatingObjectDrawingObject.GetPercentOffset: TPoint;
begin
  Result := FRun.FloatingObjectProperties.PercentOffset;
end;

{ TdxFloatingObjectDrawingObjectContent }

constructor TdxFloatingObjectDrawingObjectContent.Create(AContent: TdxPictureFloatingObjectContent);
begin
  FContent := AContent;
end;

function TdxFloatingObjectDrawingObjectContent.GetImage: TdxOfficeImageReference;
begin
  Result := FContent.Image;
end;

{ TdxPairStringList }

procedure TdxPairStringList.Add(const AKey, AValue: string);
begin
  inherited Add(TdxPairString.Create(AKey, AValue));
end;

function TdxPairStringList.GetItems(const AKey: string): string;
begin
  TryGetValue(AKey, Result);
end;

function TdxPairStringList.TryGetValue(const AKey: string; out AValue: string): Boolean;
var
  APair: TdxPairString;
begin
  for APair in Self do
    if AKey = APair.Key then
    begin
      AValue := APair.Value;
      Exit(True);
    end;
  AValue := '';
  Result := False;
end;

{ TdxEffectExtent }

constructor TdxEffectExtent.Create(AWidth: Integer; AHeight: Integer; ARotation: Integer; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FRotation := ARotation;
  FUnitConverter := AUnitConverter;
end;

procedure TdxEffectExtent.Calculate;
var
  AAngle: Single;
  ASin, ACos: Double;
  AXCoordinateA, AYCoordinateA, ANewXCoordinateA, ANewYCoordinateA,
  AXCoordinateB, AYCoordinateB, ANewXCoordinateB, ANewYCoordinateB: Integer;
begin
  AAngle := FUnitConverter.ModelUnitsToDegree(FRotation) * PI / 180;
  SinCos(AAngle, ASin, ACos);
  AXCoordinateA := FWidth div 2;
  AYCoordinateA := FHeight div 2;
  ANewXCoordinateA := Trunc(AXCoordinateA * ACos - AYCoordinateA * ASin);
  ANewYCoordinateA := Trunc(AXCoordinateA * ASin + AYCoordinateA * ACos);
  AXCoordinateB := AXCoordinateA;
  AYCoordinateB := -AYCoordinateA;
  ANewXCoordinateB := Trunc(AXCoordinateB * ACos - AYCoordinateB * ASin);
  ANewYCoordinateB := Trunc(AXCoordinateB * ASin + AYCoordinateB * ACos);

  if Abs(ANewXCoordinateA) > Abs(ANewXCoordinateB) then
  begin
    FHorizontalIndent := Abs(Abs(ANewXCoordinateA) - Abs(AXCoordinateA));
    FVerticalIndent := Abs(Abs(ANewYCoordinateB) - Abs(AYCoordinateB));
  end
  else
  begin
    FHorizontalIndent := Abs(Abs(ANewXCoordinateB) - Abs(AXCoordinateB));
    FVerticalIndent := Abs(Abs(ANewYCoordinateA) - Abs(AYCoordinateA));
  end;

  if (FUnitConverter.ModelUnitsToDegree(FRotation) - 90) mod 180 = 0 then
    FHorizontalIndent := 0;
end;

{ TdxOpenXmlExporter }

destructor TdxOpenXmlExporter.Destroy;
begin
  FImageRelationsTableStack.Free;
  FExportedImageTableStack.Free;
  FHeaderRelationsTable.Free;
  FFooterRelationsTable.Free;
  FFootNoteRelationsTable.Free;
  FEndNoteRelationsTable.Free;
  FCommentRelationsTable.Free;
  FHyperlinkRelationsTable.Free;
  FUsedContentTypes.Free;
  FOverriddenContentTypes.Free;
  FFootNotes.Free;
  FEndNotes.Free;
  FComments.Free;
  FBookmarkNames.Free;
  FStyleNames.Free;
  FIgnorableNamespaces.Free;
  inherited Destroy;
end;

function TdxOpenXmlExporter.GetOptions: TdxOpenXmlDocumentExporterOptions;
begin
  Result := TdxOpenXmlDocumentExporterOptions(inherited Options);
end;

function TdxOpenXmlExporter.GetUsedContentTypes: TdxPairStringList;
begin
  Result := FUsedContentTypes;
end;

function TdxOpenXmlExporter.GetOverriddenContentTypes: TdxPairStringList;
begin
  Result := FOverriddenContentTypes;
end;

function TdxOpenXmlExporter.GetImageRelationsTable: TdxStringsDictionary;
begin
  Result := FImageRelationsTableStack.Peek;
end;

function TdxOpenXmlExporter.GetExportedImageTable: TDictionary<TdxOfficeImage, string>;
begin
  Result := FExportedImageTableStack.Peek;
end;

function TdxOpenXmlExporter.GetPackage: TdxInternalZipArchive;
begin
  Result := FPackage;
end;

function TdxOpenXmlExporter.ShouldExportHiddenText: Boolean;
begin
  Result := True;
end;

function TdxOpenXmlExporter.GetWordProcessingNamespace: string;
begin
  Result := WordProcessingNamespace;
end;

function TdxOpenXmlExporter.GetWordProcessingPrefix: string;
begin
  Result := 'w';
end;

function TdxOpenXmlExporter.GetHorizontalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectHorizontalPositionType>;
begin
  Result := TdxInlineObjectDestination.OpenXmlHorizontalPositionTypeAttributeTable;
end;

function TdxOpenXmlExporter.GetVerticalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectVerticalPositionType>;
begin
  Result := TdxInlineObjectDestination.OpenXmlVerticalPositionTypeAttributeTable;
end;

procedure TdxOpenXmlExporter.Export(AStream: TStream);
begin
  if DocumentModel.EncryptionProperties.IsEncrypted then
  begin
    FOutputStream := TMemoryStream.Create;
    try
      Export;
      TdxOLECryptoContainer.Encrypt(FOutputStream, AStream, DocumentModel.EncryptionProperties.Password,
        TdxOLECryptoContainerEncryptorStandard);
    finally
      FreeAndNil(FOutputStream);
    end;
  end
  else
  begin
    FOutputStream := AStream;
    Export;
  end;
end;

procedure TdxOpenXmlExporter.Export;
var
  ADocumentPackage: TdxInternalZipArchive;
begin
  if FOutputStream = nil then
    TdxRichEditExceptions.ThrowInvalidOperationException('OutputStream is nil');

  ADocumentPackage := TdxInternalZipArchive.Create(FOutputStream);
  try
    FPackage := ADocumentPackage;

    InitializeExport;
    AddCompressedPackages;
  finally
    ADocumentPackage.Free;
  end;
end;

procedure TdxOpenXmlExporter.AddCompressedPackages;
begin

  AddCompressedPackageContent('word\document.xml', ExportDocumentContent);
  AddCompressedPackageContent('word\styles.xml', ExportStyles);
  AddCompressedPackageContent('word\numbering.xml', ExportNumbering);
  AddCompressedPackageContent('word\settings.xml', ExportSettings);

  ExportFootNotesAndEndNotes;
  ExportWebSettings;

  AddCompressedPackageContent('_rels\.rels', ExportPackageRelations);
  AddCompressedPackageContent('[Content_Types].xml', ExportContentTypes);
  AddCompressedPackageContent('word\_rels\document.xml.rels', ExportDocumentRelations);
end;

procedure TdxOpenXmlExporter.SetPackage(APackage: TdxInternalZipArchive);
begin
  FPackage := APackage;
end;

procedure TdxOpenXmlExporter.ExportFootNotesAndEndNotes;
begin
  PopulateExportedFootNotes;
  PopulateExportedEndNotes;
  if (FFootNotes.Count > 0) and DocumentModel.DocumentCapabilities.FootNotesAllowed then
  begin
    AddCompressedPackageContent('word\footnotes.xml', ExportFootNotes);
    RegisterContentTypeOverride('/word/footnotes.xml', 'application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml');
    FootNoteRelationsTable.Add('RelFnt1', 'footnotes.xml');
  end;
  if (FEndNotes.Count > 0) and DocumentModel.DocumentCapabilities.EndNotesAllowed then
  begin
    AddCompressedPackageContent('word\endnotes.xml', ExportEndNotes);
    RegisterContentTypeOverride('/word/endnotes.xml', 'application/vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml');
    EndNoteRelationsTable.Add('RelEnt1', 'endnotes.xml');
  end;
end;

procedure TdxOpenXmlExporter.ExportWebSettings;
var
  AWebSettings: TdxWebSettings;
begin
  AWebSettings := DocumentModel.WebSettings;
  if not AWebSettings.IsBodyMarginsSet then
    Exit;

  AddCompressedPackageContent('word\webSettings.xml', CreateWebSettingsContent);
  RegisterContentTypeOverride('/word/webSettings.xml', 'application/vnd.openxmlformats-officedocument.wordprocessingml.webSettings+xml');
end;

function TdxOpenXmlExporter.CreateWebSettingsContent: TdxCompressedStream;
begin
  Result := CreateCompressedXmlContent(GenerateWebSettingsContent);
end;

procedure TdxOpenXmlExporter.GenerateWebSettingsContent(AXmlWriter: TdxXmlWriter);
var
  AWebSettings: TdxWebSettings;
begin
  DocumentContentWriter := AXmlWriter;
  WriteWpStartElement('webSettings');
  WriteWpStartElement('divs');
  try
    AWebSettings := DocumentModel.WebSettings;
    WriteWpStartElement('div');
    try

      WriteWpIntAttr('id', AWebSettings.GetHashCode);
      WriteWpBoolValue('bodyDiv', True);
      WriteWpIntValue('marLeft', UnitConverter.ModelUnitsToTwips(AWebSettings.LeftMargin));
      WriteWpIntValue('marRight', UnitConverter.ModelUnitsToTwips(AWebSettings.RightMargin));
      WriteWpIntValue('marTop', UnitConverter.ModelUnitsToTwips(AWebSettings.TopMargin));
      WriteWpIntValue('marBottom', UnitConverter.ModelUnitsToTwips(AWebSettings.BottomMargin));
    finally
      WriteWpEndElement;
    end;
  finally
    WriteWpEndElement;
    WriteWpEndElement;
  end;
end;

procedure TdxOpenXmlExporter.InitializeExport;
begin
  ImageCounter := 0;
  FHeaderCounter := 0;
  FFooterCounter := 0;
  FImageRelationsTableStack := TdxObjectStack<TdxStringsDictionary>.Create(True);
  FImageRelationsTableStack.Push(TdxStringsDictionary.Create);
  FExportedImageTableStack := TdxObjectStack<TDictionary<TdxOfficeImage, string>>.Create(True);
  FExportedImageTableStack.Push(TDictionary<TdxOfficeImage, string>.Create);
  FHeaderRelationsTable := TdxPairStringList.Create;
  FFooterRelationsTable := TdxPairStringList.Create;
  FFootNoteRelationsTable := TdxPairStringList.Create;
  FEndNoteRelationsTable := TdxPairStringList.Create;
  FCommentRelationsTable := TdxPairStringList.Create;
  FHyperlinkRelationsTable := TdxPairStringList.Create;
  FUsedContentTypes := TdxPairStringList.Create;
  FOverriddenContentTypes := TdxPairStringList.Create;
  FFootNotes := TdxList<TdxFootNote>.Create;
  FEndNotes := TdxList<TdxEndNote>.Create;
  FComments := TList<TdxComment>.Create;
  FBookmarkNames := TdxStringSet.Create;
  FStyleNames := TdxStringSet.Create;
  FIgnorableNamespaces := TdxStringList.Create;
  FDocumentRelationId := CalcDocumentRelationId;

  FUsedContentTypes.Add('rels', 'application/vnd.openxmlformats-package.relationships+xml');
  RegisterContentTypeOverride('/word/document.xml', 'application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml');
end;

function TdxOpenXmlExporter.CalcDocumentRelationId: string;
begin
  Result := Format('R%.8x', [GetTickCount]);
end;

function TdxOpenXmlExporter.ExportDocumentContent: TdxCompressedStream;
begin
  Result := CreateCompressedXmlContent(GenerateDocumentXmlContent);
end;

function TdxOpenXmlExporter.ExportPackageRelations: TdxCompressedStream;
begin
  Result := CreateCompressedXmlContent(GeneratePackageRelationsContent);
end;

function TdxOpenXmlExporter.ExportDocumentRelations: TdxCompressedStream;
begin
  Result := CreateCompressedXmlContent(GenerateDocumentRelationsContent);
end;

function TdxOpenXmlExporter.ExportHeaderFooterRelations: TdxCompressedStream;
begin
  Result := CreateCompressedXmlContent(GenerateHeaderFooterRelationsContent);
end;

function TdxOpenXmlExporter.ExportCommentRelations: TdxCompressedStream;
begin
  Result := CreateCompressedXmlContent(GenerateCommentRelationsContent);
end;

function TdxOpenXmlExporter.ExportContentTypes: TdxCompressedStream;
begin
  Result := CreateCompressedXmlContent(GenerateContentTypesContent);
end;

function TdxOpenXmlExporter.ExportStyles: TdxCompressedStream;
begin
  Result := CreateCompressedXmlContent(GenerateStylesContent);
end;

function TdxOpenXmlExporter.ExportNumbering: TdxCompressedStream;
begin
  Result := CreateCompressedXmlContent(GenerateNumberingContent);
end;

function TdxOpenXmlExporter.ExportSettings: TdxCompressedStream;
begin
  Result := CreateCompressedXmlContent(GenerateSettingsContent);
end;

procedure TdxOpenXmlExporter.GenerateDocumentRelationsContent(AWriter: TdxXmlWriter);
var
  ARelation: TdxPairString;
begin
  AWriter.WriteStartElement('Relationships', PackageRelsNamespace);

  GenerateDocumentRelations(AWriter);

  AWriter.WriteStartElement('Relationship');
  AWriter.WriteAttributeString('Id', 'RelStyle1');
  AWriter.WriteAttributeString('Type', OfficeStylesType);
  AWriter.WriteAttributeString('Target', 'styles.xml');
  AWriter.WriteEndElement;

  if DocumentModel.WebSettings.IsBodyMarginsSet then
  begin
    AWriter.WriteStartElement('Relationship');
    AWriter.WriteAttributeString('Id', 'RelWebSettings1');
    AWriter.WriteAttributeString('Type', OfficeWebSettingsType);
    AWriter.WriteAttributeString('Target', 'webSettings.xml');
    AWriter.WriteEndElement;
  end;

  AWriter.WriteStartElement('Relationship');
  AWriter.WriteAttributeString('Id', 'RelNum1');
  AWriter.WriteAttributeString('Type', OfficeNumberingType);
  AWriter.WriteAttributeString('Target', 'numbering.xml');
  AWriter.WriteEndElement;

  AWriter.WriteStartElement('Relationship');
  AWriter.WriteAttributeString('Id', 'RelSettings1');
  AWriter.WriteAttributeString('Type', OfficeDocumentSettings);
  AWriter.WriteAttributeString('Target', 'settings.xml');
  AWriter.WriteEndElement;

  for ARelation in FHyperlinkRelationsTable do
  begin
    AWriter.WriteStartElement('Relationship');
    AWriter.WriteAttributeString('Id', ARelation.Key);
    AWriter.WriteAttributeString('Type', OfficeHyperlinkType);
    AWriter.WriteAttributeString('Target', ARelation.Value);
    AWriter.WriteAttributeString('TargetMode', 'External');
    AWriter.WriteEndElement;
  end;

  AWriter.WriteEndElement;
end;

procedure TdxOpenXmlExporter.GenerateDocumentRelations(AWriter: TdxXmlWriter);
begin
  GenerateFileRelationsCore(AWriter, ImageRelationsTable, RelsImage);
  GenerateFileRelationsCore(AWriter, FHeaderRelationsTable, RelsHeader);
  GenerateFileRelationsCore(AWriter, FFooterRelationsTable, RelsFooter);
  GenerateFileRelationsCore(AWriter, FFootNoteRelationsTable, RelsFootNote);
  GenerateFileRelationsCore(AWriter, FEndNoteRelationsTable, RelsEndNote);
  GenerateFileRelationsCore(AWriter, FCommentRelationsTable, RelsComment);
end;

procedure TdxOpenXmlExporter.GenerateHeaderFooterRelationsContent(AWriter: TdxXmlWriter);
begin
  AWriter.WriteStartElement('Relationships', PackageRelsNamespace);
  GenerateFileRelationsCore(AWriter, ImageRelationsTable, RelsImage);
  AWriter.WriteEndElement;
end;

procedure TdxOpenXmlExporter.GenerateCommentRelationsContent(AWriter: TdxXmlWriter);
begin
  AWriter.WriteStartElement('Relationships', PackageRelsNamespace);
  GenerateFileRelationsCore(AWriter, ImageRelationsTable, RelsImage);
  AWriter.WriteEndElement;
end;

procedure TdxOpenXmlExporter.GenerateFileRelationsCore(AWriter: TdxXmlWriter; ARelationTable: TdxPairStringList;
  const ARelationType: string);
var
  ARelation: TdxPairString;
begin
  for ARelation in ARelationTable do
  begin
    AWriter.WriteStartElement('Relationship');
    AWriter.WriteAttributeString('Id', ARelation.Key);
    AWriter.WriteAttributeString('Type', ARelationType);
    AWriter.WriteAttributeString('Target', ARelation.Value);
    AWriter.WriteEndElement;
  end;
end;

procedure TdxOpenXmlExporter.GenerateDocumentXmlContent(AWriter: TdxXmlWriter);
begin
  DocumentContentWriter := AWriter;
  GenerateDocumentContent;
end;

procedure TdxOpenXmlExporter.GeneratePackageRelationsContent(AWriter: TdxXmlWriter);
begin
  AWriter.WriteStartElement('Relationships', PackageRelsNamespace);
  AWriter.WriteStartElement('Relationship');
  AWriter.WriteAttributeString('Id', DocumentRelationId);
  AWriter.WriteAttributeString('Type', OfficeDocumentType);
  AWriter.WriteAttributeString('Target', '/word/document.xml');
  AWriter.WriteEndElement;
  AWriter.WriteEndElement;
end;

procedure TdxOpenXmlExporter.GenerateContentTypesContent(AWriter: TdxXmlWriter);
var
  APair: TdxPairString;
begin
  RegisterContentTypeOverride('/word/styles.xml', 'application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml');
  RegisterContentTypeOverride('/word/settings.xml', 'application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml');
  RegisterContentTypeOverride('/word/numbering.xml', 'application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml');

  AWriter.WriteStartElement('Types', 'http://schemas.openxmlformats.org/package/2006/content-types');
  for APair in FUsedContentTypes do
  begin
    AWriter.WriteStartElement('Default');
    AWriter.WriteAttributeString('Extension', APair.Key);
    AWriter.WriteAttributeString('ContentType', APair.Value);
    AWriter.WriteEndElement;
  end;

  for APair in FOverriddenContentTypes do
  begin
    AWriter.WriteStartElement('Override');
    AWriter.WriteAttributeString('PartName', APair.Key);
    AWriter.WriteAttributeString('ContentType', APair.Value);
    AWriter.WriteEndElement;
  end;

  AWriter.WriteEndElement;
end;

procedure TdxOpenXmlExporter.RegisterContentTypeOverride(const APartName, AContentType: string);
begin
  FOverriddenContentTypes.Add(TdxPairString.Create(APartName, AContentType));
end;

procedure TdxOpenXmlExporter.GenerateDocumentContent;
begin
  WriteWpStartElement('document');
  try
    RegisterNamespaces;
    ExportDocumentBackground;
    ExportDocumentVersion;
    WriteWpStartElement('body');
    try
      inherited Export;
      ExportSectionProperties(CurrentSection);
    finally
      WriteWpEndElement;
    end;
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxOpenXmlExporter.ClearIgnorableNamespaces;
begin
  FIgnorableNamespaces.Clear;
end;

procedure TdxOpenXmlExporter.RegisterNamespaces;
begin

  ClearIgnorableNamespaces;
  RegisterDefaultNamespaces;
  RegisterIgnorableNamespaces;
end;

procedure TdxOpenXmlExporter.RegisterDefaultNamespaces;
begin
  RegisterNamespace(WordProcessingPrefix, WordProcessingNamespace, False);
  RegisterNamespace(WpsPrefix, WpsNamespace, False);
  RegisterNamespace(MCPrefix, MCNamespace, False);
  RegisterNamespace(W10MLPrefix, W10MLNamespace, False);
  RegisterNamespace(VMLPrefix, VMLNamespace, False);
  RegisterNamespace(WordProcessingDrawingPrefix14, WordProcessingDrawing14Namespace, True);
end;

procedure TdxOpenXmlExporter.RegisterNamespace(const APrefix, ANs: string; AIgnorable: Boolean);
begin
  DocumentContentWriter.WriteAttributeString('xmlns', APrefix, '', ANs);
  if AIgnorable then
    FIgnorableNamespaces.Add(APrefix);
end;

procedure TdxOpenXmlExporter.RegisterIgnorableNamespaces(const APrefixes: TArray<string>);
begin
  DocumentContentWriter.WriteAttributeString('mc', 'Ignorable', '', TdxStringHelper.Join(' ', APrefixes));
end;

procedure TdxOpenXmlExporter.RegisterIgnorableNamespaces;
begin
  if FIgnorableNamespaces.Count > 0 then
    RegisterIgnorableNamespaces(FIgnorableNamespaces.ToArray);
  FIgnorableNamespaces.Clear;
end;

procedure TdxOpenXmlExporter.ExportDocumentBackground;
var
  APageBackColor: TdxAlphaColor;
begin
  APageBackColor := DocumentModel.DocumentProperties.PageBackColor;
  if APageBackColor = TdxAlphaColors.Empty then
    Exit;

  WriteWpStartElement('background');
  try
    WriteWpStringAttr('color', ConvertColorToString(APageBackColor));
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxOpenXmlExporter.ExportDocumentVersion;
begin
end;

procedure TdxOpenXmlExporter.ExportParagraphListReference(AParagraph: TdxParagraph);
begin
  WriteWpStartElement('numPr');
  try
    WriteWpIntValue('ilvl', AParagraph.GetListLevelIndex);
    WriteWpIntValue('numId', GetNumberingListIndexForExport(AParagraph.GetOwnNumberingListIndex));
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxOpenXmlExporter.ExportImageReference(ARun: TdxInlinePictureRun);
begin
  ExportImageAsDrawing(ARun);
end;

procedure TdxOpenXmlExporter.ExportImageReferenceCore(ARun: TdxInlinePictureRun; const AStartElement: string);
var
  AFinalWidth, AFinalHeight: Single;
  AImageStyle: string;
begin
  WriteWpStartElement(AStartElement);
  try
    DocumentContentWriter.WriteStartElement(VMLPrefix, 'shape', VMLNamespace);
    try
      AFinalWidth := UnitConverter.ModelUnitsToPointsF(ARun.ActualSizeF.Width);
      AFinalHeight := UnitConverter.ModelUnitsToPointsF(ARun.ActualSizeF.Height);
      AImageStyle := TdxStringHelper.Format('width:%gpt;height:%gpt', [AFinalWidth, AFinalHeight]);
      DocumentContentWriter.WriteAttributeString('style', AImageStyle);
      ExportImageData(ARun);
    finally
      DocumentContentWriter.WriteEndElement;
    end;
  finally
    WriteWpEndElement;
  end;
end;

function TdxOpenXmlExporter.ExportImageData(AImage: TdxOfficeImageReference): string;
var
  AImageRelationId, AImageId, AImagePath: string;
  ARootImage: TdxOfficeImage;
begin
  ARootImage := AImage.Image;
  if ExportedImageTable.TryGetValue(ARootImage, AImageRelationId) then
    Exit(AImageRelationId);

  AImageId := GenerateImageId;
  AImagePath := ExportImageData(ARootImage, AImageId);
  AImageRelationId := GenerateImageRelationId(AImageId);
  ImageRelationsTable.Add(AImageRelationId, AImagePath);
  ExportedImageTable.Add(ARootImage, AImageRelationId);
  Result := AImageRelationId;
end;

procedure TdxOpenXmlExporter.ExportImageData(ARun: TdxInlinePictureRun);
var
  AImageRelationId: string;
begin
  AImageRelationId := ExportImageData(ARun.Image);
  ExportImageReference(AImageRelationId);
end;

function TdxOpenXmlExporter.ExportImageData(AImage: TdxOfficeImage; const AImageId: string): string;
var
  AExtension, AContentType: string;
begin
  AExtension := GetImageExtension(AImage);
  if not FUsedContentTypes.TryGetValue(AExtension, AContentType) then
  begin
    AContentType := TdxOfficeImage.GetContentType(AImage.RawFormat);
    if AContentType = '' then
      AContentType := TdxOfficeImage.GetContentType(TdxOfficeImageFormat.Png);
    FUsedContentTypes.Add(AExtension, AContentType);
  end;

  AddPackageImage(GetImageFileName(AImageId, AExtension), AImage);
  Result := GetImagePath(AImageId, AExtension);
end;

function TdxOpenXmlExporter.GetImageFileName(const AImageId, AExtension: string): string;
begin
  if DocumentModel.DocumentExportOptions.OpenXml.AlternateImageFolder then
    Result := 'word\media\' + AImageId + '.' + AExtension
  else
    Result := 'media\' + AImageId + '.' + AExtension;
end;

function TdxOpenXmlExporter.GetImagePath(const AImageId, AExtension: string): string;
begin
  if DocumentModel.DocumentExportOptions.OpenXml.AlternateImageFolder then
    Result := '/word/media/' + AImageId + '.' + AExtension
  else
    Result := '/media/' + AImageId + '.' + AExtension;
end;

procedure TdxOpenXmlExporter.ExportImageReference(const AImageRelationId: string);
begin
  DocumentContentWriter.WriteStartElement(VMLPrefix, 'imagedata', VMLNamespace);
  try
    DocumentContentWriter.WriteAttributeString(RelsPrefix, 'id', RelsNamespace, AImageRelationId);
    DocumentContentWriter.WriteAttributeString(OfficePrefix, 'title', OfficeNamespace, '');
  finally
    DocumentContentWriter.WriteEndElement;
  end;
end;

procedure TdxOpenXmlExporter.GenerateStylesContent(AWriter: TdxXmlWriter);
begin
  DocumentContentWriter := AWriter;
  ExportStylesCore;
end;

procedure TdxOpenXmlExporter.ExportDocumentDefaults;
begin
  WriteWpStartElement('docDefaults');
  try
    ExportDocumentCharacterDefaults;
    ExportDocumentParagraphDefaults;
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxOpenXmlExporter.ExportDocumentCharacterDefaults;
begin
  WriteWpStartElement('rPrDefault');
  try
    WriteWpStartElement('rPr');
    try
      ExportRunPropertiesCore(DocumentModel.DefaultCharacterProperties);
    finally
      WriteWpEndElement;
    end;
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxOpenXmlExporter.ExportDocumentParagraphDefaults;
begin
  WriteWpStartElement('pPrDefault');
  try
    WriteWpStartElement('pPr');
    try
      ExportParagraphPropertiesCore(DocumentModel.DefaultParagraphProperties, True, nil, nil, nil);
    finally
      WriteWpEndElement;
    end;
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxOpenXmlExporter.ExportParagraphStyleListReference(ANumberingListIndex: TdxNumberingListIndex; AListLevelIndex: Integer);
begin
  if not HasNumberingProperties(ANumberingListIndex, AListLevelIndex) then
    Exit;
  WriteWpStartElement('numPr');
  try
    if AListLevelIndex > 0 then
      WriteWpIntValue('ilvl', AListLevelIndex);
    if (ANumberingListIndex >= NumberingListIndexMinValue) or (ANumberingListIndex = NumberingListIndexNoNumberingList) then
      WriteWpIntValue('numId', GetNumberingListIndexForExport(ANumberingListIndex));
  finally
    WriteWpEndElement;
  end;
end;

function TdxOpenXmlExporter.HasNumberingProperties(ANumberingListIndex: TdxNumberingListIndex; AListLevelIndex: Integer): Boolean;
begin
  Result := (ANumberingListIndex >= NumberingListIndexMinValue) or (ANumberingListIndex = NumberingListIndexNoNumberingList) or
    (AListLevelIndex > 0);
end;

procedure TdxOpenXmlExporter.GenerateNumberingContent(AWriter: TdxXmlWriter);
begin
  DocumentContentWriter := AWriter;
  ExportNumberingCore;
end;

procedure TdxOpenXmlExporter.ExportAbstractNumberingList(AList: TdxAbstractNumberingList; AId: Integer);
var
  AStyleLinkIndex, ANumberingStyleReferenceIndex: Integer;
  AShouldExportLevels: Boolean;
  AStyleId: string;
begin
  WriteWpStartElement('abstractNum');
  try
    WriteWpIntAttr('abstractNumId', AId);
    WriteWpStringValue('nsid', ConvertToHexBinary(AList.Id));
    WriteWpStringValue('multiLevelType', ConvertNumberingListType(TdxNumberingListHelper.GetListType(AList)));
    AStyleLinkIndex := AList.StyleLinkIndex;
    ANumberingStyleReferenceIndex := AList.NumberingStyleReferenceIndex;
    AShouldExportLevels := (AStyleLinkIndex >= 0) or (ANumberingStyleReferenceIndex < 0);
    if AStyleLinkIndex >= 0 then
    begin
      WriteWpStringValue('tmpl', ConvertToHexBinary(AList.TemplateCode));
      WriteWpStringValue('styleLink', GetNumberingStyleId(AStyleLinkIndex));
    end
    else
      if ANumberingStyleReferenceIndex >= 0 then
      begin
        WriteWpStringValue('tmpl', ConvertToHexBinary(AList.TemplateCode));
        AStyleId := GetNumberingStyleId(ANumberingStyleReferenceIndex);
        WriteWpStringValue('numStyleLink', AStyleId);
      end;
    if AShouldExportLevels then
      ExportLevels(AList.Levels);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxOpenXmlExporter.ExportNumberingList(AList: TdxNumberingList; AId: Integer);
begin
  WriteWpStartElement('num');
  try
    WriteWpIntAttr('numId', AId + 1);
    WriteWpIntValue('abstractNumId', AList.AbstractNumberingListIndex);
    ExportOverrideLevels(AList.Levels);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxOpenXmlExporter.ExportNumberFormatValue(AProperties: TdxListLevelProperties);
begin
  WriteWpStringValue('numFmt', ConvertNumberFormat(AProperties.Format));
end;

procedure TdxOpenXmlExporter.GenerateSettingsContent(AWriter: TdxXmlWriter);
begin
  DocumentContentWriter := AWriter;
  ExportSettingsCore;
end;

procedure TdxOpenXmlExporter.ExportSettingsCore;
var
  AVal: TdxWordProcessingMLValue;
begin
  AVal := TdxWordProcessingMLValue.Create('settings', 'docPr');
  WriteWpStartElement(GetWordProcessingMLValue(AVal));
  try
    WriteSettingsCore;
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxOpenXmlExporter.ExportDocumentProtectionSettingsCore;
var
  AProperties: TdxDocumentProtectionProperties;
begin
  AProperties := DocumentModel.ProtectionProperties;
  if (AProperties.PasswordHash = nil) or (Length(AProperties.PasswordHash) <= 0) then
    Exit;

  WriteWpStringAttr('cryptProviderType', 'rsaFull');
  WriteWpStringAttr('cryptAlgorithmClass', 'hash');
  WriteWpStringAttr('cryptAlgorithmType', 'typeAny');
  WriteWpIntAttr('cryptAlgorithmSid', Integer(AProperties.HashAlgorithmType));
  WriteWpStringAttr('cryptSpinCount', IntToStr(Max(1, AProperties.HashIterationCount)));
  if AProperties.PasswordHash <> nil then
    WriteWpStringAttr('hash', TdxBase64.ToBase64String(AProperties.PasswordHash));
  if AProperties.PasswordPrefix <> nil then
    WriteWpStringAttr('salt', TdxBase64.ToBase64String(AProperties.PasswordPrefix));
end;

procedure TdxOpenXmlExporter.ExportBookmarkStart(ABookmark: TdxBookmark);
begin
  WriteWpStartElement('bookmarkStart');
  try
    WriteWpStringAttr('id', GenerateBookmarkId(ABookmark));
    WriteWpStringAttr('name', PrepareBookmarkName(ABookmark.Name));
  finally
    WriteWpEndElement;
  end;
end;

function TdxOpenXmlExporter.PrepareFontName(const AName: string): string;
begin
  if not DocumentModel.DocumentExportOptions.OpenXml.LimitFontNameTo31Chars or (Length(AName) <= MaxFontNameLength) then
    Exit(AName);
  Result := Copy(AName, 1, MaxFontNameLength);
end;

function TdxOpenXmlExporter.PrepareBookmarkName(const AName: string): string;
begin
  if not DocumentModel.DocumentExportOptions.OpenXml.LimitBookmarkNameTo40Chars or (Length(AName) <= MaxBookmarkNameLength) then
    Exit(AName);
  Result := CutNameToPermittedLength(AName, FBookmarkNames, MaxBookmarkNameLength);
end;

function TdxOpenXmlExporter.PrepareStyleName(const AName: string): string;
begin
  if not DocumentModel.DocumentExportOptions.OpenXml.LimitStyleNameTo253Chars or (Length(AName) <= MaxStyleNameLength) then
    Exit(AName);
  Result := CutNameToPermittedLength(AName, FStyleNames, MaxStyleNameLength);
end;

function TdxOpenXmlExporter.CutNameToPermittedLength(const AName: string; ANamesHashSet: TdxStringSet; AMaxLength: Integer): string;
var
  AIndex: Integer;
  AIndexStr: string;
begin
  Result := Copy(AName, 1, AMaxLength);
  AIndex := 2;
  while ANamesHashSet.Contains(Result) do
  begin
    AIndexStr := IntToStr(AIndex);
    Result := Copy(AName, 1, AMaxLength - Length(AIndexStr)) + AIndexStr;
    Inc(AIndex);
  end;
  ANamesHashSet.Add(Result);
end;

procedure TdxOpenXmlExporter.ExportBookmarkEnd(ABookmark: TdxBookmark);
begin
  WriteWpStartElement('bookmarkEnd');
  try
    WriteWpStringAttr('id', GenerateBookmarkId(ABookmark));
  finally
    WriteWpEndElement;
  end;
end;

function TdxOpenXmlExporter.GenerateBookmarkId(ABookmark: TdxBookmark): string;
begin
  Result := IntToStr(PieceTable.Bookmarks.IndexOf(ABookmark));
end;


procedure TdxOpenXmlExporter.ExportFirstPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean);
var
  ARelationId: string;
begin
  if ALinkedToPrevious then
    Exit;
  ARelationId := ExportHeader(ASectionHeader);
  ExportHeaderReference('first', ARelationId);
end;

procedure TdxOpenXmlExporter.ExportOddPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean);
var
  ARelationId: string;
begin
  if ALinkedToPrevious then
    Exit;
  ARelationId := ExportHeader(ASectionHeader);
  ExportHeaderReference('default', ARelationId);
end;

procedure TdxOpenXmlExporter.ExportEvenPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean);
var
  ARelationId: string;
begin
  if ALinkedToPrevious then
    Exit;
  ARelationId := ExportHeader(ASectionHeader);
  ExportHeaderReference('even', ARelationId);
end;

procedure TdxOpenXmlExporter.ExportFirstPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean);
var
  ARelationId: string;
begin
  if ALinkedToPrevious then
    Exit;
  ARelationId := ExportFooter(ASectionFooter);
  ExportFooterReference('first', ARelationId);
end;

procedure TdxOpenXmlExporter.ExportOddPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean);
var
  ARelationId: string;
begin
  if ALinkedToPrevious then
    Exit;
  ARelationId := ExportFooter(ASectionFooter);
  ExportFooterReference('default', ARelationId);
end;

procedure TdxOpenXmlExporter.ExportEvenPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean);
var
  ARelationId: string;
begin
  if ALinkedToPrevious then
    Exit;
  ARelationId := ExportFooter(ASectionFooter);
  ExportFooterReference('even', ARelationId);
end;

function TdxOpenXmlExporter.ExportHeader(AHeader: TdxSectionHeader): string;
var
  AFileName, ARelsFileName, ARelationId: string;
  AContentStream: TdxCompressedStream;
begin
  ImageRelationsTableStack.Push(TdxStringsDictionary.Create);
  ExportedImageTableStack.Push(TDictionary<TdxOfficeImage, string>.Create);
  try
    Inc(FHeaderCounter);
    AFileName := Format('header%d.xml', [FHeaderCounter]);
    AContentStream := CreateCompressedXmlContent(
      procedure (AWriter: TdxXmlWriter)
      begin
        ExportHeaderCore(AWriter, AHeader);
      end);
    AddCompressedPackageContent('word\' + AFileName, AContentStream);
    RegisterContentTypeOverride('/word/' + AFileName, 'application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml');

    if ImageRelationsTable.Count > 0 then
    begin
      ARelsFileName := Format('word\_rels\header%d.xml.rels', [FHeaderCounter]);
      AddCompressedPackageContent(ARelsFileName, ExportHeaderFooterRelations);
    end;

    ARelationId := GenerateHeaderRelationId;
    FHeaderRelationsTable.Add(ARelationId, AFileName);
    Exit(ARelationId);
  finally
    ExportedImageTableStack.Pop;
    ImageRelationsTableStack.Pop;
  end;
end;

function TdxOpenXmlExporter.ExportFooter(AFooter: TdxSectionFooter): string;
var
  AFileName, ARelsFileName, ARelationId: string;
  AContentStream: TdxCompressedStream;
begin
  ImageRelationsTableStack.Push(TdxStringsDictionary.Create);
  ExportedImageTableStack.Push(TDictionary<TdxOfficeImage, string>.Create);
  try
    Inc(FFooterCounter);
    AFileName := Format('footer%d.xml', [FFooterCounter]);
    AContentStream := CreateCompressedXmlContent(
      procedure (AWriter: TdxXmlWriter)
      begin
        ExportFooterCore(AWriter, AFooter);
      end);
    AddCompressedPackageContent('word\' + AFileName, AContentStream);
    RegisterContentTypeOverride('/word/' + AFileName, 'application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml');

    if ImageRelationsTable.Count > 0 then
    begin
      ARelsFileName := Format('word\_rels\footer%d.xml.rels', [FFooterCounter]);
      AddCompressedPackageContent(ARelsFileName, ExportHeaderFooterRelations);
    end;

    ARelationId := GenerateFooterRelationId;
    FFooterRelationsTable.Add(ARelationId, AFileName);
    Exit(ARelationId);
  finally
    ExportedImageTableStack.Pop;
    ImageRelationsTableStack.Pop;
  end;
end;

function TdxOpenXmlExporter.GenerateHeaderRelationId: string;
begin
  Result := Format('RelHdr%d', [FHeaderCounter]);
end;

procedure TdxOpenXmlExporter.GenerateFileRelationsCore(AWriter: TdxXmlWriter; ARelationTable: TdxStringsDictionary;
  const ARelationType: string);
var
  ARelationId, APath: string;
begin
  for ARelationId in ARelationTable.Keys do
  begin
    APath := ARelationTable[ARelationId];
    AWriter.WriteStartElement('Relationship');
    AWriter.WriteAttributeString('Id', ARelationId);
    AWriter.WriteAttributeString('Type', ARelationType);
    AWriter.WriteAttributeString('Target', APath);
    AWriter.WriteEndElement;
  end;
end;

function TdxOpenXmlExporter.GenerateFooterRelationId: string;
begin
  Result := Format('RelFtr%d', [FFooterCounter]);
end;

procedure TdxOpenXmlExporter.ExportHeaderCore(AWriter: TdxXmlWriter; AHeader: TdxSectionHeaderFooterBase);
var
  AOldWriter: TdxXmlWriter;
begin
  AOldWriter := DocumentContentWriter;
  try
    DocumentContentWriter := AWriter;
    WriteWpStartElement('hdr');
    try
      RegisterNamespaces;
      PerformExportPieceTable(TdxPieceTable(AHeader.PieceTable), ExportPieceTable);
    finally
      WriteWpEndElement;
    end;
  finally
    DocumentContentWriter := AOldWriter;
  end;
end;

procedure TdxOpenXmlExporter.ExportFooterCore(AWriter: TdxXmlWriter; AFooter: TdxSectionFooter);
var
  AOldWriter: TdxXmlWriter;
begin
  AOldWriter := DocumentContentWriter;
  try
    DocumentContentWriter := AWriter;
    WriteWpStartElement('ftr');
    try
      RegisterNamespaces;
      PerformExportPieceTable(TdxPieceTable(AFooter.PieceTable), ExportPieceTable);
    finally
      WriteWpEndElement;
    end;
  finally
    DocumentContentWriter := AOldWriter;
  end;
end;

procedure TdxOpenXmlExporter.ExportHeaderReference(const AType: string; const AHeaderRelationId: string);
begin
  WriteWpStartElement('headerReference');
  try
    WriteWpStringAttr('type', AType);
    DocumentContentWriter.WriteAttributeString(RelsPrefix, 'id', RelsNamespace, AHeaderRelationId);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxOpenXmlExporter.ExportFooterReference(const AType: string; const AFooterRelationId: string);
begin
  WriteWpStartElement('footerReference');
  try
    WriteWpStringAttr('type', AType);
    DocumentContentWriter.WriteAttributeString(RelsPrefix, 'id', RelsNamespace, AFooterRelationId);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxOpenXmlExporter.ExportFootNoteReference(ARun: TdxFootNoteRun);
begin
  WriteWpStartElement('footnoteReference');
  try
    WriteWpIntAttr('id', ARun.NoteIndex);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxOpenXmlExporter.PopulateExportedFootNotes;
var
  ANotes: TdxFootNoteCollection;
  ACount, I: Integer;
begin
  ANotes := DocumentModel.FootNotes;
  ACount := ANotes.Count;
  for I := 0 to ACount - 1 do
    if ANotes[I].IsReferenced then
      FFootNotes.Add(ANotes[I]);
end;

function TdxOpenXmlExporter.ExportFootNotes: TdxCompressedStream;
begin
  Result := CreateCompressedXmlContent(GenerateFootNotesContent);
end;

procedure TdxOpenXmlExporter.GenerateFootNotesContent(AWriter: TdxXmlWriter);
begin
  DocumentContentWriter := AWriter;
  ExportFootNotesCore;
end;

procedure TdxOpenXmlExporter.ExportFootNotesCore;
var
  ACount, I: Integer;
begin
  WriteWpStartElement('footnotes');
  try
    RegisterNamespaces;
    ACount := FFootNotes.Count;
    for I := 0 to ACount - 1 do
      ExportFootNoteCore(DocumentContentWriter, FFootNotes[I], DocumentModel.FootNotes.IndexOf(FFootNotes[I]));
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxOpenXmlExporter.ExportEndNoteReference(ARun: TdxEndNoteRun);
begin
  WriteWpStartElement('endnoteReference');
  try
    WriteWpIntAttr('id', ARun.NoteIndex);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxOpenXmlExporter.PopulateExportedEndNotes;
var
  ANotes: TdxEndNoteCollection;
  ACount, I: Integer;
begin
  ANotes := DocumentModel.EndNotes;
  ACount := ANotes.Count;
  for I := 0 to ACount - 1 do
    if ANotes[I].IsReferenced then
      FEndNotes.Add(ANotes[I]);
end;

function TdxOpenXmlExporter.ExportEndNotes: TdxCompressedStream;
begin
  Result := CreateCompressedXmlContent(GenerateEndNotesContent);
end;

procedure TdxOpenXmlExporter.GenerateEndNotesContent(AWriter: TdxXmlWriter);
begin
  DocumentContentWriter := AWriter;
  ExportEndNotesCore;
end;

procedure TdxOpenXmlExporter.ExportEndNotesCore;
var
  ACount, I: Integer;
begin
  WriteWpStartElement('endnotes');
  try
    RegisterNamespaces;
    ACount := FEndNotes.Count;
    for I := 0 to ACount - 1 do
      ExportEndNoteCore(DocumentContentWriter, FEndNotes[I], DocumentModel.EndNotes.IndexOf(FEndNotes[I]));
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteWp14DrawingStartElement(const ATag: string);
begin

  DocumentContentWriter.WriteStartElement(WordProcessingDrawingPrefix14, ATag, '');
end;

procedure TdxOpenXmlExporter.WriteWpDrawingStartElement(const ATag: string);
begin
  DocumentContentWriter.WriteStartElement(WordProcessingDrawingPrefix, ATag, WordProcessingDrawingNamespace);
end;

procedure TdxOpenXmlExporter.WriteADrawingStartElement(const ATag: string);
begin
  DocumentContentWriter.WriteStartElement(DrawingMLPrefix, ATag, DrawingMLNamespace);
end;

procedure TdxOpenXmlExporter.WritePicDrawingStartElement(const ATag: string);
begin
  DocumentContentWriter.WriteStartElement(DrawingMLPicturePrefix, ATag, DrawingMLPictureNamespace);
end;

procedure TdxOpenXmlExporter.WriteMcStartElement(const ATag: string);
begin
  DocumentContentWriter.WriteStartElement(MCPrefix, ATag, MCNamespace);
end;

procedure TdxOpenXmlExporter.WriteWpsStartElement(const ATag: string);
begin
  DocumentContentWriter.WriteStartElement(WpsPrefix, ATag, WpsNamespace);
end;

procedure TdxOpenXmlExporter.WriteWpDrawingEndElement;
begin
  DocumentContentWriter.WriteEndElement;
end;

procedure TdxOpenXmlExporter.WriteWp14DrawingEndElement;
begin
  DocumentContentWriter.WriteEndElement;
end;

procedure TdxOpenXmlExporter.WriteADrawingEndElement;
begin
  DocumentContentWriter.WriteEndElement;
end;

procedure TdxOpenXmlExporter.WritePicDrawingEndElement;
begin
  DocumentContentWriter.WriteEndElement;
end;

procedure TdxOpenXmlExporter.WriteMcEndElement;
begin
  DocumentContentWriter.WriteEndElement;
end;

procedure TdxOpenXmlExporter.WriteWpsEndElement;
begin
  DocumentContentWriter.WriteEndElement;
end;

procedure TdxOpenXmlExporter.ExportFloatingObjectAnchorRun(ARun: TdxFloatingObjectAnchorRun);
var
  APictureContent: TdxPictureFloatingObjectContent;
  ATextBoxContent: TdxTextBoxFloatingObjectContent;
  ADrawObject: IdxOpenXMLDrawingObject;
  AContent: IdxOpenXMLDrawingObjectContent;
begin
  APictureContent := Safe<TdxPictureFloatingObjectContent>.Cast(ARun.Content);
  ATextBoxContent := Safe<TdxTextBoxFloatingObjectContent>.Cast(ARun.Content);
  if (APictureContent = nil) and (ATextBoxContent = nil) then
    Exit;

  if APictureContent <> nil then
  begin
    WriteWpStartElement('r');
    try
      ExportRunProperties(ARun);
      ADrawObject := TdxFloatingObjectDrawingObject.Create(ARun);
      AContent := TdxFloatingObjectDrawingObjectContent.Create(APictureContent);
      WriteFloatingObjectDrawing(ADrawObject, AContent);
    finally
      WriteWpEndElement;
    end;
  end
  else
  begin
    WriteWpStartElement('r');
    try
      WriteFloatingObjectAlternateContent(ARun, ATextBoxContent);
    finally
      WriteWpEndElement;
    end;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectAlternateContent(ARun: TdxFloatingObjectAnchorRun; ATextBoxContent: TdxTextBoxFloatingObjectContent);
var
  ATextBoxId: Integer;
begin
  WriteMcStartElement('AlternateContent');
  try
    ATextBoxId := WriteFloatingObjectTextBoxContent2010(ARun, ATextBoxContent);
    WriteFloatingObjectTextBoxContent2007(ARun, ATextBoxContent, ATextBoxId);
  finally
    WriteMcEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectAnchor(const ADrawingObject: IdxOpenXMLDrawingObject;
  const APictureContent: IdxOpenXMLDrawingObjectContent);
var
  AId: Integer;
  AName: string;
begin
  if ADrawingObject.IsFloatingObject then
    WriteWpDrawingStartElement('anchor')
  else
    WriteWpDrawingStartElement('inline');
  try
    AId := DrawingElementId;
    AName := GenerateFloatingObjectName(ADrawingObject.Name, 'Picture', DrawingElementId);
    IncrementDrawingElementId;

    ExportFloatingObjectProperties(ADrawingObject, AName, AId);
    WriteFloatingObjectPictureContent(APictureContent, ADrawingObject, AId);

    if ADrawingObject.IsFloatingObject then
      ExportRelativeSize(ADrawingObject);
  finally
    WriteWpDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.ExportImageAsDrawing(ARun: TdxInlinePictureRun);
var
  ADrawObject: IdxOpenXMLDrawingObject;
  AContent: IdxOpenXMLDrawingObjectContent;
begin
  if ARun.Image <> nil then
  begin
    ADrawObject := TdxInlineDrawingObject.Create(ARun);
    AContent := TdxInlineDrawingObjectContent.Create(ARun.Image);
    WriteFloatingObjectDrawing(ADrawObject, AContent);
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectDrawing(const ADrawingObject: IdxOpenXMLDrawingObject;
  const APictureContent: IdxOpenXMLDrawingObjectContent);
begin
  WriteWpStartElement('drawing');
  try
    WriteFloatingObjectAnchor(ADrawingObject, APictureContent);
  finally
    WriteWpEndElement;
  end;
end;

function TdxOpenXmlExporter.WriteFloatingObjectTextBoxContent2010(ARun: TdxFloatingObjectAnchorRun;
  ATextBoxContent: TdxTextBoxFloatingObjectContent): Integer;
var
  ADrawingObject: IdxOpenXMLDrawingObject;
begin
  WriteMcStartElement('Choice');
  try
    WriteStringValue('Requires', WpsPrefix);
    ExportRunProperties(ARun);
    ADrawingObject := TdxFloatingObjectDrawingObject.Create(ARun);
    Result := WriteFloatingObjectDrawing(ADrawingObject, ATextBoxContent);
  finally
    WriteMcEndElement;
  end;
end;

function TdxOpenXmlExporter.WriteFloatingObjectDrawing(const ADrawingObject: IdxOpenXMLDrawingObject;
  ATextBoxContent: TdxTextBoxFloatingObjectContent): Integer;
begin
  WriteWpStartElement('drawing');
  try
    Exit(WriteFloatingObjectAnchor(ADrawingObject, ATextBoxContent));
  finally
    WriteWpEndElement;
  end;
end;

function TdxOpenXmlExporter.WriteFloatingObjectAnchor(const ADrawingObject: IdxOpenXMLDrawingObject;
  ATextBoxContent: TdxTextBoxFloatingObjectContent): Integer;
var
  AName: string;
begin
  WriteWpDrawingStartElement('anchor');
  try
    Result := DrawingElementId;
    AName := GenerateFloatingObjectName(ADrawingObject.Name, 'Text Box', DrawingElementId);
    IncrementDrawingElementId;

    ExportFloatingObjectProperties(ADrawingObject, AName, Result);
    WriteFloatingObjectTextBoxContent(ATextBoxContent, ADrawingObject);

    ExportRelativeSize(ADrawingObject);
  finally
    WriteWpDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectTextBoxContent2007(ARun: TdxFloatingObjectAnchorRun; ATextBoxContent: TdxTextBoxFloatingObjectContent; ATextBoxId: Integer);
var
  AName: string;
begin
  WriteMcStartElement('Fallback');
  try
    AName := GenerateFloatingObjectName(ARun.Name, 'Text Box', ATextBoxId);

    WriteFloatingObjectPict(ARun.FloatingObjectProperties, ATextBoxContent, nil, ARun.Shape, AName);
  finally
    WriteMcEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectTextBoxContent(AContent: TdxTextBoxFloatingObjectContent; const ADrawingObject: IdxOpenXMLDrawingObject);
begin
  WriteADrawingStartElement('graphic');
  try
    WriteFloatingObjectGraphicData(AContent, ADrawingObject);
  finally
    WriteADrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectGraphicData(AContent: TdxTextBoxFloatingObjectContent; const ADrawingObject: IdxOpenXMLDrawingObject);
begin
  WriteADrawingStartElement('graphicData');
  try
    WriteStringValue('uri', WpsNamespace);
    WriteFloatingObjectWsp(AContent, ADrawingObject);
  finally
    WriteADrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectWsp(AContent: TdxTextBoxFloatingObjectContent; const ADrawingObject: IdxOpenXMLDrawingObject);
begin
  WriteWpsStartElement('wsp');
  try
    WriteFloatingObjectCNvSpPr;
    WriteFloatingObjectWpsSpPr(ADrawingObject);
    WriteFloatingObjectTxbx(AContent);
    WriteFloatingObjectBodyPr(AContent.TextBoxProperties);
  finally
    WriteWpsEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectBodyPr(AProperties: TdxTextBoxProperties);
begin
  WriteWpsStartElement('bodyPr');
  try
    if AProperties.UseWrapText then
      if AProperties.WrapText then
        WriteStringValue('wrap', 'square')
      else
        WriteStringValue('wrap', 'none');
    if AProperties.UseLeftMargin then
      WriteIntValue('lIns', UnitConverter.ModelUnitsToEmu(AProperties.LeftMargin));
    if AProperties.UseTopMargin then
      WriteIntValue('tIns', UnitConverter.ModelUnitsToEmu(AProperties.TopMargin));
    if AProperties.UseRightMargin then
      WriteIntValue('rIns', UnitConverter.ModelUnitsToEmu(AProperties.RightMargin));
    if AProperties.UseBottomMargin then
      WriteIntValue('bIns', UnitConverter.ModelUnitsToEmu(AProperties.BottomMargin));
    if AProperties.UseVerticalAlignment then
      WriteStringValue('anchor', ConvertTextBoxVerticalAlignment(AProperties.VerticalAlignment));
    if AProperties.UseUpright then
      WriteStringValue('upright', ConvertBoolToString(AProperties.Upright));

    if AProperties.UseResizeShapeToFitText then
    begin
      if AProperties.ResizeShapeToFitText then
        WriteADrawingStartElement('spAutoFit')
      else
        WriteADrawingStartElement('noAutofit');
      WriteADrawingEndElement;
    end;
  finally
    WriteWpsEndElement;
  end;
end;

function TdxOpenXmlExporter.ConvertTextBoxVerticalAlignment(AValue: TdxVerticalAlignment): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if TextBoxVerticalAlignmentTable.TryGetValue(AValue, AResult) then
    Exit(GetWordProcessingMLValue(AResult))
  else
    Exit(GetWordProcessingMLValue(VerticalAlignmentTable[TdxVerticalAlignment.Top]));
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectTxbx(AContent: TdxTextBoxFloatingObjectContent);
begin
  WriteWpsStartElement('txbx');
  try
    WriteFloatingObjectTxbxContent(AContent);
  finally
    WriteWpsEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectWpsSpPr(const ADrawingObject: IdxOpenXMLDrawingObject);
begin
  WriteWpsStartElement('spPr');
  try
    WriteFloatingObjectSpPr(ADrawingObject);
  finally
    WriteWpsEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectLn(AShape: TdxShape);
begin
  WriteADrawingStartElement('ln');
  try
    if AShape.UseOutlineWidth then
      WriteIntValue('w', ModelUnitsToEMU(AShape.OutlineWidth));

    WriteFloatingObjectSolidFill(AShape.OutlineColor);
  finally
    WriteADrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectSolidFill(AColor: TdxAlphaColor);
begin
  if TdxAlphaColors.IsTransparentOrEmpty(AColor) then
  begin
    WriteADrawingStartElement('noFill');
    WriteADrawingEndElement;
  end
  else
  begin
    WriteADrawingStartElement('solidFill');
    try
      WriteFloatingObjectSrgbClr(AColor);
    finally
      WriteADrawingEndElement;
    end;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectSrgbClr(AColor: TdxAlphaColor);
var
  AAlpha: Integer;
begin
  WriteADrawingStartElement('srgbClr');
  try
    WriteStringValue('val', ConvertColorToString(AColor));
    if TdxAlphaColors.Alpha(AColor) <> 255 then
    begin
      WriteADrawingStartElement('alpha');
      try
        AAlpha := Round(TdxAlphaColors.Alpha(AColor) / 255 * 100) * 1000;
        WriteIntValue('val', AAlpha);
      finally
        WriteADrawingEndElement;
      end;
    end;
  finally
    WriteADrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectCNvSpPr;
begin
  WriteWpsStartElement('cNvSpPr');
  WriteWpsEndElement;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectGraphicData(const AContent: IdxOpenXMLDrawingObjectContent; const ARun: IdxOpenXMLDrawingObject; AId: Integer);
begin
  WriteADrawingStartElement('graphicData');
  try
    WriteStringValue('uri', DrawingMLPictureNamespace);
    WriteFloatingObjectPic(AContent, ARun, AId);
  finally
    WriteADrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectCNvPicPr;
begin
  WritePicDrawingStartElement('cNvPicPr');
  WritePicDrawingEndElement;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectCNvPr(AId: Integer);
begin
  WritePicDrawingStartElement('cNvPr');
  try
    WriteIntValue('id', AId);
    WriteStringValue('name', 'Picture ' + IntToStr(AId));
  finally
    WritePicDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectNvPicPr(AId: Integer);
begin
  WritePicDrawingStartElement('nvPicPr');
  try
    WriteFloatingObjectCNvPr(AId);
    WriteFloatingObjectCNvPicPr;
  finally
    WritePicDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectExt(const ARun: IdxOpenXMLDrawingObject);
begin
  WriteADrawingStartElement('ext');
  try
    WriteIntValue('cx', DocumentModel.UnitConverter.ModelUnitsToEmu(Max(0, ARun.ActualSize.Width)));
    WriteIntValue('cy', DocumentModel.UnitConverter.ModelUnitsToEmu(Max(0, ARun.ActualSize.Height)));
  finally
    WriteADrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectOff;
begin
  WriteADrawingStartElement('off');
  try
    WriteIntValue('x', 0);
    WriteIntValue('y', 0);
  finally
    WriteADrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectBlip(const AContent: IdxOpenXMLDrawingObjectContent);
var
  AImageRelationId: string;
begin
  AImageRelationId := ExportImageData(AContent.Image);
  WriteADrawingStartElement('blip');
  try
    WriteStringAttr(RelsPrefix, 'embed', RelsNamespace, AImageRelationId);
  finally
    WriteADrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectStretch;
begin
  WriteADrawingStartElement('stretch');
  try
    WriteADrawingStartElement('fillRect');
    WriteADrawingEndElement;
  finally
    WriteADrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectBlipFill(const AContent: IdxOpenXMLDrawingObjectContent);
begin
  WritePicDrawingStartElement('blipFill');
  try
    WriteFloatingObjectBlip(AContent);
    WriteFloatingObjectStretch;
  finally
    WritePicDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectXfrm(const ARun: IdxOpenXMLDrawingObject);
begin
  WriteADrawingStartElement('xfrm');
  try
    if (ARun.Shape <> nil) and ARun.UseRotation then
      WriteIntValue('rot', UnitConverter.ModelUnitsToAdjAngle(ARun.Rotation));
    WriteFloatingObjectOff;
    WriteFloatingObjectExt(ARun);
  finally
    WriteADrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectPrstGeom;
begin
  WriteADrawingStartElement('prstGeom');
  try
    WriteStringValue('prst', 'rect');
  finally
    WriteADrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectPicSpPr(const ADrawingObject: IdxOpenXMLDrawingObject);
begin
  WritePicDrawingStartElement('spPr');
  try
    WriteFloatingObjectSpPr(ADrawingObject);
  finally
    WritePicDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectSpPr(const ADrawingObject: IdxOpenXMLDrawingObject);
var
  AInlineObject: TdxInlineDrawingObject;
begin
  WriteFloatingObjectXfrm(ADrawingObject);
  WriteFloatingObjectPrstGeom;

  if ADrawingObject.Shape <> nil then
  begin
    if ADrawingObject.Shape.UseFillColor then
      WriteFloatingObjectSolidFill(ADrawingObject.Shape.FillColor);
    if ADrawingObject.Shape.UseOutlineColor then
      WriteFloatingObjectLn(ADrawingObject.Shape);
  end;
  AInlineObject := TdxInlineDrawingObject(ADrawingObject);
  if AInlineObject <> nil then
    WriteFloatingObjectSolidFill(AInlineObject.FillColor);
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectPic(const AContent: IdxOpenXMLDrawingObjectContent;
  const ADrawingObject: IdxOpenXMLDrawingObject; AId: Integer);
begin
  WritePicDrawingStartElement('pic');
  try
    WriteFloatingObjectNvPicPr(AId);
    WriteFloatingObjectBlipFill(AContent);
    WriteFloatingObjectPicSpPr(ADrawingObject);
  finally
    WritePicDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteInlinePictureRunContent(ARun: TdxInlinePictureRun; AId: Integer);
begin
  WriteADrawingStartElement('graphic');
  try
  finally
    WriteADrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectPictureContent(const AContent: IdxOpenXMLDrawingObjectContent;
  const ADrawingObject: IdxOpenXMLDrawingObject; AId: Integer);
begin
  WriteADrawingStartElement('graphic');
  try
    WriteFloatingObjectGraphicData(AContent, ADrawingObject, AId);
  finally
    WriteADrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.ExportFloatingObjectProperties(const ADrawingObject: IdxOpenXMLDrawingObject;
  const AName: string; AId: Integer);
begin
  if ADrawingObject.IsFloatingObject then
  begin
    WriteIntValue('simplePos', 0);
    WriteBoolValue('allowOverlap', ADrawingObject.AllowOverlap);
    WriteBoolValue('behindDoc', ADrawingObject.IsBehindDoc);
    WriteBoolValue('layoutInCell', ADrawingObject.LayoutInTableCell);
    WriteBoolValue('locked', ADrawingObject.Locked);
    WriteIntValue('relativeHeight', ADrawingObject.ZOrder);

    if ADrawingObject.UseBottomDistance and (ADrawingObject.BottomDistance > 0) then
      WriteIntValue('distB', ModelUnitsToEMU(ADrawingObject.BottomDistance));

    if ADrawingObject.UseLeftDistance and (ADrawingObject.LeftDistance > 0) then
      WriteIntValue('distL', ModelUnitsToEMU(ADrawingObject.LeftDistance));

    if ADrawingObject.UseRightDistance and (ADrawingObject.RightDistance > 0) then
      WriteIntValue('distR', ModelUnitsToEMU(ADrawingObject.RightDistance));

    if ADrawingObject.UseTopDistance and (ADrawingObject.TopDistance > 0) then
      WriteIntValue('distT', ModelUnitsToEMU(ADrawingObject.TopDistance));

    if ADrawingObject.UseHidden then
      WriteBoolValue('hidden', ADrawingObject.Hidden);

    WriteFloatingObjectSimplePosition;
    if (ADrawingObject.HorizontalPositionAlignment = TdxFloatingObjectHorizontalPositionAlignment.None) and
        ADrawingObject.UsePercentOffset and
       (ADrawingObject.PercentOffsetX <> 0) then
      WriteFloatingObjectPercentPositionH2010(ADrawingObject)
    else
      WriteFloatingObjectPositionH(ADrawingObject);
    if (ADrawingObject.VerticalPositionAlignment = TdxFloatingObjectVerticalPositionAlignment.None) and
        ADrawingObject.UsePercentOffset and
       (ADrawingObject.PercentOffsetY <> 0) then
      WriteFloatingObjectPercentPositionV2010(ADrawingObject)
    else
      WriteFloatingObjectPositionV(ADrawingObject);
  end;
  WriteFloatingObjectExtent(ADrawingObject);
  WriteFloatingObjectEffectExtent(ADrawingObject);
  if ADrawingObject.IsFloatingObject then
    WriteFloatingObjectWrap(ADrawingObject);
  WriteFloatingObjectDocPr(AName, AId);
  WriteFloatingObjectCNvGraphicFramePr(ADrawingObject);
end;

procedure TdxOpenXmlExporter.ExportInlinePictureRun(ARun: TdxInlinePictureRun; const AName: string; AId: Integer);
begin
  WriteInlinePictureRunExtent(ARun);
  WriteInlinePictureEffectExtent(ARun);
  WriteInlinePictureDocPr(AName, AId);
  WriteInlinePictureCNvGraphicFramePr(ARun);
end;

procedure TdxOpenXmlExporter.ExportRelativeSize(const ARun: IdxOpenXMLDrawingObject);
begin
  if ARun.UseRelativeWidth then
    WriteFloatingObjectPercentWidth(ARun.RelativeWidth);
  if ARun.UseRelativeHeight then
    WriteFloatingObjectPercentHeight(ARun.RelativeHeight);
end;

procedure TdxOpenXmlExporter.WriteInlinePictureCNvGraphicFramePr(ARun: TdxInlinePictureRun);
begin
  WriteWpDrawingStartElement('cNvGraphicFramePr');
  try
    WriteInlinePictureGraphicFrameLocks(ARun);
  finally
    WriteWpDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteInlinePictureGraphicFrameLocks(ARun: TdxInlinePictureRun);
begin
  WriteADrawingStartElement('graphicFrameLocks');
  try
    WriteBoolValue('noChangeAspect', ARun.LockAspectRatio);
  finally
    WriteADrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectCNvGraphicFramePr(const ARun: IdxOpenXMLDrawingObject);
begin
  if not ARun.UseLockAspectRatio then
    Exit;

  WriteWpDrawingStartElement('cNvGraphicFramePr');
  try
    WriteFloatingObjectGraphicFrameLocks(ARun);
  finally
    WriteWpDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectGraphicFrameLocks(const ARun: IdxOpenXMLDrawingObject);
begin
  WriteADrawingStartElement('graphicFrameLocks');
  try
    WriteBoolValue('noChangeAspect', ARun.LockAspectRatio);
  finally
    WriteADrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteInlinePictureDocPr(const AName: string; AId: Integer);
begin
  WriteElementDocPrCore(AName, AId);
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectDocPr(const AName: string; AId: Integer);
begin

  WriteElementDocPrCore(AName, AId);
end;

procedure TdxOpenXmlExporter.WriteElementDocPrCore(const AName: string; AId: Integer);
begin
  WriteWpDrawingStartElement('docPr');
  try
    WriteIntValue('id', AId);
    if AName <> '' then
      WriteStringValue('name', AName);
  finally
    WriteWpDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectSimplePosition;
begin
  WriteWpDrawingStartElement('simplePos');
  try
    WriteIntValue('x', 0);
    WriteIntValue('y', 0);
  finally
    WriteWpDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectWrap(const ARun: IdxOpenXMLDrawingObject);
var
  ATextWrapType: TdxWordProcessingMLValue;
  AUseTextWrapSide: Boolean;
begin
  AUseTextWrapSide := False;
  if FloatingObjectTextWrapTypeTable.TryGetValue(ARun.TextWrapType, ATextWrapType) then
  begin
    if (ARun.TextWrapType = TdxFloatingObjectTextWrapType.Square) or
       (ARun.TextWrapType = TdxFloatingObjectTextWrapType.Through) or
       (ARun.TextWrapType = TdxFloatingObjectTextWrapType.Tight) then
      AUseTextWrapSide := True;

    WriteWpDrawingElement(ARun, ATextWrapType.OpenXmlValue, AUseTextWrapSide);
  end;
end;

procedure TdxOpenXmlExporter.WriteWpWrapPolygonElement(const ARun: IdxOpenXMLDrawingObject);
begin
  if (ARun.TextWrapType <> TdxFloatingObjectTextWrapType.Through) and (ARun.TextWrapType <> TdxFloatingObjectTextWrapType.Tight) then
    Exit;

  WriteWpDrawingStartElement('wrapPolygon');
  try
    WriteWpDrawingStart;
    WriteWpLineToDrawingElement(0, 21600);
    WriteWpLineToDrawingElement(21600, 21600);
    WriteWpLineToDrawingElement(21600, 0);
    WriteWpLineToDrawingElement(0, 0);
  finally
    WriteWpDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteWpLineToDrawingElement(X: Integer; Y: Integer);
begin
  WriteWpDrawingStartElement('lineTo');
  try
    WriteIntValue('x', X);
    WriteIntValue('y', Y);
  finally
    WriteWpDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteWpDrawingStart;
begin
  WriteWpDrawingStartElement('start');
  try
    WriteIntValue('x', 0);
    WriteIntValue('y', 0);
  finally
    WriteWpDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteWpDrawingElement(const ARun: IdxOpenXMLDrawingObject; const AElementName: string;
  AUseTextWrapSide: Boolean);
begin
  WriteWpDrawingStartElement(AElementName);
  try
    if AUseTextWrapSide then
      WriteStringValue('wrapText', ConvertFloatingObjectTextWrapSide(ARun.TextWrapSide));

    WriteWpWrapPolygonElement(ARun);
  finally
    WriteWpDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectPercentWidth(ARelativeWidth: TdxFloatingObjectRelativeWidth);
begin
  WriteWp14DrawingStartElement('sizeRelH');
  try
    WriteStringValue('relativeFrom', ConvertFloatingObjectRelativeFromHorizontalType(ARelativeWidth.From));
    WriteWp14DrawingStartElement('pctWidth');
    DocumentContentWriter.WriteString(IntToStr(ARelativeWidth.Width));
    WriteWp14DrawingEndElement;
  finally
    WriteWp14DrawingEndElement;
  end;
end;

function TdxOpenXmlExporter.ConvertFloatingObjectRelativeFromHorizontalType(AValue: TdxFloatingObjectRelativeFromHorizontal): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FloatingObjectRelativeFromHorizontalTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FloatingObjectRelativeFromHorizontalTable[TdxFloatingObjectRelativeFromHorizontal.Page]);
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectPercentHeight(ARelativeHeight: TdxFloatingObjectRelativeHeight);
begin
  WriteWp14DrawingStartElement('sizeRelV');
  try
    WriteStringValue('relativeFrom', ConvertFloatingObjectRelativeFromVerticalType(ARelativeHeight.From));
    WriteWp14DrawingStartElement('pctHeight');
    DocumentContentWriter.WriteString(IntToStr(ARelativeHeight.Height));
    WriteWp14DrawingEndElement;
  finally
    WriteWp14DrawingEndElement;
  end;
end;

function TdxOpenXmlExporter.ConvertFloatingObjectRelativeFromVerticalType(AValue: TdxFloatingObjectRelativeFromVertical): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FloatingObjectRelativeFromVerticalTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FloatingObjectRelativeFromVerticalTable[TdxFloatingObjectRelativeFromVertical.Page]);
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectPercentPositionV2010(const ADrawingObject: IdxOpenXMLDrawingObject);
begin
  WriteMcStartElement('AlternateContent');
  WriteMcStartElement('Choice');
  WriteStringValue('Requires', Wp14Prefix);

  WriteFloatingObjectPositionVCore(ADrawingObject, WriteFloatingObjectPercentVerticalOffset);
  WriteMcEndElement;
  WriteMcStartElement('Fallback');
  WriteFloatingObjectPositionV(ADrawingObject);
  WriteMcEndElement;
  WriteMcEndElement;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectPositionV(const ADrawingObject: IdxOpenXMLDrawingObject);
begin
  WriteFloatingObjectPositionVCore(ADrawingObject, WriteFloatingObjectVerticalOffset);
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectPositionVCore(const ADrawingObject: IdxOpenXMLDrawingObject;
  const AWriteVerticalOffsetAction: TdxAction<IdxOpenXMLDrawingObject>);
begin
  WriteWpDrawingStartElement('positionV');
  try
    WriteStringValue('relativeFrom', ConvertFloatingObjectVerticalPositionType(ADrawingObject.VerticalPositionType));

    if ADrawingObject.VerticalPositionAlignment <> TdxFloatingObjectVerticalPositionAlignment.None then
      WriteFloatingObjectVerticalPositionAlignment(ADrawingObject)
    else
      AWriteVerticalOffsetAction(ADrawingObject);
  finally
    WriteWpDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectVerticalOffset(const ADrawingObject: IdxOpenXMLDrawingObject);
begin
  WriteWpDrawingStartElement('posOffset');
  DocumentContentWriter.WriteString(IntToStr(DocumentModel.UnitConverter.ModelUnitsToEmu(ADrawingObject.Offset.Y)));
  WriteWpDrawingEndElement;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectPercentVerticalOffset(const ADrawingObject: IdxOpenXMLDrawingObject);
begin
  WriteWp14DrawingStartElement('pctPosVOffset');
  try
    DocumentContentWriter.WriteString(IntToStr(ADrawingObject.PercentOffset.Y));
  finally
    WriteWp14DrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectVerticalPositionAlignment(const ADrawingObject: IdxOpenXMLDrawingObject);
begin
  WriteWpDrawingStartElement('align');
  try
    DocumentContentWriter.WriteString(ConvertFloatingObjectVerticalPositionAlignment(ADrawingObject.VerticalPositionAlignment));
  finally
    WriteWpDrawingEndElement;
  end;
end;

function TdxOpenXmlExporter.ConvertFloatingObjectVerticalPositionAlignment(AValue: TdxFloatingObjectVerticalPositionAlignment): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FloatingObjectVerticalPositionAlignmentTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := '';
end;

function TdxOpenXmlExporter.ConvertFloatingObjectHorizontalPositionAlignment(AValue: TdxFloatingObjectHorizontalPositionAlignment): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FloatingObjectHorizontalPositionAlignmentTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := '';
end;

function TdxOpenXmlExporter.ConvertFloatingObjectVerticalPositionType(AValue: TdxFloatingObjectVerticalPositionType): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FloatingObjectVerticalPositionTypeTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FloatingObjectVerticalPositionTypeTable[TdxFloatingObjectVerticalPositionType.Page]);
end;

function TdxOpenXmlExporter.ConvertFloatingObjectHorizontalPositionType(AValue: TdxFloatingObjectHorizontalPositionType): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FloatingObjectHorizontalPositionTypeTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FloatingObjectHorizontalPositionTypeTable[TdxFloatingObjectHorizontalPositionType.Page]);
end;

function TdxOpenXmlExporter.ConvertFloatingObjectTextWrapSide(AValue: TdxFloatingObjectTextWrapSide): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FloatingObjectTextWrapSideTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FloatingObjectTextWrapSideTable[TdxFloatingObjectTextWrapSide.Both]);
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectPercentPositionH2010(const ADrawingObject: IdxOpenXMLDrawingObject);
begin
  WriteMcStartElement('AlternateContent');
  WriteMcStartElement('Choice');
  WriteStringValue('Requires', Wp14Prefix);

  WriteFloatingObjectPositionHCore(ADrawingObject, WriteFloatingObjectPercentHorizontalOffset);
  WriteMcEndElement;
  WriteMcStartElement('Fallback');
  WriteFloatingObjectPositionH(ADrawingObject);
  WriteMcEndElement;
  WriteMcEndElement;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectPositionH(const ADrawingObject: IdxOpenXMLDrawingObject);
begin
  WriteFloatingObjectPositionHCore(ADrawingObject, WriteFloatingObjectHorizontalOffset);
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectPositionHCore(const ADrawingObject: IdxOpenXMLDrawingObject;
  const AWriteHorizontalOffsetAction: TdxAction<IdxOpenXMLDrawingObject>);
begin
  WriteWpDrawingStartElement('positionH');
  try
    WriteStringValue('relativeFrom', ConvertFloatingObjectHorizontalPositionType(ADrawingObject.HorizontalPositionType));

    if ADrawingObject.HorizontalPositionAlignment <> TdxFloatingObjectHorizontalPositionAlignment.None then
      WriteFloatingObjectHorizontalPositionAlignment(ADrawingObject)
    else
      AWriteHorizontalOffsetAction(ADrawingObject);
  finally
    WriteWpDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectHorizontalPositionAlignment(const ADrawingObject: IdxOpenXMLDrawingObject);
begin
  WriteWpDrawingStartElement('align');
  try
    DocumentContentWriter.WriteString(ConvertFloatingObjectHorizontalPositionAlignment(ADrawingObject.HorizontalPositionAlignment));
  finally
    WriteWpDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectHorizontalOffset(const ADrawingObject: IdxOpenXMLDrawingObject);
begin
  WriteWpDrawingStartElement('posOffset');
  try
    DocumentContentWriter.WriteString(IntToStr(DocumentModel.UnitConverter.ModelUnitsToEmu(ADrawingObject.Offset.X)));
  finally
    WriteWpDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectPercentHorizontalOffset(const ADrawingObject: IdxOpenXMLDrawingObject);
begin
  WriteWp14DrawingStartElement('pctPosHOffset');
  try
    DocumentContentWriter.WriteString(IntToStr(ADrawingObject.PercentOffset.X));
  finally
    WriteWp14DrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectExtent(const ADrawingObject: IdxOpenXMLDrawingObject);
begin
  WriteWpDrawingStartElement('extent');
  try
    WriteIntValue('cx', DocumentModel.UnitConverter.ModelUnitsToEmu(Math.Max(0, ADrawingObject.ActualSize.Width)));
    WriteIntValue('cy', DocumentModel.UnitConverter.ModelUnitsToEmu(Math.Max(ADrawingObject.ActualSize.Height, 0)));
  finally
    WriteWpDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteInlinePictureRunExtent(ARun: TdxInlinePictureRun);
begin
  WriteElementExtentCore(ARun.ActualSize);
end;

procedure TdxOpenXmlExporter.WriteElementExtentCore(const AActualSize: TSize);
begin
  WriteWpDrawingStartElement('extent');
  try
    WriteIntValue('cx', DocumentModel.UnitConverter.ModelUnitsToEmu(Math.Max(0, AActualSize.Width)));
    WriteIntValue('cy', DocumentModel.UnitConverter.ModelUnitsToEmu(Math.Max(0, AActualSize.Height)));
  finally
    WriteWpDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteInlinePictureEffectExtent(ARun: TdxInlinePictureRun);
begin
  WriteWpDrawingStartElement('effectExtent');
  try
    WriteIntValue('l', 0);
    WriteIntValue('t', 0);
    WriteIntValue('r', 0);
    WriteIntValue('b', 0);
  finally
    WriteWpDrawingEndElement;
  end;
end;

procedure TdxOpenXmlExporter.WriteFloatingObjectEffectExtent(const ARun: IdxOpenXMLDrawingObject);
var
  AEffectExtent: TdxEffectExtent;
begin
  if not ARun.UseRotation then
    Exit;

  WriteWpDrawingStartElement('effectExtent');
  try

    AEffectExtent := TdxEffectExtent.Create(DocumentModel.UnitConverter.ModelUnitsToEmu(ARun.ActualSize.Width),
      DocumentModel.UnitConverter.ModelUnitsToEmu(ARun.ActualSize.Height), ARun.Rotation, DocumentModel.UnitConverter);
    AEffectExtent.Calculate;

    WriteIntValue('l', AEffectExtent.HorizontalIndent);
    WriteIntValue('t', AEffectExtent.VerticalIndent);
    WriteIntValue('r', AEffectExtent.HorizontalIndent);
    WriteIntValue('b', AEffectExtent.VerticalIndent);
  finally
    WriteWpDrawingEndElement;
  end;
end;

function TdxOpenXmlExporter.ModelUnitsToEMU(AModelUnits: Integer): Integer;
var
  ATwips: Integer;
begin
  ATwips := UnitConverter.ModelUnitsToTwips(AModelUnits);
  Result := Trunc(ATwips * 91440 / 144);
end;

function TdxOpenXmlExporter.ConvertBoolToString(AValue: Boolean): string;
begin
  if AValue then
    Result := '1'
  else
    Result := '0';
end;

function TdxOpenXmlExporter.GetAbstractNumberingId(AStyleIndex: Integer): string;
begin
  Result := 'C' + IntToStr(AStyleIndex);
end;

function TdxOpenXmlExporter.GetWordProcessingMLValue(AValue: TdxWordProcessingMLValue): string;
begin
  Result := AValue.OpenXmlValue;
end;

procedure TdxOpenXmlExporter.ExportImageReference(ARun: TdxFloatingObjectAnchorRun);
begin
end;

procedure TdxOpenXmlExporter.ExportStyleName(const AStyle: IdxStyle);
var
  AStyleName: string;
  AIndex: Integer;
  ANames: TArray<string>;
begin
  if not DocumentModel.DocumentExportOptions.OpenXml.AllowAlternateStyleNames and
     not DocumentModel.DocumentExportOptions.OpenXml.LimitStyleNameTo253Chars then
    inherited ExportStyleName(AStyle)
  else
  begin
    AStyleName := PrepareStyleName(AStyle.StyleName);
    AIndex := TdxStringHelper.IndexOf(AStyleName, ',');
    if AIndex < 0 then
      WriteWpStringValue('name', AStyleName)
    else
    begin
      AStyleName := TdxStringHelper.Trim(AStyleName, [',']);
      ANames := TdxStringHelper.Split(AStyleName, [',']);
      if Length(ANames) > 0 then
      begin
        if ANames[0] <> '' then
          WriteWpStringValue('name', ANames[0]);
        if Length(ANames) > 1 then
          WriteWpStringValue('aliases', TdxStringHelper.Join(',', Copy(ANames, 1, Length(ANames) - 1)));
      end;
    end;
  end;
end;

{ TdxExportOpenXmlFormat }

function TdxExportOpenXmlFormat.GetDocumentExporter: IdxExporter;
begin
  Result := TdxOpenXmlDocumentExporter.Create;
end;

class function TdxExportOpenXmlFormat.GetDocumentFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.OpenXml;
end;

function TdxExportOpenXmlFormat.GetExporter(ADocumentModel: TdxCustomDocumentModel;
  const AOptions: IdxExporterOptions): TdxCustomDocumentModelExporter;
begin
  Result := TdxOpenXmlExporter.Create(ADocumentModel, AOptions);
end;

end.

