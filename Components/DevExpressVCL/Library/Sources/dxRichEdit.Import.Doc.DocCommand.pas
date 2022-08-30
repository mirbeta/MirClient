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
unit dxRichEdit.Import.Doc.DocCommand;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses, dxCoreGraphics,
  dxGenerics,

  dxRichEdit.Utils.Types,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.DCO,
  dxRichEdit.Import.Doc.DocCharacterFormattingInfo,
  dxRichEdit.Import.Doc.DocStyleSheet;

type

  { TdxDocCommandFactory }

  TdxDocCommandFactory = class(TdxDocCommandFactoryBase)
  strict private
    class var
      FCommandTypes: TDictionary<SmallInt, TdxDocCommandClass>;
      FCommandOpcodes: TDictionary<TdxDocCommandClass, SmallInt>;
    class constructor Initialize;
    class destructor Finalize;
    class procedure Add(ACode: Word; ACommandClass: TdxDocCommandClass); static;
  public
    class function GetOpcodeByType(ACommandType: TClass): SmallInt; override;
    function CreateCommand(AOpcode: SmallInt): IdxDocCommand; override;
    procedure UpdatePropertyContainer(AContainer: TdxDocPropertyContainer; AChangeType: TdxChangeActionTypes); override;
    procedure ApplyDocumentProperties(AInfo: TdxSectionInfo);
  end;

  { TdxDocCommandBoolWrapperPropertyValueBase }

  TdxDocCommandBoolWrapperPropertyValueBase = class abstract(TdxDocCommand)
  strict private
    FValue: TdxDocBoolWrapper;
    function GetValue: Boolean;
    procedure SetValue(const AValue: Boolean);
  protected
    procedure SetValueTrue(APropertyContainer: TdxDocPropertyContainer); virtual; abstract;
    procedure SetValueFalse(APropertyContainer: TdxDocPropertyContainer); virtual; abstract;
    procedure SetUseValueTrue(APropertyContainer: TdxDocPropertyContainer); virtual; abstract;
    procedure SetUseValueFalse(APropertyContainer: TdxDocPropertyContainer); virtual; abstract;
    procedure LeaveValue(APropertyContainer: TdxDocPropertyContainer); virtual; abstract;
    procedure InverseValue(APropertyContainer: TdxDocPropertyContainer); virtual; abstract;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property Value: Boolean read GetValue write SetValue;
  end;

  { TdxDocCommandShadingPropertyValueBase }

  TdxDocCommandShadingPropertyValueBase = class abstract(TdxDocCommand)
  strict private
    FShadingDescriptor: TdxDocShadingDescriptor;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(const ASprm: TBytes); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property ShadingDescriptor: TdxDocShadingDescriptor read FShadingDescriptor write FShadingDescriptor;
  end;

  { TdxDocCommandAllCaps }

  TdxDocCommandAllCaps = class(TdxDocCommandBoolWrapperPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
    procedure SetValueTrue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetValueFalse(APropertyContainer: TdxDocPropertyContainer); override;
    procedure LeaveValue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure InverseValue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetUseValueTrue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetUseValueFalse(APropertyContainer: TdxDocPropertyContainer); override;
  end;

  { TdxDocCommandColorBase }

  TdxDocCommandColorBase = class abstract(TdxDocCommand)
  strict private
    FColorReference: TdxDocColorReference;
    function GetColor: TdxAlphaColor;
    procedure SetColor(const AValue: TdxAlphaColor);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(const ASprm: TBytes); override;
    procedure Write(AWriter: TBinaryWriter);  override;

    property Color: TdxAlphaColor read GetColor write SetColor;
  end;

  { TdxDocCommandBackColor }

  TdxDocCommandBackColor = class(TdxDocCommandShadingPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandForeColor }

  TdxDocCommandForeColor = class(TdxDocCommandColorBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandUnderlineColor }

  TdxDocCommandUnderlineColor = class(TdxDocCommandColorBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandBold }

  TdxDocCommandBold = class(TdxDocCommandBoolWrapperPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
    procedure SetValueTrue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetValueFalse(APropertyContainer: TdxDocPropertyContainer); override;
    procedure LeaveValue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure InverseValue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetUseValueTrue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetUseValueFalse(APropertyContainer: TdxDocPropertyContainer); override;
  end;

  { TdxDocCommandItalic }

  TdxDocCommandItalic = class(TdxDocCommandBoolWrapperPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
    procedure SetValueTrue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetValueFalse(APropertyContainer: TdxDocPropertyContainer); override;
    procedure LeaveValue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure InverseValue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetUseValueTrue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetUseValueFalse(APropertyContainer: TdxDocPropertyContainer); override;
  end;

  { TdxDocCommandShortPropertyValueBase }

  TdxDocCommandShortPropertyValueBase = class abstract(TdxDocCommand)
  strict private
    FValue: Integer;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property Value: Integer read FValue write FValue;
  end;

  { TdxDocCommandFontName }

  TdxDocCommandFontName = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandEastAsianFontName }

  TdxDocCommandEastAsianFontName = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandNonASCIIFontName }

  TdxDocCommandNonASCIIFontName = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandFontSize }

  TdxDocCommandFontSize = class(TdxDocCommand)
  strict private
    FDoubleFontSize: Integer;
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property DoubleFontSize: Integer read FDoubleFontSize write FDoubleFontSize;
  end;

  { TdxDocCommandStrike }

  TdxDocCommandStrike = class(TdxDocCommandBoolWrapperPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
    procedure SetValueTrue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetValueFalse(APropertyContainer: TdxDocPropertyContainer); override;
    procedure LeaveValue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure InverseValue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetUseValueTrue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetUseValueFalse(APropertyContainer: TdxDocPropertyContainer); override;
  end;

  { TdxDocCommandDoubleStrike }

  TdxDocCommandDoubleStrike = class(TdxDocCommandBoolWrapperPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
    procedure SetValueTrue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetValueFalse(APropertyContainer: TdxDocPropertyContainer); override;
    procedure LeaveValue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure InverseValue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetUseValueTrue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetUseValueFalse(APropertyContainer: TdxDocPropertyContainer); override;
  end;

  { TdxDocCommandHidden }

  TdxDocCommandHidden = class(TdxDocCommandBoolWrapperPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
    procedure SetValueTrue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetValueFalse(APropertyContainer: TdxDocPropertyContainer); override;
    procedure LeaveValue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure InverseValue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetUseValueTrue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetUseValueFalse(APropertyContainer: TdxDocPropertyContainer); override;
  end;

  { TdxDocCommandScript }

  TdxDocCommandScript = class(TdxDocCommand)
  strict private
    class var
      FScriptCodes: TDictionary<TdxCharacterFormattingScript, Byte>;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FScriptType: Byte;
    function GetScript: TdxCharacterFormattingScript;
    procedure SetScript(const AValue: TdxCharacterFormattingScript);
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;
    function CalcFormattingScript: TdxCharacterFormattingScript;

    property Script: TdxCharacterFormattingScript read GetScript write SetScript;
  end;

  { TdxDocCommandUnderline }

  TdxDocUnderlineInfo = record
  strict private
    FTypeCode: Byte;
    FUnderlineType: TdxUnderlineType;
  public
    constructor Create(ATypeCode: Byte; AUnderlineType: TdxUnderlineType);

    property TypeCode: Byte read FTypeCode;
    property UnderlineType: TdxUnderlineType read FUnderlineType;
  end;

  TdxDocCommandUnderline = class(TdxDocCommand)
  strict private
    class var
      FInfos: TList<TdxDocUnderlineInfo>;
      FUnderlineTypes: TDictionary<Byte, TdxUnderlineType>;
      FUnderlineTypeCodes: TDictionary<TdxUnderlineType, Byte>;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FUnderlineCode: Byte;
    FUnderlineWordsOnly: Boolean;
    FFontUnderlineType: TdxUnderlineType;
    procedure SetFontUnderlineType(const AValue: TdxUnderlineType);
    procedure SetUnderlyneWordsOnly(const AValue: Boolean);
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;
    procedure CalcUnderlineCode;

    property FontUnderlineType: TdxUnderlineType read FFontUnderlineType write SetFontUnderlineType;
    property UnderlyneWordsOnly: Boolean read FUnderlineWordsOnly write SetUnderlyneWordsOnly;
  end;

  { TdxDocCommandChangeCharacterStyle }

  TdxDocCommandChangeCharacterStyle = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandPictureBulletCharacterPosition }

  TdxDocCommandPictureBulletCharacterPosition = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;
  end;

  { TdxDocCommandPictureBulletProperties }

  TdxDocCommandPictureBulletProperties = class(TdxDocCommand)
  strict private
    FPictureBulletInformationBitField: SmallInt;
    FDefaultPicture: Boolean;
    FPictureBullet: Boolean;
    FSuppressBulletResize: Boolean;
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property DefaultPicture: Boolean read FDefaultPicture write FDefaultPicture;
    property PictureBullet: Boolean read FPictureBullet write FPictureBullet;
    property SuppressBulletResize: Boolean read FSuppressBulletResize write FSuppressBulletResize;
  end;

  { TdxDocCommandIntPropertyValueBase }

  TdxDocCommandIntPropertyValueBase = class abstract(TdxDocCommand)
  strict private
    FValue: Integer;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property Value: Integer read FValue write FValue;
  end;

  { TdxDocCommandPictureOffset }

  TdxDocCommandPictureOffset = class(TdxDocCommandIntPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandBoolPropertyValueBase }

  TdxDocCommandBoolPropertyValueBase = class abstract(TdxDocCommand)
  strict private
    FValue: Boolean;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property Value: Boolean read FValue write FValue;
  end;

  { TdxDocCommandSpecial }

  TdxDocCommandSpecial = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandBytePropertyValueBase }

  TdxDocCommandBytePropertyValueBase = class abstract(TdxDocCommand)
  strict private
    FValue: Byte;
  protected
    function GetValue: Byte; virtual;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property Value: Byte read FValue write FValue;
  end;

  { TdxDocCommandAlignment }

  TdxDocCommandAlignment = class(TdxDocCommandBytePropertyValueBase)
  strict private
    function GetAlignment: TdxParagraphAlignment;
    procedure SetAlignment(const AValue: TdxParagraphAlignment);
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;

    property Alignment: TdxParagraphAlignment read GetAlignment write SetAlignment;
  end;

  { TdxDocCommandAlignmentNew }

  TdxDocCommandAlignmentNew = class(TdxDocCommandAlignment);

  { TdxDocCommandXASPropertyValueBase }

  TdxDocCommandXASPropertyValueBase = class abstract(TdxDocCommandShortPropertyValueBase)
  public
    procedure Write(AWriter: TBinaryWriter); override;
  end;

  { TdxDocCommandFirstLineIndent }

  TdxDocCommandFirstLineIndent = class(TdxDocCommandXASPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandFirstLineIndentNew }

  TdxDocCommandFirstLineIndentNew = class(TdxDocCommandFirstLineIndent);

  { TdxDocCommandLogicalLeftIndent }

  TdxDocCommandLogicalLeftIndent = class(TdxDocCommandXASPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandPhysicalLeftIndent }

  TdxDocCommandPhysicalLeftIndent = class(TdxDocCommandLogicalLeftIndent);

  { TdxDocCommandLogicalRightIndent }

  TdxDocCommandLogicalRightIndent = class(TdxDocCommandXASPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandPhysicalRightIndent }

  TdxDocCommandPhysicalRightIndent = class(TdxDocCommandLogicalRightIndent);

  { TdxDocCommandSpacingAfter }

  TdxDocCommandSpacingAfter = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandSpacingBefore }

  TdxDocCommandSpacingBefore = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandSuppressHyphenation }

  TdxDocCommandSuppressHyphenation = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandSuppressLineNumbers }

  TdxDocCommandSuppressLineNumbers = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandContextualSpacing }

  TdxDocCommandContextualSpacing = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandPageBreakBefore }

  TdxDocCommandPageBreakBefore = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandBeforeAutoSpacing }

  TdxDocCommandBeforeAutoSpacing = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandAfterAutoSpacing }

  TdxDocCommandAfterAutoSpacing = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandKeepWithNext }

  TdxDocCommandKeepWithNext = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandKeepLinesTogether }

  TdxDocCommandKeepLinesTogether = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandWidowOrphanControl }

  TdxDocCommandWidowOrphanControl = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandOutlineLevel }

  TdxDocCommandOutlineLevel = class(TdxDocCommandBytePropertyValueBase)
  public const
    DocBodyTextLevel = 9;
  protected
    function GetChangeAction: TdxChangeActionType; override;
    function GetValue: Byte; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandParagraphShading }

  TdxDocCommandParagraphShading = class(TdxDocCommandShadingPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocParagraphTextWrapType }

  TdxDocParagraphTextWrapType = (
    Auto      = $00,
    NotBeside = $01,
    Around    = $02,
    None      = $03,
    Tight     = $04,
    Through   = $05
  );

  { TdxDocWrapTypeCalculator }

  TdxDocWrapTypeCalculator = class
  public
    class function MapToDocWrapTypeStyle(AWrapType: TdxFloatingObjectTextWrapType): TdxDocParagraphTextWrapType; overload; static;
    class function MapToWrapTypeStyle(AWrapType: TdxDocParagraphTextWrapType): TdxFloatingObjectTextWrapType; static;
    class function MapToDocWrapTypeStyle(AWrapType: TdxParagraphFrameTextWrapType): TdxDocParagraphTextWrapType; overload; static;
    class function MapToParagraphFrameWrapTypeStyle(AWrapType: TdxDocParagraphTextWrapType): TdxParagraphFrameTextWrapType; static;
  end;

  { TdxDocCommandFrameWrapType }

  TdxDocCommandFrameWrapType = class(TdxDocCommand)
  strict private
    FWrapType: TdxDocParagraphTextWrapType;
  protected
    function GetChangeAction: TdxChangeActionType; override;
    function GetValue: Byte; virtual;
  public
    procedure Read(const AData: TBytes); override;
    procedure Write(AWriter: TBinaryWriter); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;

    property WrapType: TdxDocParagraphTextWrapType read FWrapType write FWrapType;
  end;

  { TdxDocCommandFrameHeight }

  TdxDocCommandFrameHeight = class(TdxDocCommand)
  strict private
    FHeightAbs: Word;
    FValue: Integer;
    FMinHeight: Boolean;
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property Value: Integer read FValue write FValue;
    property MinHeight: Boolean read FMinHeight write FMinHeight;
  end;

  { TdxDocCommandFrameWidth }

  TdxDocCommandFrameWidth = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandFrameHorizontalPosition }

  TdxDocCommandFrameHorizontalPosition = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandFrameVerticalPosition }

  TdxDocCommandFrameVerticalPosition = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandFramePosition }

  TdxDocCommandFramePosition = class(TdxDocCommand)
  strict private
    FHorizontalAnchor: TdxHorizontalAnchorTypes;
    FVerticalAnchor: TdxVerticalAnchorTypes;
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    function DocHorizontalTypeToHorizontalType(AHorizontalAnchor: TdxHorizontalAnchorTypes): TdxParagraphFrameHorizontalPositionType;
    function DocVerticalTypeToVerticalType(AVerticalAnchorTypes: TdxVerticalAnchorTypes): TdxParagraphFrameVerticalPositionType;
    function HorizontalTypeToDocHorizontalType(AHorizontalType: TdxParagraphFrameHorizontalPositionType): TdxHorizontalAnchorTypes;
    function VerticalTypeToDocVerticalType(AVerticalType: TdxParagraphFrameVerticalPositionType): TdxVerticalAnchorTypes;
    procedure Write(AWriter: TBinaryWriter); override;
    procedure CalcAnchorTypes(AOperand: Byte);
    function CalcVerticalAnchor(AOperand: Byte): TdxVerticalAnchorTypes;
    function CalcHorizontalAnchor(AOperand: Byte): TdxHorizontalAnchorTypes;
    function GetAnchorTypeCodes: Byte;
    function GetVerticalAnchorTypeCode: Byte;
    function GetHorizontalAnchorTypeCode: Byte;

    property HorizontalAnchor: TdxHorizontalAnchorTypes read FHorizontalAnchor write FHorizontalAnchor;
    property VerticalAnchor: TdxVerticalAnchorTypes read FVerticalAnchor write FVerticalAnchor;
  end;

  { TdxDocCommandInTable }

  TdxDocCommandInTable = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandTableDepth }

  TdxDocCommandTableDepth = class(TdxDocCommandIntPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandChangeParagraphTabsBase }

  TdxDocCommandChangeParagraphTabsBase = class abstract(TdxDocCommand)
  strict private
    FOperand: TdxTabsOperandBase;
    FTabs: TdxTabFormattingInfo;
    FUnitConverter: TdxDocumentModelUnitConverter;
  protected
    function GetChangeAction: TdxChangeActionType; override;
    function CreateOperand: TdxTabsOperandBase; virtual; abstract;
    function CreateOperandFromByteArray(const ASprm: TBytes): TdxTabsOperandBase; virtual; abstract;
  public
    destructor Destroy; override;
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;
    procedure TryGetOperand;

    property Tabs: TdxTabFormattingInfo read FTabs write FTabs;
    property UnitConverter: TdxDocumentModelUnitConverter read FUnitConverter write FUnitConverter;
  end;

  { TdxDocCommandChangeParagraphTabs }

  TdxDocCommandChangeParagraphTabs = class(TdxDocCommandChangeParagraphTabsBase)
  protected
    function CreateOperand: TdxTabsOperandBase; override;
    function CreateOperandFromByteArray(const ASprm: TBytes): TdxTabsOperandBase; override;
  end;

  { TdxDocCommandListInfoIndex }

  TdxDocCommandListInfoIndex = class(TdxDocCommandShortPropertyValueBase)
  public const
    PreWord97Format = Integer($7ff);
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandListLevel }

  TdxDocCommandListLevel = class(TdxDocCommandBytePropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandLineSpacing }

  TdxDocCommandLineSpacing = class(TdxDocCommand)
  strict private
    FDocLineSpacing: SmallInt;
    FDocLineSpacingType: SmallInt;
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;
    function CalcLineSpacingType: TdxParagraphLineSpacing;
    function CalcLineSpacing(AUnitConverter: TdxDocumentModelUnitConverter): Single;

    property LineSpacing: SmallInt read FDocLineSpacing write FDocLineSpacing;
    property LineSpacingType: SmallInt read FDocLineSpacingType write FDocLineSpacingType;
  end;

  { TdxDocCommandInnerTableCell }

  TdxDocCommandInnerTableCell = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandTablePosition }

  TdxDocCommandTablePosition = class(TdxDocCommandFramePosition)
  public
    constructor Create; override;
    constructor Create(AInfo: TdxTableFloatingPositionInfo); overload;
  end;

  { TdxDocCommandInnerTableTrailer }

  TdxDocCommandInnerTableTrailer = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandTableTrailer }

  TdxDocCommandTableTrailer = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandChangeTableStyle }

  TdxDocCommandChangeTableStyle = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandInsertTableCell }

  TdxDocCommandInsertTableCell = class(TdxDocCommand)
  strict private
    FInsert: TdxInsertOperand;
    procedure SetInsert(const Value: TdxInsertOperand);
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property Insert: TdxInsertOperand read FInsert write SetInsert;
  end;

  { TdxDocCommandPreferredTableCellWidth }

  TdxDocCommandPreferredTableCellWidth = class(TdxDocCommand)
  strict private
    FTableCellWidth: TdxTableCellWidthOperand;
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property TableCellWidth: TdxTableCellWidthOperand read FTableCellWidth;
  end;

  { TdxDocCommandOverrideCellBorders }

  TdxDocCommandOverrideCellBorders = class(TdxDocCommand)
  strict private
    FBordersOverrideOperand: TdxTableBordersOverrideOperand;
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property OverriddenBorders: TdxTableBordersOverrideOperand read FBordersOverrideOperand;
  end;

  { TdxDocCommandCellSpacingPropertyValueBase }

  TdxDocCommandCellSpacingPropertyValueBase = class abstract(TdxDocCommand)
  protected
    FCellSpacing: TdxCellSpacingOperand;
    procedure ApplyCellSpacingOperand(APropertyContainer: TdxDocPropertyContainer); virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property CellSpacing: TdxCellSpacingOperand read FCellSpacing;
  end;

  { TdxDocCommandCellMargin }

  TdxDocCommandCellMargin = class(TdxDocCommandCellSpacingPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
    procedure ApplyCellSpacingOperand(APropertyContainer: TdxDocPropertyContainer); override;
  end;

  { TdxDocCommandShadingListBase }

  TdxDocCommandShadingListBase = class abstract(TdxDocCommand)
  strict private
    FCellColors: TdxAlphaColorList;
  public
    constructor Create;  override;
    destructor Destroy; override;
    procedure Read(const ASprm: TBytes); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property CellColors: TdxAlphaColorList read FCellColors;
  end;
  TdxDocCommandShadingListBaseClass = class of TdxDocCommandShadingListBase;

  { TdxDocCommandDefineTableShadingsBase }

  TdxDocCommandDefineTableShadingsBase = class abstract(TdxDocCommandShadingListBase)
  protected
    function GetStartIndex: Integer; virtual; abstract;
    function GetChangeAction: TdxChangeActionType; override;

    property StartIndex: Integer read GetStartIndex;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandDefineTableShadings }

  TdxDocCommandDefineTableShadings = class(TdxDocCommandDefineTableShadingsBase)
  protected
    function GetStartIndex: Integer; override;
  end;

  { TdxDocCommandDefineTableShadings2nd }

  TdxDocCommandDefineTableShadings2nd = class(TdxDocCommandDefineTableShadingsBase)
  protected
    function GetStartIndex: Integer; override;
  end;

  { TdxDocCommandDefineTableShadings3rd }

  TdxDocCommandDefineTableShadings3rd = class(TdxDocCommandDefineTableShadingsBase)
  protected
    function GetStartIndex: Integer; override;
  end;

  { TdxDocCommandCellRangeVerticalAlignment }

  TdxDocCommandCellRangeVerticalAlignment = class(TdxDocCommand)
  strict private
    FCellRangeVerticalAlignment: TdxCellRangeVerticalAlignmentOperand;
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property CellRangeVerticalAlignment: TdxCellRangeVerticalAlignmentOperand read FCellRangeVerticalAlignment write FCellRangeVerticalAlignment;
  end;

  { TdxDocCommandVerticalMergeTableCells }

  TdxDocCommandVerticalMergeTableCells = class(TdxDocCommand)
  strict private
    FCellIndex: Byte;
    FVerticalMerging: TdxMergingState;
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property CellIndex: Byte read FCellIndex write FCellIndex;
    property VerticalMerging: TdxMergingState read FVerticalMerging write FVerticalMerging;
  end;

  { TdxDocCommandHideCellMark }

  TdxDocCommandHideCellMark = class(TdxDocCommand)
  strict private
    FCellHideMark: TdxCellHideMarkOperand;
    procedure SetCellHideMark(const Value: TdxCellHideMarkOperand);
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property CellHideMark: TdxCellHideMarkOperand read FCellHideMark write SetCellHideMark;
  end;

  { TdxDocCommandTableRowHeight }

  TdxDocCommandTableRowHeight = class(TdxDocCommandShortPropertyValueBase)
  strict private
    FType: TdxHeightUnitType;
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;
    function CalcHeightUnitType: TdxHeightUnitType;

    property &Type: TdxHeightUnitType read FType write FType;
  end;

  { TdxDocCommandWidthUnitPropertyValueBase }

  TdxDocCommandWidthUnitPropertyValueBase = class abstract(TdxDocCommand)
  strict private
    FWidthUnit: TdxWidthUnitOperand;
  protected
    procedure ApplyWidthOperand(APropertyContainer: TdxDocPropertyContainer); virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property WidthUnit: TdxWidthUnitOperand read FWidthUnit;
  end;

  { TdxDocCommandWidthBefore }

  TdxDocCommandWidthBefore = class(TdxDocCommandWidthUnitPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
    procedure ApplyWidthOperand(APropertyContainer: TdxDocPropertyContainer); override;
  end;

  { TdxDocCommandWidthAfter }

  TdxDocCommandWidthAfter = class(TdxDocCommandWidthUnitPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
    procedure ApplyWidthOperand(APropertyContainer: TdxDocPropertyContainer); override;
  end;

  { TdxDocCommandTableDxaLeft }

  TdxDocCommandTableDxaLeft = class(TdxDocCommandXASPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandTableDxaGapHalf }

  TdxDocCommandTableDxaGapHalf = class(TdxDocCommandXASPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandPreferredTableWidth }

  TdxDocCommandPreferredTableWidth = class(TdxDocCommandWidthUnitPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
    procedure ApplyWidthOperand(APropertyContainer: TdxDocPropertyContainer); override;
  end;

  { TdxDocCommandTableAutoFit }

  TdxDocCommandTableAutoFit = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandTableBorders }

  TdxDocCommandTableBorders = class(TdxDocCommand)
  public const
    TableBordersOperandSize = Byte($30);
  strict private
    FTableBorders: TdxTableBordersOperand;
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property TableBorders: TdxTableBordersOperand read FTableBorders;
  end;

  { TdxDocCommandCellMarginDefault }

  TdxDocCommandCellMarginDefault = class(TdxDocCommandCellSpacingPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
    procedure ApplyCellSpacingOperand(APropertyContainer: TdxDocPropertyContainer); override;
  end;

  { TdxDocCommandWidthIndent }

  TdxDocCommandWidthIndent = class(TdxDocCommandWidthUnitPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
    procedure ApplyWidthOperand(APropertyContainer: TdxDocPropertyContainer); override;
  end;

  { TdxDocCommandTableAlignment }

  TdxDocCommandTableAlignment = class(TdxDocCommandShortPropertyValueBase)
  strict private
    function GetTableAlignment: TdxTableRowAlignment;
    procedure SetTableAlignment(const AValue: TdxTableRowAlignment);
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    function CalcAlignmentType: TdxTableRowAlignment;
    function CalcAlignmentTypeCode(AAlignment: TdxTableRowAlignment): Integer;

    property TableAlignment: TdxTableRowAlignment read GetTableAlignment write SetTableAlignment;
  end;

  { TdxDocCommandCellSpacing }

  TdxDocCommandCellSpacing = class(TdxDocCommandCellSpacingPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
    procedure ApplyCellSpacingOperand(APropertyContainer: TdxDocPropertyContainer); override;
  end;

  { TdxDocCommandTableBackgroundColor }

  TdxDocCommandTableBackgroundColor = class(TdxDocCommandShadingPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandTableOverlap }

  TdxDocCommandTableOverlap = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandBottomFromText }

  TdxDocCommandBottomFromText = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandLeftFromText }

  TdxDocCommandLeftFromText = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandRightFromText }

  TdxDocCommandRightFromText = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandTopFromText }

  TdxDocCommandTopFromText = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandTableHorizontalPosition }

  TdxDocCommandTableHorizontalPosition = class(TdxDocCommandShortPropertyValueBase)
  public const
    LeftAligned  = Integer($0000);
    Centered     = Integer($fffffffc);
    RightAligned = Integer($fffffff8);
    Inside       = Integer($fffffff4);
    Outside      = Integer($fffffff0);
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandTableVerticalPosition }

  TdxDocCommandTableVerticalPosition = class(TdxDocCommandShortPropertyValueBase)
  public const
    &Inline = Integer($0000);
    Top     = Integer($fffffffc);
    Center  = Integer($fffffff8);
    Bottom  = Integer($fffffff4);
    Inside  = Integer($fffffff0);
    Outside = Integer($ffffffec);
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandTableStyleRowBandSize }

  TdxDocCommandTableStyleRowBandSize = class(TdxDocCommandBytePropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandTableStyleColBandSize }

  TdxDocCommandTableStyleColBandSize = class(TdxDocCommandBytePropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandColumnCount }

  TdxDocCommandColumnCount = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;
  end;

  { TdxDocCommandColumnWidth }

  TdxDocCommandColumnWidth = class(TdxDocCommand)
  strict private
    FColumnIndex: Byte;
    FColumnWidth: SmallInt;
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property ColumnIndex: Byte read FColumnIndex write FColumnIndex;
    property ColumnWidth: SmallInt read FColumnWidth write FColumnWidth;
  end;

  { TdxDocCommandNotEvenlyColumnsSpace }

  TdxDocCommandNotEvenlyColumnsSpace = class(TdxDocCommand)
  strict private
    FColumnIndex: Byte;
    FColumnSpace: SmallInt;
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property ColumnIndex: Byte read FColumnIndex write FColumnIndex;
    property ColumnSpace: SmallInt read FColumnSpace write FColumnSpace;
  end;

  { TdxDocCommandDrawVerticalSeparator }

  TdxDocCommandDrawVerticalSeparator = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandEqualWidthColumns }

  TdxDocCommandEqualWidthColumns = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandColumnSpace }

  TdxDocCommandColumnSpace = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandFootNoteNumberingFormatBase }

  TdxDocCommandFootNoteNumberingFormatBase = class abstract(TdxDocCommandShortPropertyValueBase)
  strict private
    function GetNumberingFormat: TdxNumberingFormat;
    procedure SetNumberingFormat(const AValue: TdxNumberingFormat);
  public
    property NumberingFormat: TdxNumberingFormat read GetNumberingFormat write SetNumberingFormat;
  end;

  { TdxDocCommandFootNoteNumberingFormat }

  TdxDocCommandFootNoteNumberingFormat = class(TdxDocCommandFootNoteNumberingFormatBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandEndNoteNumberingFormat }

  TdxDocCommandEndNoteNumberingFormat = class(TdxDocCommandFootNoteNumberingFormatBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandFootNoteNumberingRestartTypeBase }

  TdxDocCommandFootNoteNumberingRestartTypeBase = class abstract(TdxDocCommandBytePropertyValueBase)
  strict private
    function GetNumberingRestartType: TdxLineNumberingRestart;
    procedure SetNumberingRestartType(const AValue: TdxLineNumberingRestart);
  public
    property NumberingRestartType: TdxLineNumberingRestart read GetNumberingRestartType write SetNumberingRestartType;
  end;

  { TdxDocCommandFootNoteNumberingRestartType }

  TdxDocCommandFootNoteNumberingRestartType = class(TdxDocCommandFootNoteNumberingRestartTypeBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandEndNoteNumberingRestartType }

  TdxDocCommandEndNoteNumberingRestartType = class(TdxDocCommandFootNoteNumberingRestartTypeBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandFootNoteStartingNumber }

  TdxDocCommandFootNoteStartingNumber = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandEndNoteStartingNumber }

  TdxDocCommandEndNoteStartingNumber = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandLeftMargin }

  TdxDocCommandLeftMargin = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandRightMargin }

  TdxDocCommandRightMargin = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandTopMargin }

  TdxDocCommandTopMargin = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandBottomMargin }

  TdxDocCommandBottomMargin = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandGutter }

  TdxDocCommandGutter = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandRTLGutter }

  TdxDocCommandRTLGutter = class(TdxDocCommand)
  strict private
    FRtlGutter: Byte;
    function GetGutterAlignment: TdxSectionGutterAlignment;
    procedure SetGutterAlignment(const AValue: TdxSectionGutterAlignment);
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;
    procedure CalcGutterAlignment(AAlignment: TdxSectionGutterAlignment);

    property GutterAlignment: TdxSectionGutterAlignment read GetGutterAlignment write SetGutterAlignment;
  end;

  { TdxDocCommandHeaderOffset }

  TdxDocCommandHeaderOffset = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandFooterOffset }

  TdxDocCommandFooterOffset = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandPageHeight }

  TdxDocCommandPageHeight = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandPageOrientation }

  TdxDocCommandPageOrientation = class(TdxDocCommandBytePropertyValueBase)
  strict private
    function GetLandscape: Boolean;
    procedure SetLandscape(const AValue: Boolean);
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    property Landscape: Boolean read GetLandscape write SetLandscape;
  end;

  { TdxDocCommandPageWidth }

  TdxDocCommandPageWidth = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandChapterHeaderStyle }

  TdxDocCommandChapterHeaderStyle = class(TdxDocCommandBytePropertyValueBase)
  strict private
    procedure SetChapterHeaderStyle(const AValue: Integer);
  private
    function GetChapterHeaderStyle: Integer;
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    property ChapterHeaderStyle: Integer read GetChapterHeaderStyle write SetChapterHeaderStyle;
  end;

  { TdxDocCommandChapterSeparator }

  TdxDocCommandChapterSeparator = class(TdxDocCommandBytePropertyValueBase)
  strict private
    class var
      FChapterSeparators: TDictionary<Byte, Char>;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    function GetChapterSeparator: Char;
    procedure SetChapterSeparator(const AValue: Char);
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    function CalcChapterSeparatorCode(AChapterSeparator: Char): Byte;
    property ChapterSeparator: Char read GetChapterSeparator write SetChapterSeparator;
  end;

  { TdxDocCommandNumberingFormat }

  TdxDocCommandNumberingFormat = class(TdxDocCommandBytePropertyValueBase)
  strict private
    function GetNumberingFormat: TdxNumberingFormat;
    procedure SetNumberingFormat(const AValue: TdxNumberingFormat);
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;

    property NumberingFormat: TdxNumberingFormat read GetNumberingFormat write SetNumberingFormat;
  end;

  { TdxDocCommandUseStartingPageNumber }

  TdxDocCommandUseStartingPageNumber = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandStartingPageNumber }

  TdxDocCommandStartingPageNumber = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandLineNumberingDistance }

  TdxDocCommandLineNumberingDistance = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandNumberingRestartType }

  TdxDocCommandNumberingRestartType = class(TdxDocCommandBytePropertyValueBase)
  strict private
    class var
      FLineNumberingRestartTypes: TDictionary<TdxLineNumberingRestart, Byte>;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    function GetNumberingRestartType: TdxLineNumberingRestart;
    procedure SetNumberingRestartType(const AValue: TdxLineNumberingRestart);
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    function CalcLineNumberingRestart: TdxLineNumberingRestart;

    property NumberingRestartType: TdxLineNumberingRestart read GetNumberingRestartType write SetNumberingRestartType;
  end;

  { TdxDocCommandStartLineNumber }

  TdxDocCommandStartLineNumber = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;
  end;

  { TdxDocCommandStep }

  TdxDocCommandStep = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandDifferentFirstPage }

  TdxDocCommandDifferentFirstPage = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandFirstPagePaperSource }

  TdxDocCommandFirstPagePaperSource = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandOnlyAllowEditingOfFormFields }

  TdxDocCommandOnlyAllowEditingOfFormFields = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandOtherPagePaperSource }

  TdxDocCommandOtherPagePaperSource = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandStartType }

  TdxDocCommandStartType = class(TdxDocCommandBytePropertyValueBase)
  strict private
    class var
      FStartTypes: TDictionary<TdxSectionStartType, Byte>;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    function GetStartType: TdxSectionStartType;
    procedure SetStartType(const AValue: TdxSectionStartType);
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    function CalcSectionStartType: TdxSectionStartType;

    property StartType: TdxSectionStartType read GetStartType write SetStartType;
  end;

  { TdxDocCommandTextDirection }

  TdxDocCommandTextDirection = class(TdxDocCommandBytePropertyValueBase)
  strict private
    FTextDirection: TdxTextDirection;
    procedure SetTextDirection(const AValue: TdxTextDirection);
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure CalcTextDirectionTypeCode;

    property TextDirection: TdxTextDirection read FTextDirection write SetTextDirection;
  end;

  { TdxDocCommandVerticalTextAlignment }

  TdxDocCommandVerticalTextAlignment = class(TdxDocCommandBytePropertyValueBase)
  strict private
    class var
      FVerticalAlignmentTypes: TDictionary<TdxVerticalAlignment, Byte>;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    function GetVerticalTextAlignment: TdxVerticalAlignment;
    procedure SetVerticalTextAlignment(const AValue: TdxVerticalAlignment);
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    function CalcVerticalAlignment: TdxVerticalAlignment;

    property VerticalTextAlignment: TdxVerticalAlignment read GetVerticalTextAlignment write SetVerticalTextAlignment;
  end;

  { TdxDocCommandReadTableProperties }

  TdxDocCommandReadTableProperties = class(TdxDocCommandIntPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandReadExtendedPropertyModifiers }

  TdxDocCommandReadExtendedPropertyModifiers = class(TdxDocCommandReadTableProperties);

  { TdxDocCommandEmpty }

  TdxDocCommandEmpty = class(TdxDocCommand)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;
  end;

  { TdxDocCommandIcoPropertyValueBase }

  TdxDocCommandIcoPropertyValueBase = class abstract(TdxDocCommand)
  strict private
    FColor: TdxAlphaColor;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property Color: TdxAlphaColor read FColor write FColor;
  end;

  { TdxDocCommandBackColorIco }

  TdxDocCommandBackColorIco = class(TdxDocCommandIcoPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandFontSizeNew }

  TdxDocCommandFontSizeNew = class(TdxDocCommandFontSize);

  { TdxDocCommandForeColorIco }

  TdxDocCommandForeColorIco = class(TdxDocCommandIcoPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandShading80PropertyValueBase }

  TdxDocCommandShading80PropertyValueBase = class abstract(TdxDocCommand)
  strict private
    FShadingDescriptor: TdxDocShadingDescriptor80;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(const ASprm: TBytes); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property ShadingDescriptor: TdxDocShadingDescriptor80 read FShadingDescriptor write FShadingDescriptor;
  end;

  { TdxDocCommandParagraphShading2 }

  TdxDocCommandParagraphShading2 = class(TdxDocCommandShading80PropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandStartingPageNumberNew }

  TdxDocCommandStartingPageNumberNew = class(TdxDocCommandIntPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandFootNotePositionBase }

  TdxDocCommandFootNotePositionBase = class abstract(TdxDocCommandBytePropertyValueBase)
  strict private
    function GetPosition: TdxFootNotePosition;
    procedure SetPosition(const AValue: TdxFootNotePosition);
  public
    property Position: TdxFootNotePosition read GetPosition write SetPosition;
  end;

  { TdxDocCommandFootNotePosition }

  TdxDocCommandFootNotePosition = class(TdxDocCommandFootNotePositionBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandEndNotePosition }

  TdxDocCommandEndNotePosition = class(TdxDocCommandFootNotePositionBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandDeleted }

  TdxDocCommandDeleted = class(TdxDocCommandBoolWrapperPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
    procedure SetValueTrue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetValueFalse(APropertyContainer: TdxDocPropertyContainer); override;
    procedure LeaveValue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure InverseValue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetUseValueTrue(APropertyContainer: TdxDocPropertyContainer); override;
    procedure SetUseValueFalse(APropertyContainer: TdxDocPropertyContainer); override;
  end;

  { TdxDocCommandChangeParagraphStyle }

  TdxDocCommandChangeParagraphStyle = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandChangeParagraphTabsClose }

  TdxDocCommandChangeParagraphTabsClose = class(TdxDocCommandChangeParagraphTabsBase)
  protected
    function CreateOperand: TdxTabsOperandBase; override;
    function CreateOperandFromByteArray(const ASprm: TBytes): TdxTabsOperandBase; override;
  end;

  { TdxDocCommandReadExtendedPropertyModifiersNew }

  TdxDocCommandReadExtendedPropertyModifiersNew = class(TdxDocCommandReadTableProperties);

  { TdxDocCommandSymbol }

  TdxDocCommandSymbol = class(TdxDocCommand)
  strict private
    FFontFamilyNameIndex: SmallInt;
    FSymbol: Char;
  protected
    function GetChangeAction: TdxChangeActionType; override;
    class function ParseUnicodeChar(AParameterValue: SmallInt): Char; static;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property FontFamilyNameIndex: SmallInt read FFontFamilyNameIndex write FFontFamilyNameIndex;
    property Symbol: Char read FSymbol write FSymbol;
  end;

  { TdxDocCommandEmbeddedObject }

  TdxDocCommandEmbeddedObject = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandBinaryData }

  TdxDocCommandBinaryData = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandOle2Object }

  TdxDocCommandOle2Object = class(TdxDocCommandBoolPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandTableDepthIncrement }

  TdxDocCommandTableDepthIncrement = class(TdxDocCommandShortPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandMergeTableCellsBase }

  TdxDocCommandMergeTableCellsBase = class abstract(TdxDocCommand)
  strict private
    FFirstMergedIndex: Byte;
    FLastMergedIndex: Byte;
  public
    procedure Read(const ASprm: TBytes); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property FirstMergedIndex: Byte read FFirstMergedIndex;
    property LastMergedIndex: Byte read FLastMergedIndex;
  end;

  { TdxDocCommandMergeTableCells }

  TdxDocCommandMergeTableCells = class(TdxDocCommandMergeTableCellsBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandSplitTableCells }

  TdxDocCommandSplitTableCells = class(TdxDocCommandMergeTableCellsBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandTableCellWidth }

  TdxDocCommandTableCellWidth = class(TdxDocCommand)
  strict private
    FColumnWidth: TdxColumnWidthOperand;
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    destructor Destroy; override;
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property ColumnWidth: TdxColumnWidthOperand read FColumnWidth write FColumnWidth;
  end;

  { TdxDocCommandTableBorders80 }

  TdxDocCommandTableBorders80 = class(TdxDocCommand)
  public const
    TableBordersOperandSize = $30;
  strict private
    FTableBorders: TdxTableBordersOperand80;
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property TableBorders: TdxTableBordersOperand80 read FTableBorders;
  end;

  { TdxDocCommandTableDefinition }

  TdxDocCommandTableDefinition = class(TdxDocCommand)
  strict private
    FTableDefinition: TdxTableDefinitionOperand;
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    destructor Destroy; override;
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;
  end;

  { TdxDocCommandTableRowBorderColorsBase }

  TdxDocCommandTableRowBorderColorsBase = class abstract(TdxDocCommand)
  strict private
    FColors: TdxDocTableBorderColorReferenceList;
  protected
    function GetChangeAction: TdxChangeActionType; override;
    procedure ExtractColors;
    property Colors: TdxDocTableBorderColorReferenceList read FColors;
  public
    destructor Destroy; override;
    procedure Read(const ASprm: TBytes); override;
    procedure Write(AWriter: TBinaryWriter); override;
  end;

  { TdxDocCommandTopBorderColors }

  TdxDocCommandTopBorderColors = class(TdxDocCommandTableRowBorderColorsBase)
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandBottomBorderColors }

  TdxDocCommandBottomBorderColors = class(TdxDocCommandTableRowBorderColorsBase)
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandLeftBorderColors }

  TdxDocCommandLeftBorderColors = class(TdxDocCommandTableRowBorderColorsBase)
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandRightBorderColors }

  TdxDocCommandRightBorderColors = class(TdxDocCommandTableRowBorderColorsBase)
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandTableShading }

  TdxDocCommandTableShading = class(TdxDocCommandShadingListBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandTableShading2 }

  TdxDocCommandTableShading2 = class(TdxDocCommandShadingListBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandTableShading3 }

  TdxDocCommandTableShading3 = class(TdxDocCommandShadingListBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandDefTableShd80 }

  TdxDocCommandDefTableShd80 = class(TdxDocCommand)
  strict private
    FColors: TdxAlphaColorList;
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Read(const ASprm: TBytes); override;
    function GetColor(ADescriptor: TdxDocShadingDescriptor80): TdxAlphaColor;
    procedure Write(AWriter: TBinaryWriter); override;
  end;

  { TdxDocCommandTablePropertiesException }

  TdxDocCommandTablePropertiesException = class(TdxDocCommand)
  strict private
    FTableAutoformatLookSpecifier: TLP;
    procedure SetTableAutoformatLookSpecifier(const Value: TLP);
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(const ASprm: TBytes); override;
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property TableAutoformatLookSpecifier: TLP read FTableAutoformatLookSpecifier write SetTableAutoformatLookSpecifier;
  end;

  { TdxDocCommandParagraphFormattingRsid }

  TdxDocCommandParagraphFormattingRsid = class(TdxDocCommandIntPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandCharacterFormattingRsid }

  TdxDocCommandCharacterFormattingRsid = class(TdxDocCommandIntPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

  { TdxDocCommandInsertTextRsid }

  TdxDocCommandInsertTextRsid = class(TdxDocCommandIntPropertyValueBase)
  protected
    function GetChangeAction: TdxChangeActionType; override;
  public
    procedure Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader); override;
  end;

implementation

uses
  Math, Contnrs,
  dxCharacters,
  dxRichEdit.Utils.Exceptions,
  dxEncoding,
  dxStringHelper,
  dxRichEdit.Import.Doc.DocPictureBulletInformation,
  dxRichEdit.Import.Doc.DocCommandHelper,
  dxRichEdit.Export.Doc.DocCharacterPropertiesActions;


{ TdxDocCommandBoolWrapperPropertyValueBase }

function TdxDocCommandBoolWrapperPropertyValueBase.GetValue: Boolean;
begin
  Result := FValue = TdxDocBoolWrapper.True;
end;

procedure TdxDocCommandBoolWrapperPropertyValueBase.SetValue(const AValue: Boolean);
begin
  if AValue then
    FValue := TdxDocBoolWrapper.True
  else
    FValue := TdxDocBoolWrapper.False;
end;

procedure TdxDocCommandBoolWrapperPropertyValueBase.Read(const ASprm: TBytes);
begin
  FValue := TdxDocBoolWrapper(ASprm[0]);
end;

procedure TdxDocCommandBoolWrapperPropertyValueBase.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  case FValue of
    TdxDocBoolWrapper.False:
      begin
        SetValueFalse(APropertyContainer);
        SetUseValueTrue(APropertyContainer);
      end;
    TdxDocBoolWrapper.True:
      begin
        SetValueTrue(APropertyContainer);
        SetUseValueTrue(APropertyContainer);
      end;
    TdxDocBoolWrapper.Leave:
      begin
        LeaveValue(APropertyContainer);
        SetUseValueFalse(APropertyContainer);
      end;
    TdxDocBoolWrapper.Inverse:
      begin
        InverseValue(APropertyContainer);
        SetUseValueTrue(APropertyContainer);
      end;
  end;
end;

procedure TdxDocCommandBoolWrapperPropertyValueBase.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), TBytes.Create(Byte(FValue))));
end;

{ TdxDocCommandShadingPropertyValueBase }

constructor TdxDocCommandShadingPropertyValueBase.Create;
begin
  ShadingDescriptor := TdxDocShadingDescriptor.Create;
end;

destructor TdxDocCommandShadingPropertyValueBase.Destroy;
begin
  ShadingDescriptor.Free;
  inherited Destroy;
end;

procedure TdxDocCommandShadingPropertyValueBase.Read(const ASprm: TBytes);
begin
  ShadingDescriptor.Free;
  ShadingDescriptor := TdxDocShadingDescriptor.FromByteArray(ASprm, 0);
end;

procedure TdxDocCommandShadingPropertyValueBase.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandFactory.GetOpcodeByType(ClassType));
  AWriter.Write(Byte(TdxDocShadingDescriptor.Size));
  ShadingDescriptor.Write(AWriter);
end;

{ TdxDocCommandAllCaps }

function TdxDocCommandAllCaps.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandAllCaps.SetValueTrue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.AllCaps := TdxDocBoolWrapper.True;
end;

procedure TdxDocCommandAllCaps.SetValueFalse(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.AllCaps := TdxDocBoolWrapper.False;
end;

procedure TdxDocCommandAllCaps.LeaveValue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.AllCaps := TdxDocBoolWrapper.Leave;
end;

procedure TdxDocCommandAllCaps.InverseValue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.AllCaps := TdxDocBoolWrapper.Inverse;
end;

procedure TdxDocCommandAllCaps.SetUseValueTrue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingOptions.UseAllCaps := True;
end;

procedure TdxDocCommandAllCaps.SetUseValueFalse(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingOptions.UseAllCaps := False;
end;

{ TdxDocCommandColorBase }

constructor TdxDocCommandColorBase.Create;
begin
  FColorReference := TdxDocColorReference.Create;
end;

destructor TdxDocCommandColorBase.Destroy;
begin
  FColorReference.Free;
  inherited Destroy;
end;

function TdxDocCommandColorBase.GetColor: TdxAlphaColor;
begin
  Result := FColorReference.Color;
end;

procedure TdxDocCommandColorBase.SetColor(const AValue: TdxAlphaColor);
begin
  FColorReference.Color := AValue;
end;

procedure TdxDocCommandColorBase.Read(const ASprm: TBytes);
begin
  FColorReference.Free;
  FColorReference := TdxDocColorReference.FromByteArray(ASprm, 0);
end;

procedure TdxDocCommandColorBase.Write(AWriter: TBinaryWriter);
var
  ASprm: TBytes;
begin
  ASprm := TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), FColorReference.GetBytes);
  AWriter.Write(ASprm);
end;

{ TdxDocCommandBackColor }

function TdxDocCommandBackColor.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandBackColor.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.BackColor := ShadingDescriptor.BackgroundColor;
  APropertyContainer.CharacterInfo.FormattingOptions.UseBackColor := True;
end;

{ TdxDocCommandForeColor }

function TdxDocCommandForeColor.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandForeColor.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.ForeColor := Color;
  APropertyContainer.CharacterInfo.FormattingOptions.UseForeColor := True;
end;

{ TdxDocCommandUnderlineColor }

function TdxDocCommandUnderlineColor.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandUnderlineColor.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.UnderlineColor := Color;
  APropertyContainer.CharacterInfo.FormattingOptions.UseUnderlineColor := True;
end;

{ TdxDocCommandBold }

function TdxDocCommandBold.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandBold.SetValueTrue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.FontBold := TdxDocBoolWrapper.True;
end;

procedure TdxDocCommandBold.SetValueFalse(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.FontBold := TdxDocBoolWrapper.False;
end;

procedure TdxDocCommandBold.LeaveValue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.FontBold := TdxDocBoolWrapper.Leave;
end;

procedure TdxDocCommandBold.InverseValue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.FontBold := TdxDocBoolWrapper.Inverse;
end;

procedure TdxDocCommandBold.SetUseValueTrue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingOptions.UseFontBold := True;
end;

procedure TdxDocCommandBold.SetUseValueFalse(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingOptions.UseFontBold := False;
end;

{ TdxDocCommandItalic }

function TdxDocCommandItalic.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandItalic.SetValueTrue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.FontItalic := TdxDocBoolWrapper.True;
end;

procedure TdxDocCommandItalic.SetValueFalse(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.FontItalic := TdxDocBoolWrapper.False;
end;

procedure TdxDocCommandItalic.LeaveValue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.FontItalic := TdxDocBoolWrapper.Leave;
end;

procedure TdxDocCommandItalic.InverseValue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.FontItalic := TdxDocBoolWrapper.Inverse;
end;

procedure TdxDocCommandItalic.SetUseValueTrue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingOptions.UseFontItalic := True;
end;

procedure TdxDocCommandItalic.SetUseValueFalse(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingOptions.UseFontItalic := False;
end;

{ TdxDocCommandShortPropertyValueBase }

procedure TdxDocCommandShortPropertyValueBase.Read(const ASprm: TBytes);
begin
  Value := PSmallInt(@ASprm[0])^;
end;

procedure TdxDocCommandShortPropertyValueBase.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), TdxByteArrayHelper.From<Word>(Word(Value))));
end;

{ TdxDocCommandFontName }

function TdxDocCommandFontName.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandFontName.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.FontFamilyNameIndex := SmallInt(Value);
  APropertyContainer.CharacterInfo.FormattingOptions.UseFontName := True;
end;

{ TdxDocCommandEastAsianFontName }

function TdxDocCommandEastAsianFontName.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.None;
end;

procedure TdxDocCommandEastAsianFontName.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
end;

{ TdxDocCommandNonASCIIFontName }

function TdxDocCommandNonASCIIFontName.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.None;
end;

procedure TdxDocCommandNonASCIIFontName.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
end;

{ TdxDocCommandFontSize }

function TdxDocCommandFontSize.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandFontSize.Read(const ASprm: TBytes);
begin
  DoubleFontSize := PWord(@ASprm[0])^;
end;

procedure TdxDocCommandFontSize.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.DoubleFontSize := Max(DoubleFontSize, 1);
  APropertyContainer.CharacterInfo.FormattingOptions.UseDoubleFontSize := True;
end;

procedure TdxDocCommandFontSize.Write(AWriter: TBinaryWriter);
var
  ASprm: TBytes;
begin
  ASprm := TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), TdxByteArrayHelper.From<Word>(Word(DoubleFontSize)));
  AWriter.Write(ASprm);
end;

{ TdxDocCommandStrike }

function TdxDocCommandStrike.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandStrike.SetValueTrue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.Strike := TdxDocBoolWrapper.True;
end;

procedure TdxDocCommandStrike.SetValueFalse(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.Strike := TdxDocBoolWrapper.False;
end;

procedure TdxDocCommandStrike.LeaveValue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.Strike := TdxDocBoolWrapper.Leave;
end;

procedure TdxDocCommandStrike.InverseValue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.Strike := TdxDocBoolWrapper.Inverse;
end;

procedure TdxDocCommandStrike.SetUseValueTrue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingOptions.UseFontStrikeoutType := True;
end;

procedure TdxDocCommandStrike.SetUseValueFalse(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingOptions.UseFontStrikeoutType := False;
end;

{ TdxDocCommandDoubleStrike }

function TdxDocCommandDoubleStrike.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandDoubleStrike.SetValueTrue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.DoubleStrike := TdxDocBoolWrapper.True;
end;

procedure TdxDocCommandDoubleStrike.SetValueFalse(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.DoubleStrike := TdxDocBoolWrapper.False;
end;

procedure TdxDocCommandDoubleStrike.LeaveValue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.DoubleStrike := TdxDocBoolWrapper.Leave;
end;

procedure TdxDocCommandDoubleStrike.InverseValue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.DoubleStrike := TdxDocBoolWrapper.Inverse;
end;

procedure TdxDocCommandDoubleStrike.SetUseValueTrue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingOptions.UseFontStrikeoutType := True;
end;

procedure TdxDocCommandDoubleStrike.SetUseValueFalse(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingOptions.UseFontStrikeoutType := False;
end;

{ TdxDocCommandHidden }

function TdxDocCommandHidden.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandHidden.SetValueTrue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.Hidden := TdxDocBoolWrapper.True;
end;

procedure TdxDocCommandHidden.SetValueFalse(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.Hidden := TdxDocBoolWrapper.False;
end;

procedure TdxDocCommandHidden.LeaveValue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.Hidden := TdxDocBoolWrapper.Leave;
end;

procedure TdxDocCommandHidden.InverseValue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.Hidden := TdxDocBoolWrapper.Inverse;
end;

procedure TdxDocCommandHidden.SetUseValueTrue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingOptions.UseHidden := True;
end;

procedure TdxDocCommandHidden.SetUseValueFalse(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.CharacterInfo.FormattingOptions.UseHidden := False;
end;

{ TdxDocCommandScript }

class constructor TdxDocCommandScript.Initialize;
begin
  FScriptCodes := TDictionary<TdxCharacterFormattingScript, Byte>.Create(3);
  FScriptCodes.Add(TdxCharacterFormattingScript.Normal, 0);
  FScriptCodes.Add(TdxCharacterFormattingScript.Superscript, 1);
  FScriptCodes.Add(TdxCharacterFormattingScript.Subscript, 2);
end;

class destructor TdxDocCommandScript.Finalize;
begin
  FScriptCodes.Free;
end;

function TdxDocCommandScript.GetScript: TdxCharacterFormattingScript;
begin
  Result := CalcFormattingScript;
end;

procedure TdxDocCommandScript.SetScript(const AValue: TdxCharacterFormattingScript);
begin
  FScriptType := FScriptCodes[AValue];
end;

function TdxDocCommandScript.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandScript.Read(const ASprm: TBytes);
begin
  FScriptType := ASprm[0];
end;

procedure TdxDocCommandScript.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.Script := Script;
  APropertyContainer.CharacterInfo.FormattingOptions.UseScript := True;
end;

procedure TdxDocCommandScript.Write(AWriter: TBinaryWriter);
var
  ASprm: TBytes;
begin
  ASprm := TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), TBytes.Create(FScriptType));
  AWriter.Write(ASprm);
end;

function TdxDocCommandScript.CalcFormattingScript: TdxCharacterFormattingScript;
begin
  if FScriptType = 1 then
    Exit(TdxCharacterFormattingScript.Superscript);
  if FScriptType = 2 then
    Exit(TdxCharacterFormattingScript.Subscript);
  Result := TdxCharacterFormattingScript.Normal;
end;

{ TdxDocUnderlineInfo }

constructor TdxDocUnderlineInfo.Create(ATypeCode: Byte;
  AUnderlineType: TdxUnderlineType);
begin
  FTypeCode := ATypeCode;
  FUnderlineType := AUnderlineType;
end;

{ TdxDocCommandUnderline }

class constructor TdxDocCommandUnderline.Initialize;
var
  I: Integer;
begin
  FInfos := TList<TdxDocUnderlineInfo>.Create;
  FInfos.Capacity := 17;

  FUnderlineTypes := TDictionary<Byte, TdxUnderlineType>.Create(17);
  FUnderlineTypeCodes := TDictionary<TdxUnderlineType, Byte>.Create(17);

  FInfos.Add(TdxDocUnderlineInfo.Create(0, TdxUnderlineType.None));
  FInfos.Add(TdxDocUnderlineInfo.Create(1, TdxUnderlineType.Single));
  FInfos.Add(TdxDocUnderlineInfo.Create(3, TdxUnderlineType.Double));
  FInfos.Add(TdxDocUnderlineInfo.Create(4, TdxUnderlineType.Dotted));
  FInfos.Add(TdxDocUnderlineInfo.Create(6, TdxUnderlineType.ThickSingle));
  FInfos.Add(TdxDocUnderlineInfo.Create(7, TdxUnderlineType.Dashed));
  FInfos.Add(TdxDocUnderlineInfo.Create(9, TdxUnderlineType.DashDotted));
  FInfos.Add(TdxDocUnderlineInfo.Create(10, TdxUnderlineType.DashDotDotted));
  FInfos.Add(TdxDocUnderlineInfo.Create(11, TdxUnderlineType.Wave));
  FInfos.Add(TdxDocUnderlineInfo.Create(20, TdxUnderlineType.ThickDotted));
  FInfos.Add(TdxDocUnderlineInfo.Create(23, TdxUnderlineType.ThickDashed));
  FInfos.Add(TdxDocUnderlineInfo.Create(25, TdxUnderlineType.ThickDashDotted));
  FInfos.Add(TdxDocUnderlineInfo.Create(26, TdxUnderlineType.ThickDashDotDotted));
  FInfos.Add(TdxDocUnderlineInfo.Create(27, TdxUnderlineType.HeavyWave));
  FInfos.Add(TdxDocUnderlineInfo.Create(39, TdxUnderlineType.LongDashed));
  FInfos.Add(TdxDocUnderlineInfo.Create(43, TdxUnderlineType.DoubleWave));
  FInfos.Add(TdxDocUnderlineInfo.Create(55, TdxUnderlineType.ThickLongDashed));
  for I := 0 to FInfos.Count - 1 do
  begin
    FUnderlineTypes.Add(FInfos[I].TypeCode, FInfos[I].UnderlineType);
    FUnderlineTypeCodes.Add(FInfos[I].UnderlineType, FInfos[I].TypeCode);
  end;
end;

class destructor TdxDocCommandUnderline.Finalize;
begin
  FInfos.Free;
  FUnderlineTypes.Free;
  FUnderlineTypeCodes.Free;
end;

procedure TdxDocCommandUnderline.SetFontUnderlineType(const AValue: TdxUnderlineType);
begin
  if FFontUnderlineType = AValue then
    Exit;
  FFontUnderlineType := AValue;
  CalcUnderlineCode;
end;

procedure TdxDocCommandUnderline.SetUnderlyneWordsOnly(const AValue: Boolean);
begin
  if FUnderlineWordsOnly = AValue then
    Exit;
  FUnderlineWordsOnly := AValue;
  CalcUnderlineCode;
end;

function TdxDocCommandUnderline.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandUnderline.Read(const ASprm: TBytes);
begin
  FUnderlineCode := ASprm[0];
end;

procedure TdxDocCommandUnderline.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  AUnderlineType: TdxUnderlineType;
begin
  if FUnderlineCode = 2 then
  begin
    APropertyContainer.CharacterInfo.FormattingInfo.UnderlineWordsOnly := True;
    APropertyContainer.CharacterInfo.FormattingInfo.FontUnderlineType := TdxUnderlineType.Single;
    APropertyContainer.CharacterInfo.FormattingOptions.UseUnderlineWordsOnly := True;
  end
  else
    if FUnderlineTypes.TryGetValue(FUnderlineCode, AUnderlineType) then
    begin
      APropertyContainer.CharacterInfo.FormattingInfo.UnderlineWordsOnly := FUnderlineWordsOnly;
      APropertyContainer.CharacterInfo.FormattingInfo.FontUnderlineType := AUnderlineType;
    end;
  APropertyContainer.CharacterInfo.FormattingOptions.UseFontUnderlineType := True;
end;

procedure TdxDocCommandUnderline.Write(AWriter: TBinaryWriter);
var
  ASprm: TBytes;
begin
  ASprm := TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), TBytes.Create(FUnderlineCode));
  AWriter.Write(ASprm);
end;

procedure TdxDocCommandUnderline.CalcUnderlineCode;
var
  AUnderlineCode: Byte;
begin
  if FUnderlineWordsOnly and (FFontUnderlineType = TdxUnderlineType.Single) then
    FUnderlineCode := 2
  else
  begin
    FUnderlineTypeCodes.TryGetValue(FFontUnderlineType, AUnderlineCode);
    FUnderlineCode := AUnderlineCode;
  end;
end;

{ TdxDocCommandChangeCharacterStyle }

function TdxDocCommandChangeCharacterStyle.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandChangeCharacterStyle.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.StyleIndex := Value;
end;

{ TdxDocCommandPictureBulletCharacterPosition }

function TdxDocCommandPictureBulletCharacterPosition.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandPictureBulletCharacterPosition.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.CharacterInfo.PictureBulletInformation.PictureCharacterPosition := Value;
end;

procedure TdxDocCommandPictureBulletCharacterPosition.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType),
    TdxByteArrayHelper.From<Integer>(Value)));
end;

{ TdxDocCommandPictureBulletProperties }

function TdxDocCommandPictureBulletProperties.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandPictureBulletProperties.Read(const ASprm: TBytes);
begin
  FPictureBulletInformationBitField := PSmallInt(@ASprm[0])^;
  PictureBullet := (FPictureBulletInformationBitField and $01) <> 0;
  SuppressBulletResize := (FPictureBulletInformationBitField and $02) <> 0;
  DefaultPicture := (FPictureBulletInformationBitField and $04) <> 0;
end;

procedure TdxDocCommandPictureBulletProperties.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  APictureBulletInformation: TdxDocPictureBulletInformation;
begin
  APictureBulletInformation := APropertyContainer.CharacterInfo.PictureBulletInformation;
  APictureBulletInformation.DefaultPicture := DefaultPicture;
  APictureBulletInformation.PictureBullet := PictureBullet;
  APictureBulletInformation.SuppressBulletResize := SuppressBulletResize;
end;

procedure TdxDocCommandPictureBulletProperties.Write(AWriter: TBinaryWriter);
begin
  if PictureBullet then
    FPictureBulletInformationBitField := FPictureBulletInformationBitField or $01;
  if SuppressBulletResize then
    FPictureBulletInformationBitField := FPictureBulletInformationBitField or $02;
  if DefaultPicture then
    FPictureBulletInformationBitField := FPictureBulletInformationBitField or $04;
  AWriter.Write(TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType),
    TdxByteArrayHelper.From<SmallInt>(FPictureBulletInformationBitField)));
end;

{ TdxDocCommandIntPropertyValueBase }

procedure TdxDocCommandIntPropertyValueBase.Read(const ASprm: TBytes);
begin
  Value := PInteger(@ASprm[0])^;
end;

procedure TdxDocCommandIntPropertyValueBase.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType),
    TdxByteArrayHelper.From<Integer>(Value)));
end;

{ TdxDocCommandPictureOffset }

function TdxDocCommandPictureOffset.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandPictureOffset.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.DataStreamOffset := Value;
  APropertyContainer.CharacterInfo.Special := True;
end;

{ TdxDocCommandBoolPropertyValueBase }

procedure TdxDocCommandBoolPropertyValueBase.Read(const ASprm: TBytes);
begin
  Value := ASprm[0] = $01;
end;

procedure TdxDocCommandBoolPropertyValueBase.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), TBytes.Create(Byte(Value))));
end;

{ TdxDocCommandSpecial }

function TdxDocCommandSpecial.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandSpecial.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.CharacterInfo.Special := Value;
end;

{ TdxDocCommandBytePropertyValueBase }

procedure TdxDocCommandBytePropertyValueBase.Read(const ASprm: TBytes);
begin
  Value := ASprm[0];
end;

procedure TdxDocCommandBytePropertyValueBase.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), TBytes.Create(GetValue)));
end;

function TdxDocCommandBytePropertyValueBase.GetValue: Byte;
begin
  Result := Value;
end;

{ TdxDocCommandAlignment }

function TdxDocCommandAlignment.GetAlignment: TdxParagraphAlignment;
begin
  Result := TdxAlignmentCalculator.CalcParagraphAlignment(Value);
end;

procedure TdxDocCommandAlignment.SetAlignment(const AValue: TdxParagraphAlignment);
begin
  Value := TdxAlignmentCalculator.CalcParagraphAlignmentCode(AValue);
end;

function TdxDocCommandAlignment.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandAlignment.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.FormattingInfo.Alignment := Alignment;
  APropertyContainer.ParagraphInfo.FormattingOptions.UseAlignment := True;
end;

{ TdxDocCommandXASPropertyValueBase }

procedure TdxDocCommandXASPropertyValueBase.Write(AWriter: TBinaryWriter);
begin
  Value := Min(Value, TdxDocConstants.MaxXASValue);
  inherited Write(AWriter);
end;

{ TdxDocCommandFirstLineIndent }

function TdxDocCommandFirstLineIndent.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandFirstLineIndent.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  AInfo: TdxParagraphFormattingInfo;
begin
  AInfo := APropertyContainer.ParagraphInfo.FormattingInfo;
  AInfo.FirstLineIndent := Abs(Value);
  if Value = 0 then
    AInfo.FirstLineIndentType := TdxParagraphFirstLineIndent.None
  else
    if (Value > 0) then
      AInfo.FirstLineIndentType := TdxParagraphFirstLineIndent.Indented
    else
      AInfo.FirstLineIndentType := TdxParagraphFirstLineIndent.Hanging;
  APropertyContainer.ParagraphInfo.FormattingOptions.UseFirstLineIndent := True;
end;

{ TdxDocCommandLogicalLeftIndent }

function TdxDocCommandLogicalLeftIndent.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandLogicalLeftIndent.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.FormattingInfo.LeftIndent := Value;
  APropertyContainer.ParagraphInfo.FormattingOptions.UseLeftIndent := True;
end;

{ TdxDocCommandLogicalRightIndent }

function TdxDocCommandLogicalRightIndent.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandLogicalRightIndent.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.FormattingInfo.RightIndent := Value;
  APropertyContainer.ParagraphInfo.FormattingOptions.UseRightIndent := True;
end;

{ TdxDocCommandSpacingAfter }

function TdxDocCommandSpacingAfter.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandSpacingAfter.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.FormattingInfo.SpacingAfter := Value;
  APropertyContainer.ParagraphInfo.FormattingOptions.UseSpacingAfter := True;
end;

{ TdxDocCommandSpacingBefore }

function TdxDocCommandSpacingBefore.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandSpacingBefore.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.FormattingInfo.SpacingBefore := Value;
  APropertyContainer.ParagraphInfo.FormattingOptions.UseSpacingBefore := True;
end;

{ TdxDocCommandSuppressHyphenation }

function TdxDocCommandSuppressHyphenation.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandSuppressHyphenation.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.FormattingInfo.SuppressHyphenation := Value;
  APropertyContainer.ParagraphInfo.FormattingOptions.UseSuppressHyphenation := True;
end;

{ TdxDocCommandSuppressLineNumbers }

function TdxDocCommandSuppressLineNumbers.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandSuppressLineNumbers.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.FormattingInfo.SuppressLineNumbers := Value;
  APropertyContainer.ParagraphInfo.FormattingOptions.UseSuppressLineNumbers := True;
end;

{ TdxDocCommandContextualSpacing }

function TdxDocCommandContextualSpacing.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandContextualSpacing.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.FormattingInfo.ContextualSpacing := Value;
  APropertyContainer.ParagraphInfo.FormattingOptions.UseContextualSpacing := True;
end;

{ TdxDocCommandPageBreakBefore }

function TdxDocCommandPageBreakBefore.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandPageBreakBefore.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.FormattingInfo.PageBreakBefore := Value;
  APropertyContainer.ParagraphInfo.FormattingOptions.UsePageBreakBefore := True;
end;

{ TdxDocCommandBeforeAutoSpacing }

function TdxDocCommandBeforeAutoSpacing.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandBeforeAutoSpacing.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.FormattingInfo.BeforeAutoSpacing := Value;
  APropertyContainer.ParagraphInfo.FormattingOptions.UseBeforeAutoSpacing := True;
end;

{ TdxDocCommandAfterAutoSpacing }

function TdxDocCommandAfterAutoSpacing.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandAfterAutoSpacing.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.FormattingInfo.AfterAutoSpacing := Value;
  APropertyContainer.ParagraphInfo.FormattingOptions.UseAfterAutoSpacing := True;
end;

{ TdxDocCommandKeepWithNext }

function TdxDocCommandKeepWithNext.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandKeepWithNext.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.FormattingInfo.KeepWithNext := Value;
  APropertyContainer.ParagraphInfo.FormattingOptions.UseKeepWithNext := True;
end;

{ TdxDocCommandKeepLinesTogether }

function TdxDocCommandKeepLinesTogether.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandKeepLinesTogether.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.FormattingInfo.KeepLinesTogether := Value;
  APropertyContainer.ParagraphInfo.FormattingOptions.UseKeepLinesTogether := True;
end;

{ TdxDocCommandWidowOrphanControl }

function TdxDocCommandWidowOrphanControl.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandWidowOrphanControl.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.FormattingInfo.WidowOrphanControl := Value;
  APropertyContainer.ParagraphInfo.FormattingOptions.UseWidowOrphanControl := True;
end;

{ TdxDocCommandOutlineLevel }

function TdxDocCommandOutlineLevel.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandOutlineLevel.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  AActualValue: Integer;
begin
  if Value < DocBodyTextLevel then
    AActualValue := Value + 1
  else
    AActualValue := 0;
  APropertyContainer.ParagraphInfo.FormattingInfo.OutlineLevel := AActualValue;
  APropertyContainer.ParagraphInfo.FormattingOptions.UseOutlineLevel := True;
end;

function TdxDocCommandOutlineLevel.GetValue: Byte;
begin
  if Value = 0 then
    Result := DocBodyTextLevel
  else
    Result := Byte(Value - 1);
end;

{ TdxDocCommandParagraphShading }

function TdxDocCommandParagraphShading.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandParagraphShading.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.FormattingInfo.BackColor := ShadingDescriptor.BackgroundColor;
  APropertyContainer.ParagraphInfo.FormattingOptions.UseBackColor := True;
end;

{ TdxDocWrapTypeCalculator }

class function TdxDocWrapTypeCalculator.MapToDocWrapTypeStyle(AWrapType: TdxFloatingObjectTextWrapType): TdxDocParagraphTextWrapType;
begin
  case AWrapType of
    TdxFloatingObjectTextWrapType.TopAndBottom:
      Result := TdxDocParagraphTextWrapType.NotBeside;
    TdxFloatingObjectTextWrapType.Square:
      Result := TdxDocParagraphTextWrapType.Around;
    TdxFloatingObjectTextWrapType.Tight:
      Result := TdxDocParagraphTextWrapType.Tight;
    TdxFloatingObjectTextWrapType.Through:
      Result := TdxDocParagraphTextWrapType.Through;
    else
      Result := TdxDocParagraphTextWrapType.None;
  end;
end;

class function TdxDocWrapTypeCalculator.MapToWrapTypeStyle(AWrapType: TdxDocParagraphTextWrapType): TdxFloatingObjectTextWrapType;
begin
  case AWrapType of
    TdxDocParagraphTextWrapType.NotBeside:
      Result := TdxFloatingObjectTextWrapType.TopAndBottom;
    TdxDocParagraphTextWrapType.Around:
      Result := TdxFloatingObjectTextWrapType.Square;
    TdxDocParagraphTextWrapType.Tight:
      Result := TdxFloatingObjectTextWrapType.Tight;
    TdxDocParagraphTextWrapType.Through:
      Result := TdxFloatingObjectTextWrapType.Through;
    else
      Result := TdxFloatingObjectTextWrapType.None;
  end;
end;

class function TdxDocWrapTypeCalculator.MapToDocWrapTypeStyle(AWrapType: TdxParagraphFrameTextWrapType): TdxDocParagraphTextWrapType;
begin
  case AWrapType of
    TdxParagraphFrameTextWrapType.Auto:
      Result := TdxDocParagraphTextWrapType.Auto;
    TdxParagraphFrameTextWrapType.Around:
      Result := TdxDocParagraphTextWrapType.Around;
    TdxParagraphFrameTextWrapType.NotBeside:
      Result := TdxDocParagraphTextWrapType.NotBeside;
    TdxParagraphFrameTextWrapType.Through:
      Result := TdxDocParagraphTextWrapType.Through;
    TdxParagraphFrameTextWrapType.Tight:
      Result := TdxDocParagraphTextWrapType.Tight;
    else
      Result := TdxDocParagraphTextWrapType.None;
  end;
end;

class function TdxDocWrapTypeCalculator.MapToParagraphFrameWrapTypeStyle(AWrapType: TdxDocParagraphTextWrapType): TdxParagraphFrameTextWrapType;
begin
  case AWrapType of
    TdxDocParagraphTextWrapType.Auto:
      Result := TdxParagraphFrameTextWrapType.Auto;
    TdxDocParagraphTextWrapType.Around:
      Result := TdxParagraphFrameTextWrapType.Around;
    TdxDocParagraphTextWrapType.NotBeside:
      Result := TdxParagraphFrameTextWrapType.NotBeside;
    TdxDocParagraphTextWrapType.Through:
      Result := TdxParagraphFrameTextWrapType.Through;
    TdxDocParagraphTextWrapType.Tight:
      Result := TdxParagraphFrameTextWrapType.Tight;
    else
      Result := TdxParagraphFrameTextWrapType.None;
  end;
end;

{ TdxDocCommandFrameWrapType }

function TdxDocCommandFrameWrapType.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Frame;
end;

procedure TdxDocCommandFrameWrapType.Read(const AData: TBytes);
begin
  FWrapType := TdxDocParagraphTextWrapType(AData[0]);
end;

procedure TdxDocCommandFrameWrapType.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), TBytes.Create(GetValue)));
end;

function TdxDocCommandFrameWrapType.GetValue: Byte;
begin
  Result := Byte(FWrapType);
end;

procedure TdxDocCommandFrameWrapType.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.FrameInfo.FormattingInfo.TextWrapType := TdxDocWrapTypeCalculator.MapToParagraphFrameWrapTypeStyle(WrapType);
  APropertyContainer.FrameInfo.FormattingOptions.UseTextWrapType := True;
end;

{ TdxDocCommandFrameHeight }

procedure TdxDocCommandFrameHeight.Read(const ASprm: TBytes);
begin
  FHeightAbs := PWord(@ASprm[0])^;
  Value := FHeightAbs and $00007fff;
  MinHeight := (FHeightAbs and $08000) <> 0;
end;

procedure TdxDocCommandFrameHeight.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.FrameInfo.FormattingInfo.Height := Value;
  APropertyContainer.FrameInfo.FormattingOptions.UseHeight := True;
  if Value <> 0 then
  begin
    if MinHeight then
      APropertyContainer.FrameInfo.FormattingInfo.HorizontalRule := TdxParagraphFrameHorizontalRule.AtLeast
    else
      APropertyContainer.FrameInfo.FormattingInfo.HorizontalRule := TdxParagraphFrameHorizontalRule.Exact;
    APropertyContainer.FrameInfo.FormattingOptions.UseHorizontalRule := True;
  end;
end;

procedure TdxDocCommandFrameHeight.Write(AWriter: TBinaryWriter);
begin
  FHeightAbs := Word(Value);
  if MinHeight then
    FHeightAbs := FHeightAbs or $08000;
  AWriter.Write(TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), TdxByteArrayHelper.From<Word>(FHeightAbs)));
end;

function TdxDocCommandFrameHeight.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Frame;
end;

{ TdxDocCommandFrameWidth }

function TdxDocCommandFrameWidth.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Frame;
end;

procedure TdxDocCommandFrameWidth.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.FrameInfo.FormattingInfo.Width := Value;
  APropertyContainer.FrameInfo.FormattingOptions.UseWidth := True;
end;

{ TdxDocCommandFrameHorizontalPosition }

function TdxDocCommandFrameHorizontalPosition.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Frame;
end;

procedure TdxDocCommandFrameHorizontalPosition.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.FrameInfo.FormattingInfo.X := Value;
  APropertyContainer.FrameInfo.FormattingOptions.UseX := True;
end;

{ TdxDocCommandFrameVerticalPosition }

function TdxDocCommandFrameVerticalPosition.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Frame;
end;

procedure TdxDocCommandFrameVerticalPosition.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.FrameInfo.FormattingInfo.Y := Value;
  APropertyContainer.FrameInfo.FormattingOptions.UseY := True;
end;

{ TdxDocCommandFramePosition }

function TdxDocCommandFramePosition.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandFramePosition.Read(const ASprm: TBytes);
begin
  CalcAnchorTypes(ASprm[0]);
end;

procedure TdxDocCommandFramePosition.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  APosition: TdxTableFloatingPosition;
  AFrameFormattingInfo: TdxParagraphFrameFormattingInfo;
  AFrameFormattingOptions: TdxParagraphFrameFormattingOptions;
begin
  APosition := APropertyContainer.TableInfo.TableProperties.FloatingPosition;
  APosition.TextWrapping := TdxTextWrapping.Around;
  APosition.HorizontalAnchor := HorizontalAnchor;
  APosition.VerticalAnchor := VerticalAnchor;

  AFrameFormattingInfo := APropertyContainer.FrameInfo.FormattingInfo;
  AFrameFormattingInfo.HorizontalPositionType := DocHorizontalTypeToHorizontalType(HorizontalAnchor);
  AFrameFormattingInfo.VerticalPositionType := DocVerticalTypeToVerticalType(VerticalAnchor);
  AFrameFormattingOptions := APropertyContainer.FrameInfo.FormattingOptions;
  AFrameFormattingOptions.UseHorizontalPositionType := True;
  AFrameFormattingOptions.UseVerticalPositionType := True;
end;

function TdxDocCommandFramePosition.DocHorizontalTypeToHorizontalType(AHorizontalAnchor: TdxHorizontalAnchorTypes): TdxParagraphFrameHorizontalPositionType;
begin
  case AHorizontalAnchor of
    TdxHorizontalAnchorTypes.Margin:
      Result := TdxParagraphFrameHorizontalPositionType.Margin;
    TdxHorizontalAnchorTypes.Page:
      Result := TdxParagraphFrameHorizontalPositionType.Page;
    else
      Result := TdxParagraphFrameHorizontalPositionType.Column;
  end;
end;

function TdxDocCommandFramePosition.DocVerticalTypeToVerticalType(AVerticalAnchorTypes: TdxVerticalAnchorTypes): TdxParagraphFrameVerticalPositionType;
begin
  case AVerticalAnchorTypes of
    TdxVerticalAnchorTypes.Margin:
      Result := TdxParagraphFrameVerticalPositionType.Margin;
    TdxVerticalAnchorTypes.Page:
      Result := TdxParagraphFrameVerticalPositionType.Page;
    else
      Result := TdxParagraphFrameVerticalPositionType.Paragraph;
  end;
end;

function TdxDocCommandFramePosition.HorizontalTypeToDocHorizontalType(AHorizontalType: TdxParagraphFrameHorizontalPositionType): TdxHorizontalAnchorTypes;
begin
  case AHorizontalType of
    TdxParagraphFrameHorizontalPositionType.Margin:
      Result := TdxHorizontalAnchorTypes.Margin;
    TdxParagraphFrameHorizontalPositionType.Page:
      Result := TdxHorizontalAnchorTypes.Page;
    else
      Result := TdxHorizontalAnchorTypes.Column;
  end;
end;

function TdxDocCommandFramePosition.VerticalTypeToDocVerticalType(AVerticalType: TdxParagraphFrameVerticalPositionType): TdxVerticalAnchorTypes;
begin
  case AVerticalType of
    TdxParagraphFrameVerticalPositionType.Margin:
      Result := TdxVerticalAnchorTypes.Margin;
    TdxParagraphFrameVerticalPositionType.Page:
      Result := TdxVerticalAnchorTypes.Page;
    else
      Result := TdxVerticalAnchorTypes.Paragraph;
  end;
end;

procedure TdxDocCommandFramePosition.Write(AWriter: TBinaryWriter);
var
  ASprm: TBytes;
begin
  ASprm := TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), TBytes.Create(GetAnchorTypeCodes));
  AWriter.Write(ASprm);
end;

procedure TdxDocCommandFramePosition.CalcAnchorTypes(AOperand: Byte);
begin
  VerticalAnchor := CalcVerticalAnchor(AOperand);
  HorizontalAnchor := CalcHorizontalAnchor(AOperand);
end;

function TdxDocCommandFramePosition.CalcVerticalAnchor(AOperand: Byte): TdxVerticalAnchorTypes;
var
  AVerticalAnchorTypeCode: Byte;
begin
  AVerticalAnchorTypeCode := Byte((AOperand and $30) shr 4);
  case AVerticalAnchorTypeCode of
    0:
      Result := TdxVerticalAnchorTypes.Margin;
    1:
      Result := TdxVerticalAnchorTypes.Page;
    2:
      Result := TdxVerticalAnchorTypes.Paragraph;
    else
      Result := TdxVerticalAnchorTypes.Page;
  end;
end;

function TdxDocCommandFramePosition.CalcHorizontalAnchor(AOperand: Byte): TdxHorizontalAnchorTypes;
var
  AHorizontalAnchorTypeCode: Byte;
begin
  AHorizontalAnchorTypeCode := Byte((AOperand and $c0) shr 6);
  case AHorizontalAnchorTypeCode of
    0:
      Result := TdxHorizontalAnchorTypes.Column;
    1:
      Result := TdxHorizontalAnchorTypes.Margin;
    2:
      Result := TdxHorizontalAnchorTypes.Page;
    else
      Result := TdxHorizontalAnchorTypes.Column;
  end;
end;

function TdxDocCommandFramePosition.GetAnchorTypeCodes: Byte;
begin
  Result := Byte((GetVerticalAnchorTypeCode shl 4) or (GetHorizontalAnchorTypeCode shl 6));
end;

function TdxDocCommandFramePosition.GetVerticalAnchorTypeCode: Byte;
begin
  case VerticalAnchor of
    TdxVerticalAnchorTypes.Margin:
      Result := 0;
    TdxVerticalAnchorTypes.Page:
      Result := 1;
    TdxVerticalAnchorTypes.Paragraph:
      Result := 2;
    else
      Result := 3;
  end;
end;

function TdxDocCommandFramePosition.GetHorizontalAnchorTypeCode: Byte;
begin
  case HorizontalAnchor of
    TdxHorizontalAnchorTypes.Column:
      Result := 0;
    TdxHorizontalAnchorTypes.Margin:
      Result := 1;
    TdxHorizontalAnchorTypes.Page:
      Result := 2;
    else
      Result := 3;
  end;
end;

{ TdxDocCommandInTable }

function TdxDocCommandInTable.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandInTable.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.InTable := Value;
end;

{ TdxDocCommandTableDepth }

function TdxDocCommandTableDepth.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandTableDepth.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.TableDepth := Value;
end;

{ TdxDocCommandChangeParagraphTabsBase }

destructor TdxDocCommandChangeParagraphTabsBase.Destroy;
begin
  FOperand.Free;
  inherited Destroy;
end;

function TdxDocCommandChangeParagraphTabsBase.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandChangeParagraphTabsBase.Read(const ASprm: TBytes);
begin
  FOperand := CreateOperandFromByteArray(ASprm);
end;

procedure TdxDocCommandChangeParagraphTabsBase.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  FOperand.AddTabs(APropertyContainer.ParagraphInfo.Tabs, APropertyContainer.UnitConverter);
end;

procedure TdxDocCommandChangeParagraphTabsBase.Write(AWriter: TBinaryWriter);
begin
  TryGetOperand;
  if FOperand <> nil then
  begin
    AWriter.Write(TdxDocCommandFactory.GetOpcodeByType(ClassType));
    FOperand.Write(AWriter);
  end;
end;

procedure TdxDocCommandChangeParagraphTabsBase.TryGetOperand;
begin
  if (FTabs <> nil) and (UnitConverter <> nil) then
  begin
    FOperand := CreateOperand;
    FOperand.ConvertFromFormattingTabInfo(FTabs, UnitConverter);
  end;
end;

{ TdxDocCommandChangeParagraphTabs }

function TdxDocCommandChangeParagraphTabs.CreateOperand: TdxTabsOperandBase;
begin
  Result := TdxTabsOperand.Create;
end;

function TdxDocCommandChangeParagraphTabs.CreateOperandFromByteArray(const ASprm: TBytes): TdxTabsOperandBase;
begin
  Result := TdxTabsOperand.FromByteArray(ASprm);
end;

{ TdxDocCommandListInfoIndex }

function TdxDocCommandListInfoIndex.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandListInfoIndex.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  if Value <> PreWord97Format then
    APropertyContainer.ParagraphInfo.ListInfoIndex := Value;
end;

{ TdxDocCommandListLevel }

function TdxDocCommandListLevel.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandListLevel.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.ListLevel := Value;
end;

{ TdxDocCommandLineSpacing }

function TdxDocCommandLineSpacing.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandLineSpacing.Read(const ASprm: TBytes);
begin
  FDocLineSpacing := PSmallInt(@ASprm[0])^;
  FDocLineSpacingType := PSmallInt(@ASprm[2])^;
end;

procedure TdxDocCommandLineSpacing.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.FormattingInfo.LineSpacingType := CalcLineSpacingType;
  APropertyContainer.ParagraphInfo.FormattingInfo.LineSpacing := CalcLineSpacing(APropertyContainer.UnitConverter);
  APropertyContainer.ParagraphInfo.FormattingOptions.UseLineSpacing := True;
end;

procedure TdxDocCommandLineSpacing.Write(AWriter: TBinaryWriter);
var
  AOperand: TBytes;
begin
  SetLength(AOperand, 4);
  Move(FDocLineSpacing, AOperand[0], SizeOf(SmallInt));
  Move(FDocLineSpacingType, AOperand[2], SizeOf(SmallInt));
  AWriter.Write(TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), AOperand));
end;

function TdxDocCommandLineSpacing.CalcLineSpacingType: TdxParagraphLineSpacing;
begin
  if FDocLineSpacingType = 0 then
    if (FDocLineSpacing >= 0) then
      Result := TdxParagraphLineSpacing.AtLeast
    else
      Result := TdxParagraphLineSpacing.Exactly
  else
    if FDocLineSpacing = 240 then
      Result := TdxParagraphLineSpacing.Single
    else
      if FDocLineSpacing = 360 then
        Result := TdxParagraphLineSpacing.Sesquialteral
      else
        if FDocLineSpacing = 480 then
          Result := TdxParagraphLineSpacing.Double
        else
          Result := TdxParagraphLineSpacing.Multiple;
end;

function TdxDocCommandLineSpacing.CalcLineSpacing(AUnitConverter: TdxDocumentModelUnitConverter): Single;
begin
  if FDocLineSpacingType = 0 then
    Result := AUnitConverter.TwipsToModelUnits(Abs(FDocLineSpacing))
  else
    if FDocLineSpacing = 240 then
      Result := 1.0
    else
      if FDocLineSpacing = 360 then
        Result := 1.5
      else
        if FDocLineSpacing = 480 then
          Result := 2.0
        else
          Result := Abs(FDocLineSpacing / 240.0);
end;

{ TdxDocCommandInnerTableCell }

function TdxDocCommandInnerTableCell.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandInnerTableCell.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.InnerTableCell := Value;
end;

{ TdxDocCommandTablePosition }

constructor TdxDocCommandTablePosition.Create(AInfo: TdxTableFloatingPositionInfo);
begin
  HorizontalAnchor := AInfo.HorizontalAnchor;
  VerticalAnchor := AInfo.VerticalAnchor;
end;

constructor TdxDocCommandTablePosition.Create;
begin
end;

{ TdxDocCommandInnerTableTrailer }

function TdxDocCommandInnerTableTrailer.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandInnerTableTrailer.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.NestedTableTrailer := Value;
end;

{ TdxDocCommandTableTrailer }

function TdxDocCommandTableTrailer.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandTableTrailer.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.TableTrailer := Value;
end;

{ TdxDocCommandChangeTableStyle }

function TdxDocCommandChangeTableStyle.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandChangeTableStyle.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableInfo.TableStyleIndex := Value;
end;

{ TdxDocCommandInsertTableCell }

constructor TdxDocCommandInsertTableCell.Create;
begin
  FInsert := TdxInsertOperand.Create;
end;

destructor TdxDocCommandInsertTableCell.Destroy;
begin
  Insert := nil;
  inherited Destroy;
end;

function TdxDocCommandInsertTableCell.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandInsertTableCell.Read(const ASprm: TBytes);
begin
  Insert := TdxInsertOperand.FromByteArray(ASprm);
end;

procedure TdxDocCommandInsertTableCell.SetInsert(const Value: TdxInsertOperand);
begin
  FInsert.Free;
  FInsert := Value;
end;

procedure TdxDocCommandInsertTableCell.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableRowInfo.InsertActions.Add(Insert);
  FInsert := nil;
end;

procedure TdxDocCommandInsertTableCell.Write(AWriter: TBinaryWriter);
var
  ASprm: TBytes;
begin
  ASprm := TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), Insert.GetBytes);
  AWriter.Write(ASprm);
end;

{ TdxDocCommandPreferredTableCellWidth }

constructor TdxDocCommandPreferredTableCellWidth.Create;
begin
  FTableCellWidth := TdxTableCellWidthOperand.Create;
end;

destructor TdxDocCommandPreferredTableCellWidth.Destroy;
begin
  FTableCellWidth.Free;
  inherited Destroy;
end;

function TdxDocCommandPreferredTableCellWidth.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandPreferredTableCellWidth.Read(const ASprm: TBytes);
begin
  FTableCellWidth.Free;
  FTableCellWidth := TdxTableCellWidthOperand.FromByteArray(ASprm);
end;

procedure TdxDocCommandPreferredTableCellWidth.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableRowInfo.PreferredCellWidths.Add(TableCellWidth);
  FTableCellWidth := nil;
end;

procedure TdxDocCommandPreferredTableCellWidth.Write(AWriter: TBinaryWriter);
var
  ASprm: TBytes;
begin
  ASprm := TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), TableCellWidth.GetBytes);
  AWriter.Write(ASprm);
end;

{ TdxDocCommandOverrideCellBorders }

constructor TdxDocCommandOverrideCellBorders.Create;
begin
  FBordersOverrideOperand := TdxTableBordersOverrideOperand.Create;
end;

destructor TdxDocCommandOverrideCellBorders.Destroy;
begin
  FBordersOverrideOperand.Free;
  inherited Destroy;
end;

function TdxDocCommandOverrideCellBorders.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandOverrideCellBorders.Read(const ASprm: TBytes);
begin
  FBordersOverrideOperand.Free;
  FBordersOverrideOperand := TdxTableBordersOverrideOperand.FromByteArray(ASprm);
end;

procedure TdxDocCommandOverrideCellBorders.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableRowInfo.OverrideCellBordersActions.Add(OverriddenBorders);
  FBordersOverrideOperand := nil;
end;

procedure TdxDocCommandOverrideCellBorders.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandFactory.GetOpcodeByType(ClassType));
  AWriter.Write(Byte(TdxTableBordersOverrideOperand.Size));
  OverriddenBorders.Write(AWriter);
end;

{ TdxDocCommandCellSpacingPropertyValueBase }

constructor TdxDocCommandCellSpacingPropertyValueBase.Create;
begin
  FCellSpacing := TdxCellSpacingOperand.Create;
end;

procedure TdxDocCommandCellSpacingPropertyValueBase.Read(const ASprm: TBytes);
begin
  FCellSpacing.Free;
  FCellSpacing := TdxCellSpacingOperand.FromByteArray(ASprm);
end;

destructor TdxDocCommandCellSpacingPropertyValueBase.Destroy;
begin
  FCellSpacing.Free;
  inherited Destroy;
end;

procedure TdxDocCommandCellSpacingPropertyValueBase.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  ApplyCellSpacingOperand(APropertyContainer);
end;

procedure TdxDocCommandCellSpacingPropertyValueBase.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandFactory.GetOpcodeByType(ClassType));
  AWriter.Write(Byte(TdxCellSpacingOperand.Size));
  FCellSpacing.Write(AWriter);
end;

{ TdxDocCommandCellMargin }

function TdxDocCommandCellMargin.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandCellMargin.ApplyCellSpacingOperand(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.TableRowInfo.CellMarginsActions.Add(CellSpacing);
  FCellSpacing := nil;
end;

{ TdxDocCommandShadingListBase }

constructor TdxDocCommandShadingListBase.Create;
begin
  FCellColors := TdxAlphaColorList.Create;
end;

destructor TdxDocCommandShadingListBase.Destroy;
begin
  FCellColors.Free;
  inherited Destroy;
end;

procedure TdxDocCommandShadingListBase.Read(const ASprm: TBytes);
var
  ACount, I: Integer;
  AShadingDescriptor: TdxDocShadingDescriptor;
begin
  ACount := Length(ASprm) div TdxDocShadingDescriptor.Size;
  for I := 0 to ACount - 1 do
  begin
    AShadingDescriptor := TdxDocShadingDescriptor.FromByteArray(ASprm, I * TdxDocShadingDescriptor.Size);
    try
      CellColors.Add(AShadingDescriptor.BackgroundColor);
    finally
      AShadingDescriptor.Free;
    end;
  end;
end;

procedure TdxDocCommandShadingListBase.Write(AWriter: TBinaryWriter);
var
  ACount, I: Integer;
  ADescriptor: TdxDocShadingDescriptor;
begin
  AWriter.Write(TdxDocCommandFactory.GetOpcodeByType(ClassType));
  ACount := FCellColors.Count;
  AWriter.Write(Byte(ACount * TdxDocShadingDescriptor.Size));
  for I := 0 to ACount - 1 do
  begin
    ADescriptor := TdxDocShadingDescriptor.Create;
    try
      ADescriptor.BackgroundColor := FCellColors[I];
      ADescriptor.Write(AWriter);
    finally
      ADescriptor.Free;
    end;
  end;
end;

{ TdxDocCommandDefineTableShadingsBase }

function TdxDocCommandDefineTableShadingsBase.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableCell;
end;

procedure TdxDocCommandDefineTableShadingsBase.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  I: Integer;
begin
  for I := 0 to CellColors.Count - 1 do
    APropertyContainer.TableCellInfo.CellColors.Insert(StartIndex + I, CellColors[I]);
end;

{ TdxDocCommandDefineTableShadings }

function TdxDocCommandDefineTableShadings.GetStartIndex: Integer;
begin
  Result := 0;
end;

{ TdxDocCommandDefineTableShadings2nd }

function TdxDocCommandDefineTableShadings2nd.GetStartIndex: Integer;
begin
  Result := 22;
end;

{ TdxDocCommandDefineTableShadings3rd }

function TdxDocCommandDefineTableShadings3rd.GetStartIndex: Integer;
begin
  Result := 44;
end;

{ TdxDocCommandCellRangeVerticalAlignment }

constructor TdxDocCommandCellRangeVerticalAlignment.Create;
begin
  FCellRangeVerticalAlignment := TdxCellRangeVerticalAlignmentOperand.Create;
end;

destructor TdxDocCommandCellRangeVerticalAlignment.Destroy;
begin
  FCellRangeVerticalAlignment.Free;
  inherited Destroy;
end;

function TdxDocCommandCellRangeVerticalAlignment.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandCellRangeVerticalAlignment.Read(const ASprm: TBytes);
begin
  FCellRangeVerticalAlignment.Free;
  FCellRangeVerticalAlignment := TdxCellRangeVerticalAlignmentOperand.FromByteArray(ASprm);
end;

procedure TdxDocCommandCellRangeVerticalAlignment.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableRowInfo.CellRangeVerticalAlignmentActions.Add(CellRangeVerticalAlignment);
  FCellRangeVerticalAlignment := nil;
end;

procedure TdxDocCommandCellRangeVerticalAlignment.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandFactory.GetOpcodeByType(ClassType));
  AWriter.Write(TdxCellRangeVerticalAlignmentOperand.Size);
  CellRangeVerticalAlignment.Write(AWriter);
end;

{ TdxDocCommandVerticalMergeTableCells }

function TdxDocCommandVerticalMergeTableCells.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandVerticalMergeTableCells.Read(const ASprm: TBytes);
begin
  CellIndex := ASprm[0];
  VerticalMerging := TdxMergingStateCalculator.CalcVerticalMergingState(ASprm[1]);
end;

procedure TdxDocCommandVerticalMergeTableCells.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableRowInfo.VerticalMerging.Add(TdxVerticalMergeInfo.Create(CellIndex, VerticalMerging));
end;

procedure TdxDocCommandVerticalMergeTableCells.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType),
    TBytes.Create(CellIndex, TdxMergingStateCalculator.CalcVerticalMergingTypeCode(VerticalMerging))));
end;

{ TdxDocCommandHideCellMark }

constructor TdxDocCommandHideCellMark.Create;
begin
  FCellHideMark := TdxCellHideMarkOperand.Create;
end;

destructor TdxDocCommandHideCellMark.Destroy;
begin
  FCellHideMark.Free;
  inherited Destroy;
end;

function TdxDocCommandHideCellMark.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandHideCellMark.Read(const ASprm: TBytes);
begin
  CellHideMark := TdxCellHideMarkOperand.FromByteArray(ASprm);
end;

procedure TdxDocCommandHideCellMark.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableRowInfo.CellHideMarkActions.Add(CellHideMark);
  FCellHideMark := nil;
end;

procedure TdxDocCommandHideCellMark.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandFactory.GetOpcodeByType(ClassType));
  AWriter.Write(TdxCellHideMarkOperand.Size);
  CellHideMark.Write(AWriter);
end;

procedure TdxDocCommandHideCellMark.SetCellHideMark(const Value: TdxCellHideMarkOperand);
begin
  if CellHideMark = Value then
    Exit;
  FCellHideMark.Free;
  FCellHideMark := Value;
end;

{ TdxDocCommandTableRowHeight }

function TdxDocCommandTableRowHeight.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandTableRowHeight.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableRowInfo.TableRowProperties.Height.&Type := CalcHeightUnitType;
  APropertyContainer.TableRowInfo.TableRowProperties.Height.Value := APropertyContainer.UnitConverter.TwipsToModelUnits(Abs(Value));
end;

procedure TdxDocCommandTableRowHeight.Write(AWriter: TBinaryWriter);
var
  ASprm: TBytes;
begin
  if &Type = TdxHeightUnitType.Exact then
    Value := Value * -1;
  ASprm := TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), TdxByteArrayHelper.From<SmallInt>(SmallInt(Value)));
  AWriter.Write(ASprm);
end;

function TdxDocCommandTableRowHeight.CalcHeightUnitType: TdxHeightUnitType;
begin
  if Value > 0 then
    Result := TdxHeightUnitType.Minimum
  else
    if Value < 0 then
      Result := TdxHeightUnitType.Exact
    else
      Result := TdxHeightUnitType.Auto;
end;

{ TdxDocCommandWidthUnitPropertyValueBase }

constructor TdxDocCommandWidthUnitPropertyValueBase.Create;
begin
  FWidthUnit := TdxWidthUnitOperand.Create;
end;

destructor TdxDocCommandWidthUnitPropertyValueBase.Destroy;
begin
  FWidthUnit.Free;
  inherited Destroy;
end;

procedure TdxDocCommandWidthUnitPropertyValueBase.Read(const ASprm: TBytes);
begin
  FWidthUnit.Free;
  FWidthUnit := TdxWidthUnitOperand.FromByteArray(ASprm, 0);
end;

procedure TdxDocCommandWidthUnitPropertyValueBase.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  if WidthUnit.&Type = TdxWidthUnitType.ModelUnits then
    WidthUnit.Value := SmallInt(APropertyContainer.UnitConverter.TwipsToModelUnits(WidthUnit.Value));
  ApplyWidthOperand(APropertyContainer);
end;

procedure TdxDocCommandWidthUnitPropertyValueBase.Write(AWriter: TBinaryWriter);
var
  ASprm: TBytes;
begin
  ASprm := TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), WidthUnit.GetBytes);
  AWriter.Write(ASprm);
end;

{ TdxDocCommandWidthBefore }

function TdxDocCommandWidthBefore.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandWidthBefore.ApplyWidthOperand(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.TableRowInfo.TableRowProperties.WidthBefore.&Type := WidthUnit.&Type;
  APropertyContainer.TableRowInfo.TableRowProperties.WidthBefore.Value := WidthUnit.Value;
  APropertyContainer.TableRowInfo.WidthBeforeSetted := True;
end;

{ TdxDocCommandWidthAfter }

function TdxDocCommandWidthAfter.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandWidthAfter.ApplyWidthOperand(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.TableRowInfo.TableRowProperties.WidthAfter.&Type := WidthUnit.&Type;
  APropertyContainer.TableRowInfo.TableRowProperties.WidthAfter.Value := WidthUnit.Value;
end;

{ TdxDocCommandTableDxaLeft }

function TdxDocCommandTableDxaLeft.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandTableDxaLeft.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableRowInfo.RowLeft := APropertyContainer.UnitConverter.TwipsToModelUnits(Value);
  APropertyContainer.TableRowInfo.RowLeftSetted := True;
end;

{ TdxDocCommandTableDxaGapHalf }

function TdxDocCommandTableDxaGapHalf.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandTableDxaGapHalf.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  if Value < 0 then
    Exit;
  APropertyContainer.TableRowInfo.RowLeftOffset := APropertyContainer.UnitConverter.TwipsToModelUnits(Value);
  APropertyContainer.TableRowInfo.RowLeftOffsetSetted := True;
end;

{ TdxDocCommandPreferredTableWidth }

function TdxDocCommandPreferredTableWidth.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandPreferredTableWidth.ApplyWidthOperand(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.TableInfo.TableProperties.PreferredWidth.&Type := WidthUnit.&Type;
  APropertyContainer.TableInfo.TableProperties.PreferredWidth.Value := WidthUnit.Value;
end;

{ TdxDocCommandTableAutoFit }

function TdxDocCommandTableAutoFit.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandTableAutoFit.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  if Value then
    APropertyContainer.TableInfo.TableProperties.TableLayout := TdxTableLayoutType.Autofit
  else
    APropertyContainer.TableInfo.TableProperties.TableLayout := TdxTableLayoutType.Fixed;
end;

{ TdxDocCommandTableBorders }

constructor TdxDocCommandTableBorders.Create;
begin
  FTableBorders := TdxTableBordersOperand.Create;
end;

destructor TdxDocCommandTableBorders.Destroy;
begin
  FTableBorders.Free;
  inherited Destroy;
end;

function TdxDocCommandTableBorders.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandTableBorders.Read(const ASprm: TBytes);
begin
  FTableBorders.Free;
  FTableBorders := TdxTableBordersOperand.FromByteArray(ASprm);
end;

procedure TdxDocCommandTableBorders.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  TableBorders.ApplyProperties(APropertyContainer.TableInfo.TableProperties.Borders, APropertyContainer.UnitConverter);
end;

procedure TdxDocCommandTableBorders.Write(AWriter: TBinaryWriter);
begin
  if FTableBorders <> nil then
  begin
    AWriter.Write(TdxDocCommandFactory.GetOpcodeByType(ClassType));
    AWriter.Write(TableBordersOperandSize);
    FTableBorders.Write(AWriter);
  end;
end;

{ TdxDocCommandCellMarginDefault }

function TdxDocCommandCellMarginDefault.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandCellMarginDefault.ApplyCellSpacingOperand(APropertyContainer: TdxDocPropertyContainer);
begin
  CellSpacing.ApplyProperties(APropertyContainer.TableInfo.TableProperties.CellMargins, APropertyContainer.UnitConverter);
end;

{ TdxDocCommandWidthIndent }

function TdxDocCommandWidthIndent.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandWidthIndent.ApplyWidthOperand(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.TableInfo.TableProperties.TableIndent.&Type := WidthUnit.&Type;
  APropertyContainer.TableInfo.TableProperties.TableIndent.Value := WidthUnit.Value;
end;

{ TdxDocCommandTableAlignment }

function TdxDocCommandTableAlignment.GetTableAlignment: TdxTableRowAlignment;
begin
  Result := CalcAlignmentType;
end;

procedure TdxDocCommandTableAlignment.SetTableAlignment(const AValue: TdxTableRowAlignment);
begin
  Value := CalcAlignmentTypeCode(AValue);
end;

function TdxDocCommandTableAlignment.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandTableAlignment.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  CalcAlignmentType;
  APropertyContainer.TableInfo.TableProperties.TableAlignment := TableAlignment;
end;

function TdxDocCommandTableAlignment.CalcAlignmentType: TdxTableRowAlignment;
begin
  case Value of
    0:
      Result := TdxTableRowAlignment.Left;
    1:
      Result := TdxTableRowAlignment.Center;
    2:
      Result := TdxTableRowAlignment.Right;
    else
      Result := TdxTableRowAlignment.Left;
  end;
end;

function TdxDocCommandTableAlignment.CalcAlignmentTypeCode(AAlignment: TdxTableRowAlignment): Integer;
begin
  case AAlignment of
    TdxTableRowAlignment.Left:
      Result := 0;
    TdxTableRowAlignment.Center:
      Result := 1;
    TdxTableRowAlignment.Right:
      Result := 2;
    else
      Result := 0;
  end;
end;

{ TdxDocCommandCellSpacing }

function TdxDocCommandCellSpacing.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandCellSpacing.ApplyCellSpacingOperand(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.TableInfo.TableProperties.CellSpacing.&Type := CellSpacing.WidthUnit.&Type;
  APropertyContainer.TableInfo.TableProperties.CellSpacing.Value := CellSpacing.WidthUnit.Value;
end;

{ TdxDocCommandTableBackgroundColor }

function TdxDocCommandTableBackgroundColor.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandTableBackgroundColor.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableInfo.TableProperties.BackgroundColor := ShadingDescriptor.BackgroundColor;
end;

{ TdxDocCommandTableOverlap }

function TdxDocCommandTableOverlap.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandTableOverlap.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableInfo.TableProperties.IsTableOverlap := not Value;
end;

{ TdxDocCommandBottomFromText }

function TdxDocCommandBottomFromText.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandBottomFromText.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableInfo.TableProperties.FloatingPosition.BottomFromText := APropertyContainer.UnitConverter.TwipsToModelUnits(Value);
end;

{ TdxDocCommandLeftFromText }

function TdxDocCommandLeftFromText.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandLeftFromText.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableInfo.TableProperties.FloatingPosition.LeftFromText := APropertyContainer.UnitConverter.TwipsToModelUnits(Value);
end;

{ TdxDocCommandRightFromText }

function TdxDocCommandRightFromText.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandRightFromText.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableInfo.TableProperties.FloatingPosition.RightFromText := APropertyContainer.UnitConverter.TwipsToModelUnits(Value);
end;

{ TdxDocCommandTopFromText }

function TdxDocCommandTopFromText.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandTopFromText.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableInfo.TableProperties.FloatingPosition.TopFromText := APropertyContainer.UnitConverter.TwipsToModelUnits(Value);
end;

{ TdxDocCommandTableHorizontalPosition }

function TdxDocCommandTableHorizontalPosition.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandTableHorizontalPosition.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  AFloatingPosition: TdxTableFloatingPosition;
begin
  AFloatingPosition := APropertyContainer.TableInfo.TableProperties.FloatingPosition;
  AFloatingPosition.TextWrapping := TdxTextWrapping.Around;
  case Value of
    LeftAligned:
      AFloatingPosition.HorizontalAlign := TdxHorizontalAlignMode.Left;
    Centered:
      AFloatingPosition.HorizontalAlign := TdxHorizontalAlignMode.Center;
    RightAligned:
      AFloatingPosition.HorizontalAlign := TdxHorizontalAlignMode.Right;
    Inside:
      AFloatingPosition.HorizontalAlign := TdxHorizontalAlignMode.Inside;
    Outside:
      AFloatingPosition.HorizontalAlign := TdxHorizontalAlignMode.Outside;
    else
      AFloatingPosition.TableHorizontalPosition := APropertyContainer.UnitConverter.TwipsToModelUnits(Value);
  end;
end;

{ TdxDocCommandTableVerticalPosition }

function TdxDocCommandTableVerticalPosition.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandTableVerticalPosition.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  AFloatingPosition: TdxTableFloatingPosition;
begin
  AFloatingPosition := APropertyContainer.TableInfo.TableProperties.FloatingPosition;
  AFloatingPosition.TextWrapping := TdxTextWrapping.Around;
  case Value of
    &Inline:
      AFloatingPosition.VerticalAlign := TdxVerticalAlignMode.Inline;
    Top:
      AFloatingPosition.VerticalAlign := TdxVerticalAlignMode.Top;
    Center:
      AFloatingPosition.VerticalAlign := TdxVerticalAlignMode.Center;
    Bottom:
      AFloatingPosition.VerticalAlign := TdxVerticalAlignMode.Bottom;
    Inside:
      AFloatingPosition.VerticalAlign := TdxVerticalAlignMode.Inside;
    Outside:
      AFloatingPosition.VerticalAlign := TdxVerticalAlignMode.Outside;
    else
      AFloatingPosition.TableVerticalPosition := APropertyContainer.UnitConverter.TwipsToModelUnits(Value);
  end;
end;

{ TdxDocCommandTableStyleRowBandSize }

function TdxDocCommandTableStyleRowBandSize.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandTableStyleRowBandSize.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableInfo.TableProperties.TableStyleRowBandSize := Value;
end;

{ TdxDocCommandTableStyleColBandSize }

function TdxDocCommandTableStyleColBandSize.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandTableStyleColBandSize.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableInfo.TableProperties.TableStyleColBandSize := Value;
end;

{ TdxDocCommandColumnCount }

function TdxDocCommandColumnCount.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandColumnCount.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  I: Integer;
begin
  APropertyContainer.SectionInfo.SectionColumns.ColumnCount := Value + 1;
  for I := 0 to APropertyContainer.SectionInfo.SectionColumns.ColumnCount - 1 do
    APropertyContainer.SectionInfo.SectionColumns.Columns.Add(TdxColumnInfo.Create);
end;

procedure TdxDocCommandColumnCount.Write(AWriter: TBinaryWriter);
begin
  Value := Value - 1;
  inherited Write(AWriter);
end;

{ TdxDocCommandColumnWidth }

function TdxDocCommandColumnWidth.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandColumnWidth.Read(const ASprm: TBytes);
begin
  ColumnIndex := ASprm[0];
  ColumnWidth := PWord(@ASprm[1])^;
end;

procedure TdxDocCommandColumnWidth.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  AColumns: TdxList<TdxColumnInfo>;
begin
  AColumns := APropertyContainer.SectionInfo.SectionColumns.Columns;
  if ColumnIndex >= AColumns.Count then
    Exit;
  AColumns[ColumnIndex].Width := ColumnWidth;
end;

procedure TdxDocCommandColumnWidth.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandFactory.GetOpcodeByType(ClassType));
  AWriter.Write(ColumnIndex);
  ColumnWidth := Max(720, ColumnWidth);
  AWriter.Write(ColumnWidth);
end;

{ TdxDocCommandNotEvenlyColumnsSpace }

function TdxDocCommandNotEvenlyColumnsSpace.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandNotEvenlyColumnsSpace.Read(const ASprm: TBytes);
begin
  ColumnIndex := ASprm[0];
  ColumnSpace := PSmallInt(@ASprm[1])^;
end;

procedure TdxDocCommandNotEvenlyColumnsSpace.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  AColumns: TdxList<TdxColumnInfo>;
begin
  AColumns := APropertyContainer.SectionInfo.SectionColumns.Columns;
  if ColumnIndex >= AColumns.Count then
    Exit;
  AColumns[ColumnIndex].Space := ColumnSpace;
end;

procedure TdxDocCommandNotEvenlyColumnsSpace.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandFactory.GetOpcodeByType(ClassType));
  AWriter.Write(ColumnIndex);
  ColumnSpace := Min(ColumnSpace, SmallInt(TdxDocConstants.MaxXASValue));
  ColumnSpace := Max(SmallInt(0), ColumnSpace);
  AWriter.Write(ColumnSpace);
end;

{ TdxDocCommandDrawVerticalSeparator }

function TdxDocCommandDrawVerticalSeparator.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandDrawVerticalSeparator.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionColumns.DrawVerticalSeparator := Value;
end;

{ TdxDocCommandEqualWidthColumns }

function TdxDocCommandEqualWidthColumns.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandEqualWidthColumns.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  AInfo: TdxColumnsInfo;
begin
  AInfo := APropertyContainer.SectionInfo.SectionColumns;
  if AInfo.ColumnCount = 1 then
    Exit;
  APropertyContainer.SectionInfo.SectionColumns.EqualWidthColumns := Value;
end;

{ TdxDocCommandColumnSpace }

function TdxDocCommandColumnSpace.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandColumnSpace.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionColumns.Space := Value;
end;

{ TdxDocCommandFootNoteNumberingFormatBase }

function TdxDocCommandFootNoteNumberingFormatBase.GetNumberingFormat: TdxNumberingFormat;
begin
  Result := TdxNumberingFormatCalculator.CalcNumberingFormat(Value);
end;

procedure TdxDocCommandFootNoteNumberingFormatBase.SetNumberingFormat(const AValue: TdxNumberingFormat);
begin
  Value := TdxNumberingFormatCalculator.CalcNumberingFormatCode(AValue);
end;

{ TdxDocCommandFootNoteNumberingFormat }

function TdxDocCommandFootNoteNumberingFormat.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandFootNoteNumberingFormat.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.FootNote.NumberingFormat := NumberingFormat;
end;

{ TdxDocCommandEndNoteNumberingFormat }

function TdxDocCommandEndNoteNumberingFormat.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandEndNoteNumberingFormat.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.EndNote.NumberingFormat := NumberingFormat;
end;

{ TdxDocCommandFootNoteNumberingRestartTypeBase }

function TdxDocCommandFootNoteNumberingRestartTypeBase.GetNumberingRestartType: TdxLineNumberingRestart;
begin
  Result := TdxFootNoteNumberingRestartCalculator.CalcFootNoteNumberingRestart(Value);
end;

procedure TdxDocCommandFootNoteNumberingRestartTypeBase.SetNumberingRestartType(const AValue: TdxLineNumberingRestart);
begin
  Value := TdxFootNoteNumberingRestartCalculator.CalcFootNoteNumberingRestartTypeCode(AValue);
end;

{ TdxDocCommandFootNoteNumberingRestartType }

function TdxDocCommandFootNoteNumberingRestartType.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandFootNoteNumberingRestartType.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.FootNote.NumberingRestartType := NumberingRestartType;
end;

{ TdxDocCommandEndNoteNumberingRestartType }

function TdxDocCommandEndNoteNumberingRestartType.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandEndNoteNumberingRestartType.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.EndNote.NumberingRestartType := NumberingRestartType;
end;

{ TdxDocCommandFootNoteStartingNumber }

function TdxDocCommandFootNoteStartingNumber.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandFootNoteStartingNumber.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.FootNote.StartingNumber := Value;
end;

{ TdxDocCommandEndNoteStartingNumber }

function TdxDocCommandEndNoteStartingNumber.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandEndNoteStartingNumber.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.EndNote.StartingNumber := Value;
end;

{ TdxDocCommandLeftMargin }

function TdxDocCommandLeftMargin.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandLeftMargin.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionMargins.Left := APropertyContainer.UnitConverter.TwipsToModelUnits(Value);
  APropertyContainer.SectionInfo.SectionPage.PaperKind := TdxPaperKind.Custom;
end;

{ TdxDocCommandRightMargin }

function TdxDocCommandRightMargin.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandRightMargin.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionMargins.Right := APropertyContainer.UnitConverter.TwipsToModelUnits(Value);
  APropertyContainer.SectionInfo.SectionPage.PaperKind := TdxPaperKind.Custom;
end;

{ TdxDocCommandTopMargin }

function TdxDocCommandTopMargin.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandTopMargin.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionMargins.Top := APropertyContainer.UnitConverter.TwipsToModelUnits(Value);
  APropertyContainer.SectionInfo.SectionPage.PaperKind := TdxPaperKind.Custom;
end;

{ TdxDocCommandBottomMargin }

function TdxDocCommandBottomMargin.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandBottomMargin.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionMargins.Bottom := APropertyContainer.UnitConverter.TwipsToModelUnits(Value);
  APropertyContainer.SectionInfo.SectionPage.PaperKind := TdxPaperKind.Custom;
end;

{ TdxDocCommandGutter }

function TdxDocCommandGutter.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandGutter.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionMargins.Gutter := APropertyContainer.UnitConverter.TwipsToModelUnits(Value);
end;

{ TdxDocCommandRTLGutter }

function TdxDocCommandRTLGutter.GetGutterAlignment: TdxSectionGutterAlignment;
begin
  if (FRtlGutter = 0) then
    Result := TdxSectionGutterAlignment.Left
  else
    Result := TdxSectionGutterAlignment.Right;
end;

procedure TdxDocCommandRTLGutter.SetGutterAlignment(const AValue: TdxSectionGutterAlignment);
begin
  CalcGutterAlignment(AValue);
end;

function TdxDocCommandRTLGutter.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandRTLGutter.Read(const ASprm: TBytes);
begin
  FRtlGutter := ASprm[0];
end;

procedure TdxDocCommandRTLGutter.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  if FRtlGutter = 1 then
    APropertyContainer.SectionInfo.SectionMargins.GutterAlignment := TdxSectionGutterAlignment.Right
  else
    APropertyContainer.SectionInfo.SectionMargins.GutterAlignment := TdxSectionGutterAlignment.Left;
end;

procedure TdxDocCommandRTLGutter.Write(AWriter: TBinaryWriter);
var
  ASprm: TBytes;
begin
  ASprm := TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), TBytes.Create(FRtlGutter));
  AWriter.Write(ASprm);
end;

procedure TdxDocCommandRTLGutter.CalcGutterAlignment(AAlignment: TdxSectionGutterAlignment);
begin
  if AAlignment = TdxSectionGutterAlignment.Left then
    FRtlGutter := 0;
  if AAlignment = TdxSectionGutterAlignment.Right then
    FRtlGutter := 1;
end;

{ TdxDocCommandHeaderOffset }

function TdxDocCommandHeaderOffset.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandHeaderOffset.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionMargins.HeaderOffset := APropertyContainer.UnitConverter.TwipsToModelUnits(Value);
end;

{ TdxDocCommandFooterOffset }

function TdxDocCommandFooterOffset.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandFooterOffset.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionMargins.FooterOffset := APropertyContainer.UnitConverter.TwipsToModelUnits(Value);
end;

{ TdxDocCommandPageHeight }

function TdxDocCommandPageHeight.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandPageHeight.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionPage.Height := APropertyContainer.UnitConverter.TwipsToModelUnits(Value);
end;

{ TdxDocCommandPageOrientation }

function TdxDocCommandPageOrientation.GetLandscape: Boolean;
begin
  Result := Value = $02;
end;

procedure TdxDocCommandPageOrientation.SetLandscape(const AValue: Boolean);
begin
  if (AValue) then
    Value := $02
  else
    Value := $01;
end;

function TdxDocCommandPageOrientation.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandPageOrientation.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionPage.Landscape := Landscape;
end;

{ TdxDocCommandPageWidth }

function TdxDocCommandPageWidth.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandPageWidth.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionPage.Width := APropertyContainer.UnitConverter.TwipsToModelUnits(Value);
end;

{ TdxDocCommandChapterHeaderStyle }

procedure TdxDocCommandChapterHeaderStyle.SetChapterHeaderStyle(const AValue: Integer);
begin
  Value := Byte(AValue);
end;

function TdxDocCommandChapterHeaderStyle.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

function TdxDocCommandChapterHeaderStyle.GetChapterHeaderStyle: Integer;
begin
  Result := Value;
end;

procedure TdxDocCommandChapterHeaderStyle.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionPageNumbering.ChapterHeaderStyle := Value;
end;

{ TdxDocCommandChapterSeparator }

class constructor TdxDocCommandChapterSeparator.Initialize;
begin
  FChapterSeparators := TDictionary<Byte, Char>.Create(5);
  FChapterSeparators.Add($00, TdxCharacters.Hyphen);
  FChapterSeparators.Add($01, TdxCharacters.Dot);
  FChapterSeparators.Add($02, TdxCharacters.Colon);
  FChapterSeparators.Add($03, TdxCharacters.EmDash);
  FChapterSeparators.Add($04, TdxCharacters.EnDash);
end;

class destructor TdxDocCommandChapterSeparator.Finalize;
begin
  FChapterSeparators.Free;
end;

function TdxDocCommandChapterSeparator.GetChapterSeparator: Char;
begin
  Result := FChapterSeparators[Value];
end;

procedure TdxDocCommandChapterSeparator.SetChapterSeparator(const AValue: Char);
begin
  Value := CalcChapterSeparatorCode(AValue);
end;

function TdxDocCommandChapterSeparator.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandChapterSeparator.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  AChapterSeparator: Char;
begin
  if FChapterSeparators.TryGetValue(Value, AChapterSeparator) then
    APropertyContainer.SectionInfo.SectionPageNumbering.ChapterSeparator := AChapterSeparator;
end;

function TdxDocCommandChapterSeparator.CalcChapterSeparatorCode(AChapterSeparator: Char): Byte;
begin
  case AChapterSeparator of
    TdxCharacters.Dot:
      Result := $01;
    TdxCharacters.Colon:
      Result := $02;
    TdxCharacters.EmDash:
      Result := $03;
    TdxCharacters.EnDash:
      Result := $04;
    else
      Result := $00;
  end;
end;

{ TdxDocCommandNumberingFormat }

function TdxDocCommandNumberingFormat.GetNumberingFormat: TdxNumberingFormat;
begin
  Result := TdxNumberingFormatCalculator.CalcNumberingFormat(Value);
end;

procedure TdxDocCommandNumberingFormat.SetNumberingFormat(const AValue: TdxNumberingFormat);
begin
  Value := TdxNumberingFormatCalculator.CalcNumberingFormatCode(AValue);
end;

function TdxDocCommandNumberingFormat.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandNumberingFormat.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionPageNumbering.NumberingFormat := NumberingFormat;
end;

{ TdxDocCommandUseStartingPageNumber }

function TdxDocCommandUseStartingPageNumber.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandUseStartingPageNumber.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionPageNumbering.ContinueNumbering := not Value;
end;

{ TdxDocCommandStartingPageNumber }

function TdxDocCommandStartingPageNumber.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandStartingPageNumber.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionPageNumbering.FirstPageNumber := Value;
end;

{ TdxDocCommandLineNumberingDistance }

function TdxDocCommandLineNumberingDistance.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandLineNumberingDistance.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionLineNumbering.Distance := Value;
end;

{ TdxDocCommandNumberingRestartType }

class constructor TdxDocCommandNumberingRestartType.Initialize;
begin
  FLineNumberingRestartTypes := TDictionary<TdxLineNumberingRestart, Byte>.Create(3);
  FLineNumberingRestartTypes.Add(TdxLineNumberingRestart.NewPage, $00);
  FLineNumberingRestartTypes.Add(TdxLineNumberingRestart.NewSection, $01);
  FLineNumberingRestartTypes.Add(TdxLineNumberingRestart.Continuous, $02);
end;

class destructor TdxDocCommandNumberingRestartType.Finalize;
begin
  FLineNumberingRestartTypes.Free;
end;

function TdxDocCommandNumberingRestartType.GetNumberingRestartType: TdxLineNumberingRestart;
begin
  Result := CalcLineNumberingRestart;
end;

procedure TdxDocCommandNumberingRestartType.SetNumberingRestartType(const AValue: TdxLineNumberingRestart);
begin
  Value := FLineNumberingRestartTypes[AValue];
end;

function TdxDocCommandNumberingRestartType.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandNumberingRestartType.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionLineNumbering.NumberingRestartType := NumberingRestartType;
end;

function TdxDocCommandNumberingRestartType.CalcLineNumberingRestart: TdxLineNumberingRestart;
begin
  if Value = $00 then
    Exit(TdxLineNumberingRestart.NewPage);
  if Value = $01 then
    Exit(TdxLineNumberingRestart.NewSection);
  Result := TdxLineNumberingRestart.Continuous;
end;

{ TdxDocCommandStartLineNumber }

function TdxDocCommandStartLineNumber.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandStartLineNumber.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionLineNumbering.StartingLineNumber := Value + 1;
end;

procedure TdxDocCommandStartLineNumber.Write(AWriter: TBinaryWriter);
begin
  Value := Value - 1;
  inherited Write(AWriter);
end;

{ TdxDocCommandStep }

function TdxDocCommandStep.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandStep.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionLineNumbering.Step := Value;
end;

{ TdxDocCommandDifferentFirstPage }

function TdxDocCommandDifferentFirstPage.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandDifferentFirstPage.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionGeneralSettings.DifferentFirstPage := Value;
end;

{ TdxDocCommandFirstPagePaperSource }

function TdxDocCommandFirstPagePaperSource.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandFirstPagePaperSource.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionGeneralSettings.FirstPagePaperSource := Value;
end;

{ TdxDocCommandOnlyAllowEditingOfFormFields }

function TdxDocCommandOnlyAllowEditingOfFormFields.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandOnlyAllowEditingOfFormFields.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionGeneralSettings.OnlyAllowEditingOfFormFields := Value;
end;

{ TdxDocCommandOtherPagePaperSource }

function TdxDocCommandOtherPagePaperSource.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandOtherPagePaperSource.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionGeneralSettings.OtherPagePaperSource := Value;
end;

{ TdxDocCommandStartType }

class constructor TdxDocCommandStartType.Initialize;
begin
  FStartTypes := TDictionary<TdxSectionStartType, Byte>.Create(5);
  FStartTypes.Add(TdxSectionStartType.Continuous, 0);
  FStartTypes.Add(TdxSectionStartType.Column, 1);
  FStartTypes.Add(TdxSectionStartType.NextPage, 2);
  FStartTypes.Add(TdxSectionStartType.EvenPage, 3);
  FStartTypes.Add(TdxSectionStartType.OddPage, 4);
end;

class destructor TdxDocCommandStartType.Finalize;
begin
  FStartTypes.Free;
end;

function TdxDocCommandStartType.GetStartType: TdxSectionStartType;
begin
  Result := CalcSectionStartType;
end;

procedure TdxDocCommandStartType.SetStartType(const AValue: TdxSectionStartType);
begin
  Value := FStartTypes[AValue];
end;

function TdxDocCommandStartType.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandStartType.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionGeneralSettings.StartType := StartType;
end;

function TdxDocCommandStartType.CalcSectionStartType: TdxSectionStartType;
begin
  case Value of
    0: Result := TdxSectionStartType.Continuous;
    1: Result := TdxSectionStartType.Column;
    2: Result := TdxSectionStartType.NextPage;
    3: Result := TdxSectionStartType.EvenPage;
  else
    Result := TdxSectionStartType.OddPage;
  end;
end;

{ TdxDocCommandTextDirection }

procedure TdxDocCommandTextDirection.SetTextDirection(const AValue: TdxTextDirection);
begin
  if FTextDirection = AValue then
    Exit;
  FTextDirection := AValue;
  CalcTextDirectionTypeCode;
end;

function TdxDocCommandTextDirection.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandTextDirection.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  if Value = 0 then
    APropertyContainer.SectionInfo.SectionGeneralSettings.TextDirection := TdxTextDirection.LeftToRightTopToBottom
  else
    APropertyContainer.SectionInfo.SectionGeneralSettings.TextDirection := TdxTextDirection.TopToBottomRightToLeft;
end;

procedure TdxDocCommandTextDirection.CalcTextDirectionTypeCode;
begin
  if (FTextDirection = TdxTextDirection.LeftToRightTopToBottom) or (FTextDirection = TdxTextDirection.LeftToRightTopToBottomRotated) then
    Value := 0
  else
    Value := 1;
end;

{ TdxDocCommandVerticalTextAlignment }

class constructor TdxDocCommandVerticalTextAlignment.Initialize;
begin
  FVerticalAlignmentTypes := TDictionary<TdxVerticalAlignment, Byte>.Create(4);
  FVerticalAlignmentTypes.Add(TdxVerticalAlignment.Top, 0);
  FVerticalAlignmentTypes.Add(TdxVerticalAlignment.Center, 1);
  FVerticalAlignmentTypes.Add(TdxVerticalAlignment.Both, 2);
  FVerticalAlignmentTypes.Add(TdxVerticalAlignment.Bottom, 3);
end;

class destructor TdxDocCommandVerticalTextAlignment.Finalize;
begin
  FVerticalAlignmentTypes.Free;
end;

function TdxDocCommandVerticalTextAlignment.GetVerticalTextAlignment: TdxVerticalAlignment;
begin
  Result := CalcVerticalAlignment;
end;

procedure TdxDocCommandVerticalTextAlignment.SetVerticalTextAlignment(const AValue: TdxVerticalAlignment);
begin
  Value := FVerticalAlignmentTypes[AValue];
end;

function TdxDocCommandVerticalTextAlignment.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandVerticalTextAlignment.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionGeneralSettings.VerticalTextAlignment := VerticalTextAlignment;
end;

function TdxDocCommandVerticalTextAlignment.CalcVerticalAlignment: TdxVerticalAlignment;
begin
  case Value of
    0: Result := TdxVerticalAlignment.Top;
    1: Result := TdxVerticalAlignment.Center;
    2: Result := TdxVerticalAlignment.Both;
  else
    Result := TdxVerticalAlignment.Bottom;
  end;
end;

{ TdxDocCommandReadTableProperties }

function TdxDocCommandReadTableProperties.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.None;
end;

procedure TdxDocCommandReadTableProperties.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
var
  ASize: Integer;
  AGrpprl: TBytes;
begin
  ADataStreamReader.BaseStream.Seek(Value, TSeekOrigin.soBeginning);
  ASize := ADataStreamReader.ReadUInt16;
  AGrpprl := ADataStreamReader.ReadBytes(ASize);
  TdxDocCommandHelper.Traverse(AGrpprl, APropertyContainer, ADataStreamReader);
end;

{ TdxDocCommandEmpty }

function TdxDocCommandEmpty.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.None;
end;

procedure TdxDocCommandEmpty.Read(const ASprm: TBytes);
begin
end;

procedure TdxDocCommandEmpty.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
end;

procedure TdxDocCommandEmpty.Write(AWriter: TBinaryWriter);
begin
end;

{ TdxDocCommandFactory }

class constructor TdxDocCommandFactory.Initialize;
begin
  FCommandTypes := TDictionary<SmallInt, TdxDocCommandClass>.Create;
  FCommandOpcodes := TDictionary<TdxDocCommandClass, SmallInt>.Create;

  //Some commands have several opcodes corresponding to different Word versions. Always add opcodes for Word 97 first!
  Add($0000, TdxDocCommandEmpty);
  //Character formatting commands
  Add($083b, TdxDocCommandAllCaps);
  Add($ca71, TdxDocCommandBackColor); //shd
  Add($2a0c, TdxDocCommandBackColorIco); //h
  Add($0835, TdxDocCommandBold);
  Add($0836, TdxDocCommandItalic);
  Add($4a4f, TdxDocCommandFontName);
  Add($4a50, TdxDocCommandEastAsianFontName);
  Add($4a51, TdxDocCommandNonASCIIFontName);
  Add($4a43, TdxDocCommandFontSize);
  Add($ca49, TdxDocCommandFontSizeNew);
  Add($0837, TdxDocCommandStrike);
  Add($2a53, TdxDocCommandDoubleStrike);
  Add($2a3e, TdxDocCommandUnderline);
  Add($6870, TdxDocCommandForeColor); //color
  Add($2a42, TdxDocCommandForeColorIco);//color
  Add($083c, TdxDocCommandHidden);
  Add($2a48, TdxDocCommandScript);
  Add($6877, TdxDocCommandUnderlineColor);
  //Paragraph formatting commands
  Add($2403, TdxDocCommandAlignment);
  Add($2461, TdxDocCommandAlignmentNew);
  Add($8411, TdxDocCommandFirstLineIndent);
  Add($8460, TdxDocCommandFirstLineIndentNew);
  Add($845e, TdxDocCommandLogicalLeftIndent);
  Add($840f, TdxDocCommandPhysicalLeftIndent);
  Add($6412, TdxDocCommandLineSpacing);
  Add($845d, TdxDocCommandLogicalRightIndent);
  Add($840e, TdxDocCommandPhysicalRightIndent);
  Add($a414, TdxDocCommandSpacingAfter);
  Add($a413, TdxDocCommandSpacingBefore);
  Add($242a, TdxDocCommandSuppressHyphenation);
  Add($240c, TdxDocCommandSuppressLineNumbers);
  Add($246D, TdxDocCommandContextualSpacing);
  Add($2407, TdxDocCommandPageBreakBefore);
  Add($245B, TdxDocCommandBeforeAutoSpacing);
  Add($245C, TdxDocCommandAfterAutoSpacing);
  Add($2406, TdxDocCommandKeepWithNext);
  Add($2405, TdxDocCommandKeepLinesTogether);
  Add($2431, TdxDocCommandWidowOrphanControl);
  Add($2640, TdxDocCommandOutlineLevel);
  Add($C64D, TdxDocCommandParagraphShading);
  Add($442D, TdxDocCommandParagraphShading2); // patterned
  Add($6424, TdxDocCommandParagraphTopBorder);
  Add($6425, TdxDocCommandParagraphLeftBorder);
  Add($6426, TdxDocCommandParagraphBottomBorder);
  Add($6427, TdxDocCommandParagraphRightBorder);
  Add($C64E, TdxDocCommandParagraphTopBorderNew);
  Add($C64F, TdxDocCommandParagraphLeftBorderNew);
  Add($C650, TdxDocCommandParagraphBottomBorderNew);
  Add($C651, TdxDocCommandParagraphRightBorderNew);
  //Section margins commands
  Add($b021, TdxDocCommandLeftMargin);
  Add($b022, TdxDocCommandRightMargin);
  Add($9023, TdxDocCommandTopMargin);
  Add($9024, TdxDocCommandBottomMargin);
  Add($b025, TdxDocCommandGutter);
  Add($b017, TdxDocCommandHeaderOffset);
  Add($b018, TdxDocCommandFooterOffset);
  Add($322a, TdxDocCommandRTLGutter);
  //Section columns commands
  Add($500b, TdxDocCommandColumnCount);
  Add($f203, TdxDocCommandColumnWidth);
  Add($f204, TdxDocCommandNotEvenlyColumnsSpace);
  Add($3019, TdxDocCommandDrawVerticalSeparator);
  Add($3005, TdxDocCommandEqualWidthColumns);
  Add($900c, TdxDocCommandColumnSpace);
  //Section page commands
  Add($b020, TdxDocCommandPageHeight);
  Add($301d, TdxDocCommandPageOrientation);
  Add($b01f, TdxDocCommandPageWidth);
  //Section general settings commands
  Add($300a, TdxDocCommandDifferentFirstPage);
  Add($5007, TdxDocCommandFirstPagePaperSource);
  Add($3006, TdxDocCommandOnlyAllowEditingOfFormFields);
  Add($5008, TdxDocCommandOtherPagePaperSource);
  Add($3009, TdxDocCommandStartType);
  Add($3228, TdxDocCommandTextDirection);
  Add($301a, TdxDocCommandVerticalTextAlignment);
  //Section line numbering commands
  Add($9016, TdxDocCommandLineNumberingDistance);
  Add($3013, TdxDocCommandNumberingRestartType);
  Add($501b, TdxDocCommandStartLineNumber);
  Add($5015, TdxDocCommandStep);
  //Section page numbering commands
  Add($3001, TdxDocCommandChapterHeaderStyle);
  Add($3000, TdxDocCommandChapterSeparator);
  Add($300e, TdxDocCommandNumberingFormat);
  Add($3011, TdxDocCommandUseStartingPageNumber);
  Add($501c, TdxDocCommandStartingPageNumber);
  Add($7044, TdxDocCommandStartingPageNumberNew);
  //FootNotes and EndNotes commands
  Add($303b, TdxDocCommandFootNotePosition);
  Add($303c, TdxDocCommandFootNoteNumberingRestartType);
  Add($503f, TdxDocCommandFootNoteStartingNumber);
  Add($5040, TdxDocCommandFootNoteNumberingFormat);
  Add($303d, TdxDocCommandEndNotePosition);
  Add($303e, TdxDocCommandEndNoteNumberingRestartType);
  Add($5041, TdxDocCommandEndNoteStartingNumber);
  Add($5042, TdxDocCommandEndNoteNumberingFormat);
  //Additional formatting commands
  Add($0800, TdxDocCommandDeleted);
  Add($4600, TdxDocCommandChangeParagraphStyle);
  Add($4a30, TdxDocCommandChangeCharacterStyle);
  Add($563a, TdxDocCommandChangeTableStyle);
  Add($6a03, TdxDocCommandPictureOffset);
  Add($c60d, TdxDocCommandChangeParagraphTabs);
  Add($c615, TdxDocCommandChangeParagraphTabsClose);
  Add($646b, TdxDocCommandReadTableProperties);
  Add($6645, TdxDocCommandReadExtendedPropertyModifiers);
  Add($6646, TdxDocCommandReadExtendedPropertyModifiersNew);
  //List formatting commands
  Add($460b, TdxDocCommandListInfoIndex);
  Add($260a, TdxDocCommandListLevel);
  Add($6887, TdxDocCommandPictureBulletCharacterPosition);
  Add($4888, TdxDocCommandPictureBulletProperties);
  //Special symbol commands
  Add($6a09, TdxDocCommandSymbol);
  Add($0855, TdxDocCommandSpecial);
  Add($0856, TdxDocCommandEmbeddedObject);
  Add($0806, TdxDocCommandBinaryData);
  Add($080a, TdxDocCommandOle2Object);
  //Table formatting commands
  Add($2416, TdxDocCommandInTable);
  Add($2417, TdxDocCommandTableTrailer);
  Add($6649, TdxDocCommandTableDepth);
  Add($664a, TdxDocCommandTableDepthIncrement);
  Add($244b, TdxDocCommandInnerTableCell);
  Add($244c, TdxDocCommandInnerTableTrailer);
  Add($9407, TdxDocCommandTableRowHeight);
  Add($f614, TdxDocCommandPreferredTableWidth);
  Add($d635, TdxDocCommandPreferredTableCellWidth);
  Add($5624, TdxDocCommandMergeTableCells);
  Add($5625, TdxDocCommandSplitTableCells);
  Add($d62b, TdxDocCommandVerticalMergeTableCells);
  Add($7621, TdxDocCommandInsertTableCell);
  Add($7623, TdxDocCommandTableCellWidth);
  Add($3615, TdxDocCommandTableAutoFit);
  Add($9601, TdxDocCommandTableDxaLeft);
  Add($9602, TdxDocCommandTableDxaGapHalf);
  Add($d605, TdxDocCommandTableBorders80);
  Add($d613, TdxDocCommandTableBorders);
  Add($f617, TdxDocCommandWidthBefore);
  Add($f618, TdxDocCommandWidthAfter);
  Add($f661, TdxDocCommandWidthIndent);
  Add($548a, TdxDocCommandTableAlignment);
  Add($d633, TdxDocCommandCellSpacing);
  Add($d632, TdxDocCommandCellMargin);
  Add($d634, TdxDocCommandCellMarginDefault);
  Add($d660, TdxDocCommandTableBackgroundColor);
  Add($d670, TdxDocCommandDefineTableShadings);
  Add($d671, TdxDocCommandDefineTableShadings2nd);
  Add($d672, TdxDocCommandDefineTableShadings3rd);
  Add($3465, TdxDocCommandTableOverlap);
  Add($940e, TdxDocCommandTableHorizontalPosition);
  Add($940f, TdxDocCommandTableVerticalPosition);
  Add($941f, TdxDocCommandBottomFromText);
  Add($9410, TdxDocCommandLeftFromText);
  Add($941e, TdxDocCommandRightFromText);
  Add($9411, TdxDocCommandTopFromText);
  Add($261b, TdxDocCommandFramePosition);
  Add($360d, TdxDocCommandTablePosition);
  Add($3488, TdxDocCommandTableStyleRowBandSize);
  Add($3489, TdxDocCommandTableStyleColBandSize);
  Add($d62f, TdxDocCommandOverrideCellBorders);
  Add($d62c, TdxDocCommandCellRangeVerticalAlignment);
  Add($d608, TdxDocCommandTableDefinition);
  Add($d642, TdxDocCommandHideCellMark);
  Add($d61a, TdxDocCommandTopBorderColors);
  Add($d61b, TdxDocCommandLeftBorderColors);
  Add($d61c, TdxDocCommandBottomBorderColors);
  Add($d61d, TdxDocCommandRightBorderColors);
  Add($d612, TdxDocCommandTableShading);
  Add($d616, TdxDocCommandTableShading2);
  Add($d60C, TdxDocCommandTableShading3);
  Add($d609, TdxDocCommandDefTableShd80);
  Add($740a, TdxDocCommandTablePropertiesException);

  //Frame properties commands
  Add($2423, TdxDocCommandFrameWrapType);
  Add($442b, TdxDocCommandFrameHeight);
  Add($841a, TdxDocCommandFrameWidth);
  Add($8418, TdxDocCommandFrameHorizontalPosition);
  Add($8419, TdxDocCommandFrameVerticalPosition);

  //rsid
  Add($6467, TdxDocCommandParagraphFormattingRsid);
  Add($6815, TdxDocCommandCharacterFormattingRsid);
  Add($6816, TdxDocCommandInsertTextRsid);
end;

class destructor TdxDocCommandFactory.Finalize;
begin
  FCommandTypes.Free;
  FCommandOpcodes.Free;
end;

class function TdxDocCommandFactory.GetOpcodeByType(ACommandType: TClass): SmallInt;
begin
  if not FCommandOpcodes.TryGetValue(TdxDocCommandClass(ACommandType), Result) then
    Result := $0000;
end;

function TdxDocCommandFactory.CreateCommand(AOpcode: SmallInt): IdxDocCommand;
var
  ACommandClass: TdxDocCommandClass;
begin
  if FCommandTypes.TryGetValue(AOpcode, ACommandClass) then
    Result := ACommandClass.Create
  else
    Result := TdxDocCommandEmpty.Create;
end;

procedure TdxDocCommandFactory.UpdatePropertyContainer(AContainer: TdxDocPropertyContainer; AChangeType: TdxChangeActionTypes);
begin
  if (AChangeType = []) or (AChangeType = [TdxChangeActionType.None]) then
    Exit;
  if (AContainer.CharacterInfo = nil) and (TdxChangeActionType.Character in AChangeType) then
    AContainer.CharacterInfo := TdxCharacterInfo.CreateDefault(Model);
  if (AContainer.ParagraphInfo = nil) and (TdxChangeActionType.Paragraph in AChangeType) then
    AContainer.ParagraphInfo := TdxParagraphInfo.CreateDefault(Model);
  if (AContainer.FrameInfo = nil) and (AChangeType * [TdxChangeActionType.Frame, TdxChangeActionType.Table] <> []) then
    AContainer.FrameInfo := TdxFrameInfo.CreateDefault(Model);
  if (AContainer.SectionInfo = nil) and (TdxChangeActionType.Section in AChangeType) then
  begin
    AContainer.SectionInfo := TdxSectionInfo.CreateDefault(Model);
    if DocumentProperties <> nil then
      ApplyDocumentProperties(AContainer.SectionInfo);
  end;
  if (AContainer.TableCellInfo = nil) and (TdxChangeActionType.TableCell in AChangeType) then
    AContainer.TableCellInfo := TdxTableCellInfo.CreateDefault(Model);
  if (AContainer.TableRowInfo = nil) and (TdxChangeActionType.TableRow in AChangeType) then
    AContainer.TableRowInfo := TdxTableRowInfo.CreateDefault(Model);
  if (AContainer.TableInfo = nil) and (TdxChangeActionType.Table in AChangeType) then
    AContainer.TableInfo := TdxDocTableInfo.CreateDefault(Model);
end;

class procedure TdxDocCommandFactory.Add(ACode: Word; ACommandClass: TdxDocCommandClass);
begin
  FCommandTypes.Add(SmallInt(ACode), ACommandClass);
  FCommandOpcodes.Add(ACommandClass, SmallInt(ACode));
end;

procedure TdxDocCommandFactory.ApplyDocumentProperties(AInfo: TdxSectionInfo);
begin
  AInfo.FootNote.NumberingRestartType := DocumentProperties.FootNoteNumberingRestartType;
  AInfo.FootNote.StartingNumber := DocumentProperties.FootNoteInitialNumber;
  AInfo.FootNote.Position := DocumentProperties.FootNotePosition;
  AInfo.EndNote.NumberingRestartType := DocumentProperties.EndNoteNumberingRestartType;
  AInfo.EndNote.StartingNumber := DocumentProperties.EndnoteInitialNumber;
  AInfo.EndNote.Position := DocumentProperties.EndNotePosition;
end;

{ TdxDocCommandIcoPropertyValueBase }

procedure TdxDocCommandIcoPropertyValueBase.Read(const ASprm: TBytes);
var
  AColorIndex: Byte;
begin
  AColorIndex := ASprm[0];
  if AColorIndex < Length(TdxDocConstants.DefaultMSWordColor) then
    Color := TdxDocConstants.DefaultMSWordColor[AColorIndex]
  else
    Color := TdxAlphaColors.Empty;
end;

procedure TdxDocCommandIcoPropertyValueBase.Write(AWriter: TBinaryWriter);
var
  AColorIndex: Byte;
  ASprm: TBytes;
  AIndex: Integer;
begin
  if TArray.BinarySearch<TdxAlphaColor>(TdxDocConstants.DefaultMSWordColor, Color, AIndex) then
    AColorIndex := AIndex
  else
    AColorIndex := 0;
  ASprm := TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), TBytes.Create(AColorIndex));
  AWriter.Write(ASprm);
end;

{ TdxDocCommandBackColorIco }

function TdxDocCommandBackColorIco.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandBackColorIco.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.BackColor := Color;

  APropertyContainer.CharacterInfo.FormattingOptions.UseBackColor := True;
end;

{ TdxDocCommandForeColorIco }

function TdxDocCommandForeColorIco.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandForeColorIco.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.CharacterInfo.FormattingInfo.ForeColor := Color;

  APropertyContainer.CharacterInfo.FormattingOptions.UseForeColor := True;
end;

{ TdxDocCommandShading80PropertyValueBase }

constructor TdxDocCommandShading80PropertyValueBase.Create;
begin
  ShadingDescriptor := TdxDocShadingDescriptor80.Create;
end;

destructor TdxDocCommandShading80PropertyValueBase.Destroy;
begin
  ShadingDescriptor.Free;
  inherited Destroy;
end;

procedure TdxDocCommandShading80PropertyValueBase.Read(const ASprm: TBytes);
begin
  ShadingDescriptor.Free;
  ShadingDescriptor := TdxDocShadingDescriptor80.FromByteArray(ASprm);
end;

procedure TdxDocCommandShading80PropertyValueBase.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandFactory.GetOpcodeByType(ClassType));
  ShadingDescriptor.Write(AWriter);
end;

{ TdxDocCommandParagraphShading2 }

function TdxDocCommandParagraphShading2.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandParagraphShading2.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  if APropertyContainer.ParagraphInfo.FormattingOptions.UseBackColor then
    Exit;
  if ShadingDescriptor.BackgroundColor = TdxAlphaColors.Empty then
    if ShadingDescriptor.ShadingPattern = 0 then
      APropertyContainer.ParagraphInfo.FormattingInfo.BackColor := TdxAlphaColors.Empty
    else
      APropertyContainer.ParagraphInfo.FormattingInfo.BackColor := TdxAlphaColors.Black
  else
    APropertyContainer.ParagraphInfo.FormattingInfo.BackColor := ShadingDescriptor.BackgroundColor;
  APropertyContainer.ParagraphInfo.FormattingOptions.UseBackColor := True;
end;

{ TdxDocCommandStartingPageNumberNew }

function TdxDocCommandStartingPageNumberNew.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandStartingPageNumberNew.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.SectionPageNumbering.FirstPageNumber := Value;
end;

{ TdxDocCommandFootNotePositionBase }

function TdxDocCommandFootNotePositionBase.GetPosition: TdxFootNotePosition;
begin
  Result := TdxFootNotePositionCalculator.CalcFootNotePosition(Value);
end;

procedure TdxDocCommandFootNotePositionBase.SetPosition(const AValue: TdxFootNotePosition);
begin
  Value := TdxFootNotePositionCalculator.CalcFootNotePositionTypeCode(AValue);
end;

{ TdxDocCommandFootNotePosition }

function TdxDocCommandFootNotePosition.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandFootNotePosition.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.FootNote.Position := Position;
end;

{ TdxDocCommandEndNotePosition }

function TdxDocCommandEndNotePosition.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Section;
end;

procedure TdxDocCommandEndNotePosition.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.SectionInfo.EndNote.Position := Position;
end;

{ TdxDocCommandDeleted }

function TdxDocCommandDeleted.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.None;
end;

procedure TdxDocCommandDeleted.SetValueTrue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.IsDeleted := True;
end;

procedure TdxDocCommandDeleted.SetValueFalse(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.IsDeleted := False;
end;

procedure TdxDocCommandDeleted.LeaveValue(APropertyContainer: TdxDocPropertyContainer);
begin
end;

procedure TdxDocCommandDeleted.InverseValue(APropertyContainer: TdxDocPropertyContainer);
begin
  APropertyContainer.IsDeleted := not APropertyContainer.IsDeleted;
end;

procedure TdxDocCommandDeleted.SetUseValueTrue(APropertyContainer: TdxDocPropertyContainer);
begin
end;

procedure TdxDocCommandDeleted.SetUseValueFalse(APropertyContainer: TdxDocPropertyContainer);
begin
end;

{ TdxDocCommandChangeParagraphStyle }

function TdxDocCommandChangeParagraphStyle.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandChangeParagraphStyle.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.ParagraphStyleIndex := Value;
end;

{ TdxDocCommandChangeParagraphTabsClose }

function TdxDocCommandChangeParagraphTabsClose.CreateOperand: TdxTabsOperandBase;
begin
  Result := TdxTabsOperandClose.Create;
end;

function TdxDocCommandChangeParagraphTabsClose.CreateOperandFromByteArray(const ASprm: TBytes): TdxTabsOperandBase;
begin
  Result := TdxTabsOperandClose.FromByteArray(ASprm);
end;

{ TdxDocCommandSymbol }

function TdxDocCommandSymbol.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandSymbol.Read(const ASprm: TBytes);
begin
  FontFamilyNameIndex := PSmallInt(@ASprm[0])^;
  Symbol := ParseUnicodeChar(PSmallInt(@ASprm[2])^);
end;

procedure TdxDocCommandSymbol.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.CharacterInfo.Special := True;
  APropertyContainer.CharacterInfo.SpecialCharactersFontFamilyNameIndex := FontFamilyNameIndex;
  APropertyContainer.CharacterInfo.FormattingOptions.UseFontName := True;
  APropertyContainer.CharacterInfo.Symbol := Symbol;
end;

procedure TdxDocCommandSymbol.Write(AWriter: TBinaryWriter);
var
  AOperand, ASprm: TBytes;
begin
  SetLength(AOperand, 4);
  Move(FontFamilyNameIndex, AOperand[0], 2); //SmallInt
  Move(Symbol, AOperand[2], 2); //char
  ASprm := TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), AOperand);
  AWriter.Write(ASprm);
end;

class function TdxDocCommandSymbol.ParseUnicodeChar(AParameterValue: SmallInt): Char;
begin
  Result := Char(AParameterValue);
  if (Result >= #$f020) and (Result <= #$f0ff) then
    Result := Char(Ord(Result) - $f000);
end;

{ TdxDocCommandEmbeddedObject }

function TdxDocCommandEmbeddedObject.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandEmbeddedObject.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.CharacterInfo.EmbeddedObject := Value;
end;

{ TdxDocCommandBinaryData }

function TdxDocCommandBinaryData.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandBinaryData.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.CharacterInfo.BinaryData := Value;
end;

{ TdxDocCommandOle2Object }

function TdxDocCommandOle2Object.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Character;
end;

procedure TdxDocCommandOle2Object.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.CharacterInfo.Ole2Object := Value;
end;

{ TdxDocCommandTableDepthIncrement }

function TdxDocCommandTableDepthIncrement.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Paragraph;
end;

procedure TdxDocCommandTableDepthIncrement.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.ParagraphInfo.TableDepth := APropertyContainer.ParagraphInfo.TableDepth + Value;
  if APropertyContainer.ParagraphInfo.TableDepth = 0 then
    APropertyContainer.ParagraphInfo.InTable := False;
end;

{ TdxDocCommandMergeTableCellsBase }

procedure TdxDocCommandMergeTableCellsBase.Read(const ASprm: TBytes);
begin
  FFirstMergedIndex := ASprm[0];
  FLastMergedIndex := ASprm[1];
end;

procedure TdxDocCommandMergeTableCellsBase.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType),
    TBytes.Create(FFirstMergedIndex, FLastMergedIndex)));
end;

{ TdxDocCommandMergeTableCells }

function TdxDocCommandMergeTableCells.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandMergeTableCells.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableRowInfo.HorizontalMerging.Add(TdxHorizontalMergeInfo.Create(FirstMergedIndex, LastMergedIndex, False));
end;

{ TdxDocCommandSplitTableCells }

function TdxDocCommandSplitTableCells.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandSplitTableCells.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableRowInfo.HorizontalMerging.Add(TdxHorizontalMergeInfo.Create(FirstMergedIndex, LastMergedIndex, True));
end;

{ TdxDocCommandTableCellWidth }

destructor TdxDocCommandTableCellWidth.Destroy;
begin
  ColumnWidth.Free;
  inherited Destroy
end;

function TdxDocCommandTableCellWidth.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandTableCellWidth.Read(const ASprm: TBytes);
begin
  ColumnWidth := TdxColumnWidthOperand.FromByteArray(ASprm);
end;

procedure TdxDocCommandTableCellWidth.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableRowInfo.ColumnWidthActions.Add(ColumnWidth);
  FColumnWidth := nil;
end;

procedure TdxDocCommandTableCellWidth.Write(AWriter: TBinaryWriter);
var
  ASprm: TBytes;
begin
  ASprm := TdxDocCommandHelper.CreateSinglePropertyModifier(TdxDocCommandFactory.GetOpcodeByType(ClassType), ColumnWidth.GetBytes);
  AWriter.Write(ASprm);
end;

{ TdxDocCommandTableBorders80 }

constructor TdxDocCommandTableBorders80.Create;
begin
  FTableBorders := TdxTableBordersOperand80.Create;
end;

destructor TdxDocCommandTableBorders80.Destroy;
begin
  FTableBorders.Free;
  inherited Destroy
end;

function TdxDocCommandTableBorders80.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandTableBorders80.Read(const ASprm: TBytes);
begin
  FTableBorders.Free;
  FTableBorders := TdxTableBordersOperand80.FromByteArray(ASprm);
end;

procedure TdxDocCommandTableBorders80.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  TableBorders.ApplyProperties(APropertyContainer.TableInfo.TableProperties.Borders, APropertyContainer.UnitConverter);
end;

procedure TdxDocCommandTableBorders80.Write(AWriter: TBinaryWriter);
begin
  raise ENotImplemented.Create('');
end;

{ TdxDocCommandTableDefinition }

destructor TdxDocCommandTableDefinition.Destroy;
begin
  FTableDefinition.Free;
  inherited Destroy
end;

function TdxDocCommandTableDefinition.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.Table;
end;

procedure TdxDocCommandTableDefinition.Read(const ASprm: TBytes);
begin
  FTableDefinition := TdxTableDefinitionOperand.FromByteArray(ASprm);
end;

procedure TdxDocCommandTableDefinition.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableInfo.TableDefinition := FTableDefinition;
end;

procedure TdxDocCommandTableDefinition.Write(AWriter: TBinaryWriter);
begin
end;

{ TdxDocCommandTableRowBorderColorsBase }

destructor TdxDocCommandTableRowBorderColorsBase.Destroy;
begin
  FColors.Free;
  inherited Destroy;
end;

function TdxDocCommandTableRowBorderColorsBase.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandTableRowBorderColorsBase.ExtractColors;
begin
  while Colors.Count > 0 do
    Colors.Extract(Colors[0]);
end;

procedure TdxDocCommandTableRowBorderColorsBase.Read(const ASprm: TBytes);
var
  ACount, I: Integer;
begin
  ACount := Length(ASprm) div TdxDocColorReference.ColorReferenceSize;
  FColors := TdxDocTableBorderColorReferenceList.Create;
  FColors.Capacity := ACount;
  for I := 0 to ACount - 1 do
    FColors.Add(TdxDocTableBorderColorReference.FromByteArray(ASprm, I * TdxDocColorReference.ColorReferenceSize));
end;

procedure TdxDocCommandTableRowBorderColorsBase.Write(AWriter: TBinaryWriter);
var
  I: Integer;
begin
  AWriter.Write(TdxDocCommandFactory.GetOpcodeByType(ClassType));
  for I := 0 to FColors.Count - 1 do
    AWriter.Write(FColors[I].GetBytes);
end;

{ TdxDocCommandTopBorderColors }

procedure TdxDocCommandTopBorderColors.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableRowInfo.TopBorders.AddRange(Colors);
  ExtractColors;
end;

{ TdxDocCommandBottomBorderColors }

procedure TdxDocCommandBottomBorderColors.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableRowInfo.BottomBorders.AddRange(Colors);
  ExtractColors;
end;

{ TdxDocCommandLeftBorderColors }

procedure TdxDocCommandLeftBorderColors.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableRowInfo.LeftBorders.AddRange(Colors);
  ExtractColors;
end;

{ TdxDocCommandRightBorderColors }

procedure TdxDocCommandRightBorderColors.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableRowInfo.RightBorders.AddRange(Colors);
  ExtractColors;
end;

{ TdxDocCommandTableShading }

function TdxDocCommandTableShading.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandTableShading.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  if APropertyContainer.Factory.Version <= $00D9 then
    APropertyContainer.TableRowInfo.CellShading1.AddRange(CellColors);
end;

{ TdxDocCommandTableShading2 }

function TdxDocCommandTableShading2.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandTableShading2.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  if APropertyContainer.Factory.Version <= $00D9 then
    APropertyContainer.TableRowInfo.CellShading2.AddRange(CellColors);
end;

{ TdxDocCommandTableShading3 }

function TdxDocCommandTableShading3.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandTableShading3.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  if APropertyContainer.Factory.Version <= $00D9 then
    APropertyContainer.TableRowInfo.CellShading3.AddRange(CellColors);
end;

{ TdxDocCommandDefTableShd80 }

constructor TdxDocCommandDefTableShd80.Create;
begin
  FColors := TdxAlphaColorList.Create;
end;

destructor TdxDocCommandDefTableShd80.Destroy;
begin
  FColors.Free;
  inherited Destroy;
end;

function TdxDocCommandDefTableShd80.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandDefTableShd80.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  if APropertyContainer.Factory.Version <= $00D9 then
    APropertyContainer.TableRowInfo.DefaultCellsShading.AddRange(FColors);
end;

procedure TdxDocCommandDefTableShd80.Read(const ASprm: TBytes);
var
  ACount, I: Integer;
  ADescriptor: TdxDocShadingDescriptor80;
  AColor: TdxAlphaColor;
begin
  ACount := Length(ASprm) div 2;
  for I := 0 to ACount - 1 do
  begin
    ADescriptor := TdxDocShadingDescriptor80.FromByteArray(ASprm, I * 2);
    try
      AColor := GetColor(ADescriptor);
      FColors.Add(AColor);
    finally
      ADescriptor.Free;
    end;
  end;
end;

function TdxDocCommandDefTableShd80.GetColor(ADescriptor: TdxDocShadingDescriptor80): TdxAlphaColor;
begin
  if ADescriptor.ShadingPattern = 0 then
    Result := ADescriptor.BackgroundColor
  else
    Result := ADescriptor.ForeColor;
end;

procedure TdxDocCommandDefTableShd80.Write(AWriter: TBinaryWriter);
begin
  raise ENotImplemented.Create('');
end;

{ TdxDocCommandTablePropertiesException }

constructor TdxDocCommandTablePropertiesException.Create;
begin
  FTableAutoformatLookSpecifier := TLP.Create;
end;

destructor TdxDocCommandTablePropertiesException.Destroy;
begin
  FTableAutoformatLookSpecifier.Free;
  inherited Destroy;
end;

function TdxDocCommandTablePropertiesException.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.TableRow;
end;

procedure TdxDocCommandTablePropertiesException.Read(const ASprm: TBytes);
begin
  TableAutoformatLookSpecifier := TLP.FromByteArray(ASprm);
end;

procedure TdxDocCommandTablePropertiesException.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
  APropertyContainer.TableRowInfo.TableAutoformatLookSpecifier := TableAutoformatLookSpecifier.Clone;
end;

procedure TdxDocCommandTablePropertiesException.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxDocCommandFactory.GetOpcodeByType(TdxDocCommandTablePropertiesException));
  AWriter.Write(TLP.Size);
  TableAutoformatLookSpecifier.Write(AWriter);
end;

procedure TdxDocCommandTablePropertiesException.SetTableAutoformatLookSpecifier(const Value: TLP);
begin
  if FTableAutoformatLookSpecifier = Value then
    Exit;
  FTableAutoformatLookSpecifier.Free;
  FTableAutoformatLookSpecifier := Value;
end;

{ TdxDocCommandParagraphFormattingRsid }

function TdxDocCommandParagraphFormattingRsid.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.None;
end;

procedure TdxDocCommandParagraphFormattingRsid.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
end;

{ TdxDocCommandCharacterFormattingRsid }

function TdxDocCommandCharacterFormattingRsid.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.None;
end;

procedure TdxDocCommandCharacterFormattingRsid.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
end;

{ TdxDocCommandInsertTextRsid }

function TdxDocCommandInsertTextRsid.GetChangeAction: TdxChangeActionType;
begin
  Result := TdxChangeActionType.None;
end;

procedure TdxDocCommandInsertTextRsid.Execute(APropertyContainer: TdxDocPropertyContainer; ADataStreamReader: TBinaryReader);
begin
end;

end.
