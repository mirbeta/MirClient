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

unit dxRichEdit.DocumentModel.CharacterFormatting;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Graphics, dxCoreClasses, dxCoreGraphics,

  dxRichEdit.NativeApi,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.MergedProperties,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.Platform.Font;

type
  TdxStrikeoutType = TdxRichEditStrikeoutType;

  TdxUnderlineType = TdxRichEditUnderlineType;

  TdxMergedCharacterProperties = class;
  TdxCharacterFormattingInfoCache = class;

  IdxCharacterProperties = interface
  ['{EEA58078-8864-4D28-8A6C-115CA48255C6}']
    function GetAllCaps: Boolean;
    function GetBackColor: TdxAlphaColor;
    function GetDoubleFontSize: Integer;
    function GetFontBold: Boolean;
    function GetForeColor: TdxAlphaColor;
    function GetFontItalic: Boolean;
    function GetFontName: string;
    function GetFontUnderlineType: TdxUnderlineType;
    function GetFontStrikeoutType: TdxStrikeoutType;
    function GetHidden: Boolean;
    function GetNoProof: Boolean;
    function GetScript: TdxCharacterFormattingScript;
    function GetStrikeoutColor: TdxAlphaColor;
    function GetStrikeoutWordsOnly: Boolean;
    function GetUnderlineColor: TdxAlphaColor;
    function GetUnderlineWordsOnly: Boolean;

    procedure SetAllCaps(const Value: Boolean);
    procedure SetBackColor(const Value: TdxAlphaColor);
    procedure SetDoubleFontSize(const Value: Integer);
    procedure SetFontBold(const Value: Boolean);
    procedure SetForeColor(const Value: TdxAlphaColor);
    procedure SetFontItalic(const Value: Boolean);
    procedure SetFontName(const Value: string);
    procedure SetFontUnderlineType(const Value: TdxUnderlineType);
    procedure SetFontStrikeoutType(const Value: TdxStrikeoutType);
    procedure SetHidden(const Value: Boolean);
    procedure SetNoProof(const Value: Boolean);
    procedure SetScript(const Value: TdxCharacterFormattingScript);
    procedure SetStrikeoutColor(const Value: TdxAlphaColor);
    procedure SetStrikeoutWordsOnly(const Value: Boolean);
    procedure SetUnderlineColor(const Value: TdxAlphaColor);
    procedure SetUnderlineWordsOnly(const Value: Boolean);

    property AllCaps: Boolean read GetAllCaps write SetAllCaps;
    property BackColor: TdxAlphaColor read GetBackColor write SetBackColor;
    property DoubleFontSize: Integer read GetDoubleFontSize write SetDoubleFontSize;
    property FontBold: Boolean read GetFontBold write SetFontBold;
    property ForeColor: TdxAlphaColor read GetForeColor write SetForeColor;
    property FontItalic: Boolean read GetFontItalic write SetFontItalic;
    property FontName: string read GetFontName write SetFontName;
    property FontUnderlineType: TdxUnderlineType read GetFontUnderlineType write SetFontUnderlineType;
    property FontStrikeoutType: TdxStrikeoutType read GetFontStrikeoutType write SetFontStrikeoutType;
    property Hidden: Boolean read GetHidden write SetHidden;
    property NoProof: Boolean read GetNoProof write SetNoProof;
    property Script: TdxCharacterFormattingScript read GetScript write SetScript;
    property StrikeoutColor: TdxAlphaColor read GetStrikeoutColor write SetStrikeoutColor;
    property StrikeoutWordsOnly: Boolean read GetStrikeoutWordsOnly write SetStrikeoutWordsOnly;
    property UnderlineColor: TdxAlphaColor read GetUnderlineColor write SetUnderlineColor;
    property UnderlineWordsOnly: Boolean read GetUnderlineWordsOnly write SetUnderlineWordsOnly;
  end;

  IdxCharacterPropertiesContainer = interface
  ['{6F091A9C-85EB-4768-82EB-4D0D7554A507}']
    function GetPieceTable: TdxCustomPieceTable;
    function CreateCharacterPropertiesChangedHistoryItem: TdxIndexChangedHistoryItemCore;
    procedure OnCharacterPropertiesChanged;

    property PieceTable: TdxCustomPieceTable read GetPieceTable;
  end;

  { TdxCharacterFormattingInfo }

  TdxCharacterFormattingInfo = class(TdxCloneableIUnknownObject,
    IdxCharacterProperties)
  private const
    MaskUnderlineType             = $00000001F;
    MaskStrikeoutType             = $000000060;
    MaskCharacterFormattingScript = $000000180;
    MaskFontBold                  = $000000200;
    MaskFontItalic                = $000000400;
    MaskAllCaps                   = $000000800;
    MaskUnderlineWordsOnly        = $000001000;
    MaskStrikeoutWordsOnly        = $000002000;
    MaskHidden                    = $000004000;
    MaskNoProof                   = $000008000;
    MaskDoubleFontSize            = $0FFFF0000;
  private
    FPackedValues: Cardinal;
    FBackColor: TdxAlphaColor;
    FForeColor: TdxAlphaColor;
    FStrikeoutColor: TdxAlphaColor;
    FUnderlineColor: TdxAlphaColor;
    FFontName: string;
    function GetBooleanValue(AMask: Cardinal): Boolean;
    procedure SetBooleanValue(AMask: Cardinal; AValue: Boolean);

    //IdxCharacterProperties
    function GetAllCaps: Boolean;
    function GetBackColor: TdxAlphaColor;
    function GetFontBold: Boolean;
    function GetFontItalic: Boolean;
    function GetFontName: string;
    function GetDoubleFontSize: Integer;
    function GetFontStrikeoutType: TdxStrikeoutType;
    function GetFontUnderlineType: TdxUnderlineType;
    function GetForeColor: TdxAlphaColor;
    function GetHidden: Boolean;
    function GetNoProof: Boolean;
    function GetScript: TdxCharacterFormattingScript;
    function GetStrikeoutColor: TdxAlphaColor;
    function GetStrikeoutWordsOnly: Boolean;
    function GetUnderlineColor: TdxAlphaColor;
    function GetUnderlineWordsOnly: Boolean;
    procedure SetAllCaps(const Value: Boolean);
    procedure SetBackColor(const Value: TdxAlphaColor);
    procedure SetFontBold(const Value: Boolean);
    procedure SetFontItalic(const Value: Boolean);
    procedure SetFontName(const Value: string);
    procedure SetDoubleFontSize(const Value: Integer);
    procedure SetFontStrikeoutType(const Value: TdxStrikeoutType);
    procedure SetFontUnderlineType(const Value: TdxUnderlineType);
    procedure SetForeColor(const Value: TdxAlphaColor);
    procedure SetHidden(const Value: Boolean);
    procedure SetNoProof(const Value: Boolean);
    procedure SetScript(const Value: TdxCharacterFormattingScript);
    procedure SetStrikeoutColor(const Value: TdxAlphaColor);
    procedure SetStrikeoutWordsOnly(const Value: Boolean);
    procedure SetUnderlineColor(const Value: TdxAlphaColor);
    procedure SetUnderlineWordsOnly(const Value: Boolean);

  protected
    property PackedValues: Cardinal read FPackedValues;
  public
    constructor Create; override;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Clone: TdxCharacterFormattingInfo; reintroduce; inline;

    function Equals(Obj: TObject): Boolean; override;
    function GetHashCode: Integer; override;

    class function CreateDefaultItem: TdxCharacterFormattingInfo; static;

    property AllCaps: Boolean read GetAllCaps write SetAllCaps;
    property BackColor: TdxAlphaColor read GetBackColor write SetBackColor;
    property DoubleFontSize: Integer read GetDoubleFontSize write SetDoubleFontSize;
    property FontBold: Boolean read GetFontBold write SetFontBold;
    property FontItalic: Boolean read GetFontItalic write SetFontItalic;
    property FontName: string read GetFontName write SetFontName;
    property FontStrikeoutType: TdxStrikeoutType read GetFontStrikeoutType write SetFontStrikeoutType;
    property FontUnderlineType: TdxUnderlineType read GetFontUnderlineType write SetFontUnderlineType;
    property ForeColor: TdxAlphaColor read GetForeColor write SetForeColor;
    property Hidden: Boolean read GetHidden write SetHidden;
    property NoProof: Boolean read GetNoProof write SetNoProof;
    property Script: TdxCharacterFormattingScript read GetScript write SetScript;
    property StrikeoutColor: TdxAlphaColor read GetStrikeoutColor write SetStrikeoutColor;
    property StrikeoutWordsOnly: Boolean read GetStrikeoutWordsOnly write SetStrikeoutWordsOnly;
    property UnderlineColor: TdxAlphaColor read GetUnderlineColor write SetUnderlineColor;
    property UnderlineWordsOnly: Boolean read GetUnderlineWordsOnly write SetUnderlineWordsOnly;
  end;
  TdxCharacterFormattingInfoClass = class of TdxCharacterFormattingInfo;

  TdxUsedCharacterFormattingOption = TdxRichEditUsedCharacterFormattingOption;

  TdxUsedCharacterFormattingOptions = set of TdxUsedCharacterFormattingOption;

  { TdxCharacterFormattingOptions }

  PdxCharacterFormattingOptions = ^TdxCharacterFormattingOptions;
  TdxCharacterFormattingOptions = record
  public const
    MaskUseAll = [
      TdxUsedCharacterFormattingOption.UseFontName,
      TdxUsedCharacterFormattingOption.UseDoubleFontSize,
      TdxUsedCharacterFormattingOption.UseFontBold,
      TdxUsedCharacterFormattingOption.UseFontItalic,
      TdxUsedCharacterFormattingOption.UseFontStrikeoutType,
      TdxUsedCharacterFormattingOption.UseFontUnderlineType,
      TdxUsedCharacterFormattingOption.UseAllCaps,
      TdxUsedCharacterFormattingOption.UseForeColor,
      TdxUsedCharacterFormattingOption.UseBackColor,
      TdxUsedCharacterFormattingOption.UseUnderlineColor,
      TdxUsedCharacterFormattingOption.UseStrikeoutColor,
      TdxUsedCharacterFormattingOption.UseUnderlineWordsOnly,
      TdxUsedCharacterFormattingOption.UseStrikeoutWordsOnly,
      TdxUsedCharacterFormattingOption.UseScript,
      TdxUsedCharacterFormattingOption.UseHidden,
      TdxUsedCharacterFormattingOption.UseNoProof];
    MaskUseNone = [];
  private
    FValue: TdxUsedCharacterFormattingOptions;
    function GetValue(const AOption: TdxUsedCharacterFormattingOption): Boolean;
    procedure SetCoreValue(const Value: TdxUsedCharacterFormattingOptions);
    class function GetEmptyCharacterFormattingOption: TdxCharacterFormattingOptions; static;
    class function GetRootCharacterFormattingOption: TdxCharacterFormattingOptions; static;
  public
    constructor Create(AUsedValues: TdxUsedCharacterFormattingOptions);
    class operator Equal(const A, B: TdxCharacterFormattingOptions): Boolean;
    class property EmptyCharacterFormattingOption: TdxCharacterFormattingOptions read GetEmptyCharacterFormattingOption;
    class property RootCharacterFormattingOption: TdxCharacterFormattingOptions read GetRootCharacterFormattingOption;

    procedure CopyFrom(const Source: TdxCharacterFormattingOptions);
    procedure Init;
    function Clone: TdxCharacterFormattingOptions;
    function GetHashCode: Integer;

    procedure ResetUse(AOptions: TdxUsedCharacterFormattingOptions);
    procedure SetValue(const AOption: TdxUsedCharacterFormattingOption; const Value: Boolean);

    property Value: TdxUsedCharacterFormattingOptions read FValue write SetCoreValue;
    property UseFontName: Boolean index TdxUsedCharacterFormattingOption.UseFontName read GetValue write SetValue;
    property UseDoubleFontSize: Boolean index TdxUsedCharacterFormattingOption.UseDoubleFontSize read GetValue write SetValue;
    property UseFontBold: Boolean index TdxUsedCharacterFormattingOption.UseFontBold read GetValue write SetValue;
    property UseFontItalic: Boolean index TdxUsedCharacterFormattingOption.UseFontItalic read GetValue write SetValue;
    property UseFontStrikeoutType: Boolean index TdxUsedCharacterFormattingOption.UseFontStrikeoutType read GetValue write SetValue;
    property UseFontUnderlineType: Boolean index TdxUsedCharacterFormattingOption.UseFontUnderlineType read GetValue write SetValue;
    property UseAllCaps: Boolean index TdxUsedCharacterFormattingOption.UseAllCaps read GetValue write SetValue;
    property UseForeColor: Boolean index TdxUsedCharacterFormattingOption.UseForeColor read GetValue write SetValue;
    property UseBackColor: Boolean index TdxUsedCharacterFormattingOption.UseBackColor read GetValue write SetValue;
    property UseUnderlineColor: Boolean index TdxUsedCharacterFormattingOption.UseUnderlineColor read GetValue write SetValue;
    property UseStrikeoutColor: Boolean index TdxUsedCharacterFormattingOption.UseStrikeoutColor read GetValue write SetValue;
    property UseUnderlineWordsOnly: Boolean index TdxUsedCharacterFormattingOption.UseUnderlineWordsOnly read GetValue write SetValue;
    property UseStrikeoutWordsOnly: Boolean index TdxUsedCharacterFormattingOption.UseStrikeoutWordsOnly read GetValue write SetValue;
    property UseScript: Boolean index TdxUsedCharacterFormattingOption.UseScript read GetValue write SetValue;
    property UseHidden: Boolean index TdxUsedCharacterFormattingOption.UseHidden read GetValue write SetValue;
    property UseNoProof: Boolean index TdxUsedCharacterFormattingOption.UseNoProof read GetValue write SetValue;
  end;

  { TdxCharacterFormattingBase }

  TdxCharacterFormattingBase = class(TdxIndexBasedObjectB<TdxCharacterFormattingInfo, TdxCharacterFormattingOptions>,
    IdxCharacterProperties)
  private
    //IdxCharacterProperties
    function GetAllCaps: Boolean;
    function GetBackColor: TdxAlphaColor;
    function GetFontBold: Boolean;
    function GetForeColor: TdxAlphaColor;
    function GetFontItalic: Boolean;
    function GetFontName: string;
    function GetDoubleFontSize: Integer;
    function GetFontUnderlineType: TdxUnderlineType;
    function GetFontStrikeoutType: TdxStrikeoutType;
    function GetHidden: Boolean;
    function GetNoProof: Boolean;
    function GetScript: TdxCharacterFormattingScript;
    function GetStrikeoutColor: TdxAlphaColor;
    function GetStrikeoutWordsOnly: Boolean;
    function GetUnderlineColor: TdxAlphaColor;
    function GetUnderlineWordsOnly: Boolean;

    procedure SetAllCaps(const Value: Boolean);
    procedure SetAllCapsCore(const AInfo: TdxCharacterFormattingInfo; const AValue: Boolean);
    procedure SetBackColor(const Value: TdxAlphaColor);
    procedure SetBackColorCore(const AInfo: TdxCharacterFormattingInfo; const AValue: TdxAlphaColor);
    procedure SetDoubleFontSize(const Value: Integer);
    procedure SetDoubleFontSizeCore(const AInfo: TdxCharacterFormattingInfo; const AValue: Integer);
    procedure SetFontBold(const Value: Boolean);
    procedure SetFontBoldCore(const AInfo: TdxCharacterFormattingInfo; const AValue: Boolean);
    procedure SetForeColor(const Value: TdxAlphaColor);
    procedure SetForeColorCore(const AInfo: TdxCharacterFormattingInfo; const AValue: TdxAlphaColor);
    procedure SetFontItalic(const Value: Boolean);
    procedure SetFontItalicCore(const AInfo: TdxCharacterFormattingInfo; const AValue: Boolean);
    procedure SetFontName(const Value: string);
    procedure SetFontNameCore(const AInfo: TdxCharacterFormattingInfo; const AValue: string);
    procedure SetFontUnderlineType(const Value: TdxUnderlineType);
    procedure SetFontUnderlineTypeCore(const AInfo: TdxCharacterFormattingInfo; const AValue: TdxUnderlineType);
    procedure SetFontStrikeoutType(const Value: TdxStrikeoutType);
    procedure SetFontStrikeoutTypeCore(const AInfo: TdxCharacterFormattingInfo; const AValue: TdxStrikeoutType);
    procedure SetHidden(const Value: Boolean);
    procedure SetHiddenCore(const AInfo: TdxCharacterFormattingInfo; const AValue: Boolean);
    procedure SetNoProof(const Value: Boolean);
    procedure SetNoProofCore(const AInfo: TdxCharacterFormattingInfo; const Value: Boolean);
    procedure SetScript(const Value: TdxCharacterFormattingScript);
    procedure SetScriptCore(const AInfo: TdxCharacterFormattingInfo; const AValue: TdxCharacterFormattingScript);
    procedure SetStrikeoutColor(const Value: TdxAlphaColor);
    procedure SetStrikeoutColorCore(const AInfo: TdxCharacterFormattingInfo; const AValue: TdxAlphaColor);
    procedure SetStrikeoutWordsOnly(const Value: Boolean);
    procedure SetStrikeoutWordsOnlyCore(const AInfo: TdxCharacterFormattingInfo; const AValue: Boolean);
    procedure SetUnderlineColor(const Value: TdxAlphaColor);
    procedure SetUnderlineColorCore(const AInfo: TdxCharacterFormattingInfo; const AValue: TdxAlphaColor);
    procedure SetUnderlineWordsOnly(const Value: Boolean);
    procedure SetUnderlineWordsOnlyCore(const AInfo: TdxCharacterFormattingInfo; const AValue: Boolean);
  protected
    function CloneCore: TdxCharacterFormattingBase; virtual;
    function PropertyEquals(const AOther: TdxIndexBasedObject<TdxCharacterFormattingInfo, TdxCharacterFormattingOptions>): Boolean; override;
    function SupportsFormatting: Boolean;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const ADocumentModel: TdxCustomDocumentModel;
      const AFormattingInfo: TdxCharacterFormattingInfo; const AFormattingOptions: TdxCharacterFormattingOptions); reintroduce;

    procedure CopyFrom(Source: TdxCloneable); overload; override;
    procedure CopyFrom(const AInfo: TdxCharacterFormattingInfo; const AOptions: TdxCharacterFormattingOptions); reintroduce; overload;
    procedure ResetUse(AResetOptions: TdxUsedCharacterFormattingOptions);
    function Clone: TdxCloneable; override;

    property AllCaps: Boolean read GetAllCaps write SetAllCaps;
    property BackColor: TdxAlphaColor read GetBackColor write SetBackColor;
    property DoubleFontSize: Integer read GetDoubleFontSize write SetDoubleFontSize;
    property FontBold: Boolean read GetFontBold write SetFontBold;
    property ForeColor: TdxAlphaColor read GetForeColor write SetForeColor;
    property FontItalic: Boolean read GetFontItalic write SetFontItalic;
    property FontName: string read GetFontName write SetFontName;
    property FontUnderlineType: TdxUnderlineType read GetFontUnderlineType write SetFontUnderlineType;
    property FontStrikeoutType: TdxStrikeoutType read GetFontStrikeoutType write SetFontStrikeoutType;
    property Hidden: Boolean read GetHidden write SetHidden;
    property NoProof: Boolean read GetNoProof write SetNoProof;

    property Script: TdxCharacterFormattingScript read GetScript write SetScript;
    property StrikeoutColor: TdxAlphaColor read GetStrikeoutColor write SetStrikeoutColor;
    property StrikeoutWordsOnly: Boolean read GetStrikeoutWordsOnly write SetStrikeoutWordsOnly;
    property UnderlineColor: TdxAlphaColor read GetUnderlineColor write SetUnderlineColor;
    property UnderlineWordsOnly: Boolean read GetUnderlineWordsOnly write SetUnderlineWordsOnly;
  end;

  { TdxCharacterProperties }

  TdxCharacterProperties = class(TdxRichEditIndexBasedObject<TdxCharacterFormattingBase>, IdxCharacterProperties)
  private
    FOwner: IdxCharacterPropertiesContainer;
  strict protected
    function GetUseFontName: Boolean;
    function GetUseDoubleFontSize: Boolean;
    function GetUseFontBold: Boolean;
    function GetUseFontItalic: Boolean;
    function GetUseFontStrikeoutType: Boolean;
    function GetUseFontUnderlineType: Boolean;
    function GetUseAllCaps: Boolean;
    function GetUseForeColor: Boolean;
    function GetUseBackColor: Boolean;
    function GetUseUnderlineColor: Boolean;
    function GetUseStrikeoutColor: Boolean;
    function GetUseUnderlineWordsOnly: Boolean;
    function GetUseStrikeoutWordsOnly: Boolean;
    function GetUseScript: Boolean;
    function GetUseHidden: Boolean;
    //IdxCharacterProperties
    function GetAllCaps: Boolean;
    function GetBackColor: TdxAlphaColor;
    function GetFontBold: Boolean;
    function GetFontItalic: Boolean;
    function GetFontName: string;
    function GetDoubleFontSize: Integer;
    function GetFontStrikeoutType: TdxStrikeoutType;
    function GetFontUnderlineType: TdxUnderlineType;
    function GetForeColor: TdxAlphaColor;
    function GetHidden: Boolean;
    function GetNoProof: Boolean;
    function GetScript: TdxCharacterFormattingScript;
    function GetStrikeoutColor: TdxAlphaColor;
    function GetStrikeoutWordsOnly: Boolean;
    function GetUnderlineColor: TdxAlphaColor;
    function GetUnderlineWordsOnly: Boolean;
    procedure SetAllCaps(const Value: Boolean);
    function SetAllCapsCore(const AInfo: TdxCharacterFormattingBase; const Value: Boolean): TdxDocumentModelChangeActions;
    procedure SetBackColor(const Value: TdxAlphaColor);
    function SetBackColorCore(const AInfo: TdxCharacterFormattingBase; const Value: TdxAlphaColor): TdxDocumentModelChangeActions;
    procedure SetDoubleFontSize(const Value: Integer);
    function SetDoubleFontSizeCore(const AInfo: TdxCharacterFormattingBase; const Value: Integer): TdxDocumentModelChangeActions;
    procedure SetFontBold(const Value: Boolean);
    function SetFontBoldCore(const AInfo: TdxCharacterFormattingBase; const Value: Boolean): TdxDocumentModelChangeActions;
    procedure SetFontItalic(const Value: Boolean);
    function SetFontItalicCore(const AInfo: TdxCharacterFormattingBase; const Value: Boolean): TdxDocumentModelChangeActions;
    procedure SetFontName(const Value: string);
    function SetFontNameCore(const AInfo: TdxCharacterFormattingBase; const Value: string): TdxDocumentModelChangeActions;
    procedure SetFontStrikeoutType(const Value: TdxStrikeoutType);
    function SetFontStrikeoutTypeCore(const AInfo: TdxCharacterFormattingBase; const Value: Integer): TdxDocumentModelChangeActions;
    procedure SetFontUnderlineType(const Value: TdxUnderlineType);
    function SetFontUnderlineTypeCore(const AInfo: TdxCharacterFormattingBase; const Value: Integer): TdxDocumentModelChangeActions;
    procedure SetForeColor(const Value: TdxAlphaColor);
    function SetForeColorCore(const AInfo: TdxCharacterFormattingBase; const Value: TdxAlphaColor): TdxDocumentModelChangeActions;
    procedure SetHidden(const Value: Boolean);
    function SetHiddenCore(const AInfo: TdxCharacterFormattingBase; const Value: Boolean): TdxDocumentModelChangeActions;
    procedure SetScript(const Value: TdxCharacterFormattingScript);
    function SetScriptCore(const AInfo: TdxCharacterFormattingBase; const Value: Integer): TdxDocumentModelChangeActions;
    procedure SetStrikeoutColor(const Value: TdxAlphaColor);
    function SetStrikeoutColorCore(const AInfo: TdxCharacterFormattingBase; const Value: TdxAlphaColor): TdxDocumentModelChangeActions;
    procedure SetStrikeoutWordsOnly(const Value: Boolean);
    function SetStrikeoutWordsOnlyCore(const AInfo: TdxCharacterFormattingBase; const Value: Boolean): TdxDocumentModelChangeActions;
    procedure SetUnderlineColor(const Value: TdxAlphaColor);
    function SetUnderlineColorCore(const AInfo: TdxCharacterFormattingBase; const Value: TdxAlphaColor): TdxDocumentModelChangeActions;
    procedure SetUnderlineWordsOnly(const Value: Boolean);
    function SetUnderlineWordsOnlyCore(const AInfo: TdxCharacterFormattingBase; const Value: Boolean): TdxDocumentModelChangeActions;
    procedure SetNoProof(const Value: Boolean);
    function SetNoProofCore(const AInfo: TdxCharacterFormattingBase; const AValue: Boolean): TdxDocumentModelChangeActions;
    function GetUseNoProof: Boolean;

    procedure CopyFrom(const ACharacterProperties: TdxMergedProperties<TdxCharacterFormattingInfo, TdxCharacterFormattingOptions>); overload;
    function CreateIndexChangedHistoryItem: TdxIndexChangedHistoryItemCore; override;
    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxCharacterFormattingBase>; override;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    procedure OnIndexChanged; override;
    property Owner: IdxCharacterPropertiesContainer read FOwner;
  public
    constructor Create(const AOwner: IdxCharacterPropertiesContainer); reintroduce;

    class procedure ApplyPropertiesDiff(ATarget: TdxCharacterProperties; ATargetMergedInfo, ASourceMergedInfo: TdxCharacterFormattingInfo);
    procedure CopyFrom(const AProperties: TdxMergedCharacterProperties); overload;
  {$IFNDEF DELPHI17}
    procedure CopyFrom(const Source: TdxUndoableIndexBasedObject<TdxCharacterFormattingBase>); override; final;
    procedure CopyFrom(const Source: TdxCharacterFormattingBase); override; final;
  {$ENDIF}
    procedure Merge(const AProperties: TdxCharacterProperties);
    procedure Reset;
    procedure ResetAllUse;
    procedure ResetUse(const AResetOption: TdxUsedCharacterFormattingOption); overload;
    procedure ResetUse(const AResetOptions: TdxUsedCharacterFormattingOptions); overload;
    function UseVal(AMask: TdxUsedCharacterFormattingOption): Boolean;

    function GetObtainAffectedRangeListener: IdxObtainAffectedRangeListener; override;

    property AllCaps: Boolean read GetAllCaps write SetAllCaps;
    property BackColor: TdxAlphaColor read GetBackColor write SetBackColor;
    property DoubleFontSize: Integer read GetDoubleFontSize write SetDoubleFontSize;
    property FontBold: Boolean read GetFontBold write SetFontBold;
    property FontItalic: Boolean read GetFontItalic write SetFontItalic;
    property FontName: string read GetFontName write SetFontName;
    property FontStrikeoutType: TdxStrikeoutType read GetFontStrikeoutType write SetFontStrikeoutType;
    property FontUnderlineType: TdxUnderlineType read GetFontUnderlineType write SetFontUnderlineType;
    property ForeColor: TdxAlphaColor read GetForeColor write SetForeColor;
    property Hidden: Boolean read GetHidden write SetHidden;
    property NoProof: Boolean read GetNoProof write SetNoProof;
    property Script: TdxCharacterFormattingScript read GetScript write SetScript;
    property StrikeoutColor: TdxAlphaColor read GetStrikeoutColor write SetStrikeoutColor;
    property StrikeoutWordsOnly: Boolean read GetStrikeoutWordsOnly write SetStrikeoutWordsOnly;
    property UnderlineColor: TdxAlphaColor read GetUnderlineColor write SetUnderlineColor;
    property UnderlineWordsOnly: Boolean read GetUnderlineWordsOnly write SetUnderlineWordsOnly;

    property UseAllCaps: Boolean read GetUseAllCaps;
    property UseBackColor: Boolean read GetUseBackColor;
    property UseDoubleFontSize: Boolean read GetUseDoubleFontSize;
    property UseFontBold: Boolean read GetUseFontBold;
    property UseFontItalic: Boolean read GetUseFontItalic;
    property UseFontName: Boolean read GetUseFontName;
    property UseFontStrikeoutType: Boolean read GetUseFontStrikeoutType;
    property UseFontUnderlineType: Boolean read GetUseFontUnderlineType;
    property UseForeColor: Boolean read GetUseForeColor;
    property UseHidden: Boolean read GetUseHidden;
    property UseNoProof: Boolean read GetUseNoProof;
    property UseScript: Boolean read GetUseScript;
    property UseStrikeoutColor: Boolean read GetUseStrikeoutColor;
    property UseStrikeoutWordsOnly: Boolean read GetUseStrikeoutWordsOnly;
    property UseUnderlineColor: Boolean read GetUseUnderlineColor;
    property UseUnderlineWordsOnly: Boolean read GetUseUnderlineWordsOnly;
  end;

  { TdxMergedCharacterProperties }

  TdxMergedCharacterProperties = class(TdxMergedProperties<TdxCharacterFormattingInfo, TdxCharacterFormattingOptions>)
  protected
    procedure MergeCore(const AInfo: TdxCharacterFormattingInfo; const AOptions: TdxCharacterFormattingOptions);
  public
    constructor Create(const AProperties: TdxCharacterProperties); reintroduce; overload;
    constructor Create(const AProperties: TdxMergedCharacterProperties); reintroduce; overload;

    procedure Merge(const AProperties: TdxCharacterProperties); overload;
    procedure Merge(const AProperties: TdxMergedCharacterProperties); overload;
  end;

  { TdxCharacterFormattingInfoCache }

  TdxCharacterFormattingInfoCache = class(TdxUniqueItemsCache<TdxCharacterFormattingInfo>)
  public const
    DefaultItemIndex = 0;
  public
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxCharacterFormattingInfo; override;
  end;

  { TdxCharacterFormattingCache }

  TdxCharacterFormattingCache = class(TdxUniqueItemsCache<TdxCharacterFormattingBase>)
  public const
    RootCharacterFormattingIndex  = 1;
    EmptyCharacterFormattingIndex = 0;
  public
    constructor Create(const ADocumentModel: TdxCustomDocumentModel); reintroduce;

    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxCharacterFormattingBase; override;
  end;

  { TdxCharacterPropertiesFontAssignmentHelper }

  TdxCharacterPropertiesFontAssignmentHelper = class
  public
    class procedure AssignFont(const ACharacterProperties: IdxCharacterProperties; AFont: TFont); static;
  end;

  TdxCharacterFormattingChangeType = (
    None,
    FontName,
    FontSize,
    FontBold,
    FontItalic,
    FontUnderlineType,
    FontStrikeoutType,
    AllCaps,
    ForeColor,
    BackColor,
    UnderlineColor,
    StrikeoutColor,
    UnderlineWordsOnly,
    StrikeoutWordsOnly,
    Script,
    CharacterStyle,
    Hidden,
    NoProof,
    BatchUpdate
  );

  { TdxCharacterFormattingChangeActionsCalculator }

	TdxCharacterFormattingChangeActionsCalculator = class
  public
    class function CalculateChangeActions(const AChange: TdxCharacterFormattingChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxParagraphMergedCharacterPropertiesCachedResult }

  TdxParagraphMergedCharacterPropertiesCachedResult = class
  strict private
    FParagraphStyleIndex: Integer;
    FTableCell: TObject;
    FMergedCharacterProperties: TdxMergedCharacterProperties;
  private
    procedure SetMergedCharacterProperties(const Value: TdxMergedCharacterProperties);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property ParagraphStyleIndex: Integer read FParagraphStyleIndex write FParagraphStyleIndex;
    property TableCell: TObject read FTableCell write FTableCell;
    property MergedCharacterProperties: TdxMergedCharacterProperties read FMergedCharacterProperties write SetMergedCharacterProperties;
  end;

  { TdxRunMergedCharacterPropertiesCachedResult }

  TdxRunMergedCharacterPropertiesCachedResult = class(TdxParagraphMergedCharacterPropertiesCachedResult)
  strict private
    FCharacterStyleIndex: Integer;
    FCharacterPropertiesIndex: Integer;
  public
    constructor Create; override;
    property CharacterStyleIndex: Integer read FCharacterStyleIndex write FCharacterStyleIndex;
    property CharacterPropertiesIndex: Integer read FCharacterPropertiesIndex write FCharacterPropertiesIndex;
  end;


implementation

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  RTLConsts,
  Windows,
  dxHash,
  dxHashUtils,
  dxRichEdit.DocumentModel.Cache.Simple,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.BatchUpdateHelper;

type
  { TdxDefaultCharacterFormattingBase }

  TdxDefaultCharacterFormattingBase = class(TdxCharacterFormattingBase)
  strict private
    FDefaultInfo: TdxCharacterFormattingInfo;
  protected
    function CloneCore: TdxCharacterFormattingBase; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const ADocumentModel: TdxCustomDocumentModel;
      const AFormattingInfo: TdxCharacterFormattingInfo; const AFormattingOptions: TdxCharacterFormattingOptions);
    destructor Destroy; override;
  end;

constructor TdxDefaultCharacterFormattingBase.Create(APieceTable: TdxCustomPieceTable; const ADocumentModel: TdxCustomDocumentModel;
  const AFormattingInfo: TdxCharacterFormattingInfo; const AFormattingOptions: TdxCharacterFormattingOptions);
begin
  inherited Create(APieceTable, ADocumentModel, nil, AFormattingOptions);
  FDefaultInfo := AFormattingInfo;
  SetInfo(AFormattingInfo);
end;

destructor TdxDefaultCharacterFormattingBase.Destroy;
begin
  SetInfo(nil);
  inherited Destroy;
end;

function TdxDefaultCharacterFormattingBase.CloneCore: TdxCharacterFormattingBase;
begin
  Result := TdxCharacterFormattingBase.Create(PieceTable, DocumentModel, Info, Options);
end;

{ TdxCharacterFormattingInfo }

constructor TdxCharacterFormattingInfo.Create;
begin
  inherited Create;
  FForeColor := TdxAlphaColors.Empty;
  FBackColor := TdxAlphaColors.Transparent;
  FUnderlineColor := TdxAlphaColors.Empty;
  FStrikeoutColor := TdxAlphaColors.Empty;
end;


function TdxCharacterFormattingInfo.GetBooleanValue(AMask: Cardinal): Boolean;
begin
  Result := FPackedValues and AMask <> 0;
end;

procedure TdxCharacterFormattingInfo.SetBooleanValue(AMask: Cardinal; AValue: Boolean);
begin
  if AValue then
    FPackedValues := FPackedValues or AMask
  else
    FPackedValues := FPackedValues and not AMask;
end;

function TdxCharacterFormattingInfo.GetAllCaps: Boolean;
begin
  Result := GetBooleanValue(MaskAllCaps);
end;

function TdxCharacterFormattingInfo.GetBackColor: TdxAlphaColor;
begin
  Result := FBackColor;
end;

function TdxCharacterFormattingInfo.GetFontBold: Boolean;
begin
  Result := GetBooleanValue(MaskFontBold);
end;

function TdxCharacterFormattingInfo.GetFontItalic: Boolean;
begin
  Result := GetBooleanValue(MaskFontItalic);
end;

function TdxCharacterFormattingInfo.GetFontName: string;
begin
  Result := FFontName;
end;

class function TdxCharacterFormattingInfo.CreateDefaultItem: TdxCharacterFormattingInfo;
begin
  Result := TdxCharacterFormattingInfo.Create;
  Result.FontName := TdxRichEditControlCompatibility.DefaultFontName;
  Result.DoubleFontSize := TdxRichEditControlCompatibility.DefaultDoubleFontSize;
end;

function TdxCharacterFormattingInfo.GetDoubleFontSize: Integer;
begin
  Result := LongRec(FPackedValues).Hi;
end;

function TdxCharacterFormattingInfo.GetFontStrikeoutType: TdxStrikeoutType;
begin
  Result := TdxStrikeoutType((FPackedValues and MaskStrikeoutType) shr 5);
end;

function TdxCharacterFormattingInfo.GetFontUnderlineType: TdxUnderlineType;
begin
  Result := TdxUnderlineType(FPackedValues and MaskUnderlineType);
end;

function TdxCharacterFormattingInfo.GetForeColor: TdxAlphaColor;
begin
  Result := FForeColor;
end;

function TdxCharacterFormattingInfo.GetHidden: Boolean;
begin
  Result := GetBooleanValue(MaskHidden);
end;

function TdxCharacterFormattingInfo.GetNoProof: Boolean;
begin
  Result := GetBooleanValue(MaskNoProof);
end;

function TdxCharacterFormattingInfo.GetScript: TdxCharacterFormattingScript;
begin
  Result := TdxCharacterFormattingScript((FPackedValues and MaskCharacterFormattingScript) shr 7);
end;

function TdxCharacterFormattingInfo.GetStrikeoutColor: TdxAlphaColor;
begin
  Result := FStrikeoutColor;
end;

function TdxCharacterFormattingInfo.GetStrikeoutWordsOnly: Boolean;
begin
  Result := GetBooleanValue(MaskStrikeoutWordsOnly);
end;

function TdxCharacterFormattingInfo.GetUnderlineColor: TdxAlphaColor;
begin
  Result := FUnderlineColor;
end;

function TdxCharacterFormattingInfo.GetUnderlineWordsOnly: Boolean;
begin
  Result := GetBooleanValue(MaskUnderlineWordsOnly);
end;

procedure TdxCharacterFormattingInfo.SetAllCaps(const Value: Boolean);
begin
  SetBooleanValue(MaskAllCaps, Value);
end;

procedure TdxCharacterFormattingInfo.SetBackColor(const Value: TdxAlphaColor);
begin
  FBackColor := Value;
end;

procedure TdxCharacterFormattingInfo.SetFontBold(const Value: Boolean);
begin
  SetBooleanValue(MaskFontBold, Value);
end;

procedure TdxCharacterFormattingInfo.SetFontItalic(const Value: Boolean);
begin
  SetBooleanValue(MaskFontItalic, Value);
end;

procedure TdxCharacterFormattingInfo.SetFontName(const Value: string);
begin
  FFontName := Value;
end;

procedure TdxCharacterFormattingInfo.SetDoubleFontSize(const Value: Integer);
begin
  Assert(Value > 0, 'Bad font size');
  LongRec(FPackedValues).Hi := Word(Value);
end;

procedure TdxCharacterFormattingInfo.SetFontStrikeoutType(const Value: TdxStrikeoutType);
begin
  FPackedValues := FPackedValues and not MaskStrikeoutType;
  FPackedValues := FPackedValues or ((Cardinal(Ord(Value)) shl 5) and MaskStrikeoutType);
end;

procedure TdxCharacterFormattingInfo.SetFontUnderlineType(const Value: TdxUnderlineType);
begin
  FPackedValues := FPackedValues and not MaskUnderlineType;
  FPackedValues := FPackedValues or (Cardinal(Ord(Value)) and MaskUnderlineType);
end;

procedure TdxCharacterFormattingInfo.SetForeColor(const Value: TdxAlphaColor);
begin
  FForeColor := Value;
end;

procedure TdxCharacterFormattingInfo.SetHidden(const Value: Boolean);
begin
  SetBooleanValue(MaskHidden, Value);
end;

procedure TdxCharacterFormattingInfo.SetNoProof(const Value: Boolean);
begin
  SetBooleanValue(MaskNoProof, Value);
end;

procedure TdxCharacterFormattingInfo.SetScript(const Value: TdxCharacterFormattingScript);
begin
  FPackedValues := FPackedValues and not MaskCharacterFormattingScript;
  FPackedValues := FPackedValues or ((Cardinal(Ord(Value)) shl 7) and MaskCharacterFormattingScript);
end;

procedure TdxCharacterFormattingInfo.SetStrikeoutColor(const Value: TdxAlphaColor);
begin
  FStrikeoutColor := Value;
end;

procedure TdxCharacterFormattingInfo.SetStrikeoutWordsOnly(const Value: Boolean);
begin
  SetBooleanValue(MaskStrikeoutWordsOnly, Value);
end;

procedure TdxCharacterFormattingInfo.SetUnderlineColor(const Value: TdxAlphaColor);
begin
  FUnderlineColor := Value;
end;

procedure TdxCharacterFormattingInfo.SetUnderlineWordsOnly(const Value: Boolean);
begin
  SetBooleanValue(MaskUnderlineWordsOnly, Value);
end;

function TdxCharacterFormattingInfo.Clone: TdxCharacterFormattingInfo;
begin
  Result := TdxCharacterFormattingInfo(inherited Clone);
end;

procedure TdxCharacterFormattingInfo.CopyFrom(Source: TdxCloneable);
var
  ASource: TdxCharacterFormattingInfo absolute Source;
begin
  FPackedValues := ASource.PackedValues;

  BackColor := ASource.BackColor;
  FontName := ASource.FontName;
  ForeColor := ASource.ForeColor;
  StrikeoutColor := ASource.StrikeoutColor;
  UnderlineColor := ASource.UnderlineColor;
end;

function TdxCharacterFormattingInfo.Equals(Obj: TObject): Boolean;
var
  AInfo: TdxCharacterFormattingInfo absolute Obj;
begin
  Assert(Obj is TdxCharacterFormattingInfo);
  Result :=
    (FPackedValues = AInfo.FPackedValues) and
    (FForeColor = AInfo.FForeColor) and
    (FBackColor = AInfo.FBackColor) and
    (FUnderlineColor = AInfo.FUnderlineColor) and
    (FStrikeoutColor = AInfo.FStrikeoutColor) and
    (CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
     PChar(FFontName), Length(FFontName),
     PChar(AInfo.FFontName), Length(AInfo.FFontName)) = CSTR_EQUAL);
end;

function TdxCharacterFormattingInfo.GetHashCode: Integer;
var
  ABuffer: array[0..512] of Char;
begin
  Result := Integer(FPackedValues xor (Cardinal(ForeColor) shl 8) xor (Cardinal(BackColor) shl 8) xor
    Cardinal(dxElfHash(PChar(FFontName), Length(FFontName), PChar(@ABuffer), 512)));


end;

{ TdxCharacterFormattingOptions }

constructor TdxCharacterFormattingOptions.Create(AUsedValues: TdxUsedCharacterFormattingOptions);
begin
  FValue := AUsedValues;
end;

procedure TdxCharacterFormattingOptions.CopyFrom(const Source: TdxCharacterFormattingOptions);
begin
  FValue := Source.FValue;
end;

function TdxCharacterFormattingOptions.Clone: TdxCharacterFormattingOptions;
begin
  Result := Self;
end;

class function TdxCharacterFormattingOptions.GetEmptyCharacterFormattingOption: TdxCharacterFormattingOptions;
begin
  Result := TdxCharacterFormattingOptions.Create([]);
end;

function TdxCharacterFormattingOptions.GetHashCode: Integer;
begin
  Result := Word(FValue);
end;

class function TdxCharacterFormattingOptions.GetRootCharacterFormattingOption: TdxCharacterFormattingOptions;
begin
  Result := TdxCharacterFormattingOptions.Create(TdxCharacterFormattingOptions.MaskUseAll);
end;

class operator TdxCharacterFormattingOptions.Equal(const A, B: TdxCharacterFormattingOptions): Boolean;
begin
  Result := A.FValue = B.FValue;
end;

procedure TdxCharacterFormattingOptions.ResetUse(AOptions: TdxUsedCharacterFormattingOptions);
begin
  FValue := FValue - AOptions;
end;

function TdxCharacterFormattingOptions.GetValue(const AOption: TdxUsedCharacterFormattingOption): Boolean;
begin
  Result := AOption in FValue;
end;

procedure TdxCharacterFormattingOptions.Init;
begin
  FValue := [];
end;

procedure TdxCharacterFormattingOptions.SetCoreValue(const Value: TdxUsedCharacterFormattingOptions);
begin
  FValue := Value;
end;

procedure TdxCharacterFormattingOptions.SetValue(const AOption: TdxUsedCharacterFormattingOption; const Value: Boolean);
begin
  if Value then
    Include(FValue, AOption)
  else
    Exclude(FValue, AOption);
end;

{ TdxCharacterFormattingBase }

constructor TdxCharacterFormattingBase.Create(APieceTable: TdxCustomPieceTable; const ADocumentModel: TdxCustomDocumentModel;
  const AFormattingInfo: TdxCharacterFormattingInfo; const AFormattingOptions: TdxCharacterFormattingOptions);
begin
  inherited Create(APieceTable, ADocumentModel, AFormattingInfo, AFormattingOptions);
end;

function TdxCharacterFormattingBase.SupportsFormatting: Boolean;
begin
  Result := DocumentModel.DocumentCapabilities.CharacterFormattingAllowed;
end;

procedure TdxCharacterFormattingBase.ResetUse(AResetOptions: TdxUsedCharacterFormattingOptions);
var
  AOptions: TdxCharacterFormattingOptions;
begin
  AOptions := Options;
  AOptions.ResetUse(AResetOptions);
  ReplaceInfo(Info, AOptions);
end;

function TdxCharacterFormattingBase.CloneCore: TdxCharacterFormattingBase;
begin
  Result := TdxCharacterFormattingBase.Create(PieceTable, DocumentModel, Info, Options);
end;

procedure TdxCharacterFormattingBase.CopyFrom(Source: TdxCloneable);
var
  ASource: TdxCharacterFormattingBase absolute Source;
begin
  CopyFrom(ASource.Info, ASource.Options);
end;

procedure TdxCharacterFormattingBase.CopyFrom(const AInfo: TdxCharacterFormattingInfo; const AOptions: TdxCharacterFormattingOptions);
begin
  CopyFromCore(AInfo, AOptions);
end;

function TdxCharacterFormattingBase.Clone: TdxCloneable;
begin
  if Self = nil then
    Exit(nil);
  Result := CloneCore;
end;

function TdxCharacterFormattingBase.GetNoProof: Boolean;
begin
  Result := Info.NoProof;
end;

function TdxCharacterFormattingBase.PropertyEquals(const AOther: TdxIndexBasedObject<TdxCharacterFormattingInfo, TdxCharacterFormattingOptions>): Boolean;
begin
  Result := (Options.Value = AOther.Options.Value) and Info.Equals(AOther.Info);
end;

function TdxCharacterFormattingBase.GetAllCaps: Boolean;
begin
  Result := Info.AllCaps;
end;

function TdxCharacterFormattingBase.GetBackColor: TdxAlphaColor;
begin
  Result := Info.BackColor;
end;

function TdxCharacterFormattingBase.GetFontBold: Boolean;
begin
  Result := Info.FontBold;
end;

function TdxCharacterFormattingBase.GetForeColor: TdxAlphaColor;
begin
  Result := Info.ForeColor;
end;

function TdxCharacterFormattingBase.GetFontItalic: Boolean;
begin
  Result := Info.FontItalic;
end;

function TdxCharacterFormattingBase.GetFontName: string;
begin
  Result := Info.FontName;
end;

function TdxCharacterFormattingBase.GetDoubleFontSize: Integer;
begin
  Result := Info.DoubleFontSize;
end;

function TdxCharacterFormattingBase.GetFontUnderlineType: TdxUnderlineType;
begin
  Result := Info.FontUnderlineType;
end;

function TdxCharacterFormattingBase.GetFontStrikeoutType: TdxStrikeoutType;
begin
  Result := Info.FontStrikeoutType;
end;

function TdxCharacterFormattingBase.GetHidden: Boolean;
begin
  Result := Info.Hidden;
end;

function TdxCharacterFormattingBase.GetScript: TdxCharacterFormattingScript;
begin
  Result := Info.Script;
end;

function TdxCharacterFormattingBase.GetStrikeoutColor: TdxAlphaColor;
begin
  Result := Info.StrikeoutColor;
end;

function TdxCharacterFormattingBase.GetStrikeoutWordsOnly: Boolean;
begin
  Result := Info.StrikeoutWordsOnly;
end;

function TdxCharacterFormattingBase.GetUnderlineColor: TdxAlphaColor;
begin
  Result := Info.UnderlineColor;
end;

function TdxCharacterFormattingBase.GetUnderlineWordsOnly: Boolean;
begin
  Result := Info.UnderlineWordsOnly;
end;

procedure TdxCharacterFormattingBase.SetAllCapsCore(const AInfo: TdxCharacterFormattingInfo; const AValue: Boolean);
begin
  AInfo.AllCaps := AValue;
end;

procedure TdxCharacterFormattingBase.SetAllCaps(const Value: Boolean);
begin
  if not SupportsFormatting or (Info.AllCaps = Value) and Options.UseAllCaps then
    Exit;
  SetAllCapsCore(Info, Value);
  Options.UseAllCaps := True;
end;

procedure TdxCharacterFormattingBase.SetBackColorCore(const AInfo: TdxCharacterFormattingInfo; const AValue: TdxAlphaColor);
begin
  AInfo.BackColor := AValue;
end;

procedure TdxCharacterFormattingBase.SetBackColor(const Value: TdxAlphaColor);
begin
  if not SupportsFormatting or (Info.BackColor = Value) and Options.UseBackColor then
    Exit;
  SetBackColorCore(Info, Value);
  Options.UseBackColor := True;
end;

procedure TdxCharacterFormattingBase.SetDoubleFontSizeCore(const AInfo: TdxCharacterFormattingInfo; const AValue: Integer);
begin
  AInfo.DoubleFontSize := AValue;
end;

procedure TdxCharacterFormattingBase.SetDoubleFontSize(const Value: Integer);
begin
  if not SupportsFormatting or (Info.DoubleFontSize = Value) and Options.UseDoubleFontSize then
    Exit;
  SetDoubleFontSizeCore(Info, Value);
  Options.UseDoubleFontSize := True;
end;

procedure TdxCharacterFormattingBase.SetFontBoldCore(const AInfo: TdxCharacterFormattingInfo; const AValue: Boolean);
begin
  AInfo.FontBold := AValue;
end;

procedure TdxCharacterFormattingBase.SetFontBold(const Value: Boolean);
begin
  if not SupportsFormatting or (Info.FontBold = Value) and Options.UseFontBold then
    Exit;
  SetFontBoldCore(Info, Value);
  Options.UseFontBold := True;
end;

procedure TdxCharacterFormattingBase.SetForeColorCore(const AInfo: TdxCharacterFormattingInfo; const AValue: TdxAlphaColor);
begin
  AInfo.ForeColor := AValue;
end;

procedure TdxCharacterFormattingBase.SetForeColor(const Value: TdxAlphaColor);
begin
  if not SupportsFormatting or (Info.ForeColor = Value) and Options.UseForeColor then
    Exit;
  SetForeColorCore(Info, Value);
  Options.UseForeColor := True;
end;

procedure TdxCharacterFormattingBase.SetFontItalicCore(const AInfo: TdxCharacterFormattingInfo; const AValue: Boolean);
begin
  AInfo.FontItalic := AValue;
end;

procedure TdxCharacterFormattingBase.SetFontItalic(const Value: Boolean);
begin
  if not SupportsFormatting or (Info.FontItalic = Value) and Options.UseFontItalic then
    Exit;
  SetFontItalicCore(Info, Value);
  Options.UseFontItalic := True;
end;

procedure TdxCharacterFormattingBase.SetFontNameCore(const AInfo: TdxCharacterFormattingInfo; const AValue: string);
begin
  AInfo.FontName := AValue;
end;

procedure TdxCharacterFormattingBase.SetFontName(const Value: string);
begin
  if not SupportsFormatting or Options.UseFontName and (Info.FontName = Value) then
    Exit;
  SetFontNameCore(Info, Value);
  Options.UseFontName := True;
end;

procedure TdxCharacterFormattingBase.SetFontUnderlineTypeCore(const AInfo: TdxCharacterFormattingInfo; const AValue: TdxUnderlineType);
begin
  AInfo.FontUnderlineType := AValue;
end;

procedure TdxCharacterFormattingBase.SetFontUnderlineType(const Value: TdxUnderlineType);
begin
  if not SupportsFormatting or (Info.FontUnderlineType = Value) and Options.UseFontUnderlineType then
    Exit;
  SetFontUnderlineTypeCore(Info, Value);
  Options.UseFontUnderlineType := True;
end;

procedure TdxCharacterFormattingBase.SetFontStrikeoutTypeCore(const AInfo: TdxCharacterFormattingInfo; const AValue: TdxStrikeoutType);
begin
  AInfo.FontStrikeoutType := AValue;
end;

procedure TdxCharacterFormattingBase.SetFontStrikeoutType(const Value: TdxStrikeoutType);
begin
  if not SupportsFormatting or (Info.FontStrikeoutType = Value) and Options.UseFontStrikeoutType then
    Exit;
  SetFontStrikeoutTypeCore(Info, Value);
  Options.UseFontStrikeoutType := True;
end;

procedure TdxCharacterFormattingBase.SetHiddenCore(const AInfo: TdxCharacterFormattingInfo; const AValue: Boolean);
begin
  AInfo.Hidden := AValue;
end;

procedure TdxCharacterFormattingBase.SetHidden(const Value: Boolean);
begin
  if not SupportsFormatting or (Info.Hidden = Value) and Options.UseHidden then
    Exit;
  SetHiddenCore(Info, Value);
  Options.UseHidden := True;
end;


procedure TdxCharacterFormattingBase.SetNoProofCore(const AInfo: TdxCharacterFormattingInfo; const Value: Boolean);
begin
  AInfo.NoProof := Value;
end;

procedure TdxCharacterFormattingBase.SetNoProof(const Value: Boolean);
begin
  if not SupportsFormatting or (Info.NoProof = Value) and Options.UseNoProof then
    Exit;
  SetNoProofCore(Info, Value);
  Options.UseNoProof := True;
end;

procedure TdxCharacterFormattingBase.SetScriptCore(const AInfo: TdxCharacterFormattingInfo; const AValue: TdxCharacterFormattingScript);
begin
  AInfo.Script := AValue;
end;

procedure TdxCharacterFormattingBase.SetScript(const Value: TdxCharacterFormattingScript);
begin
  if not SupportsFormatting or (Info.Script = Value) and Options.UseScript then
    Exit;
  SetScriptCore(Info, Value);
  Options.UseScript := True;
end;

procedure TdxCharacterFormattingBase.SetStrikeoutColorCore(const AInfo: TdxCharacterFormattingInfo; const AValue: TdxAlphaColor);
begin
  AInfo.StrikeoutColor := AValue;
end;

procedure TdxCharacterFormattingBase.SetStrikeoutColor(const Value: TdxAlphaColor);
begin
  if not SupportsFormatting or (Info.StrikeoutColor = Value) and Options.UseStrikeoutColor then
    Exit;
  SetStrikeoutColorCore(Info, Value);
  Options.UseStrikeoutColor := True;
end;

procedure TdxCharacterFormattingBase.SetStrikeoutWordsOnlyCore(const AInfo: TdxCharacterFormattingInfo; const AValue: Boolean);
begin
  AInfo.StrikeoutWordsOnly := AValue;
end;

procedure TdxCharacterFormattingBase.SetStrikeoutWordsOnly(const Value: Boolean);
begin
  if not SupportsFormatting or (Info.StrikeoutWordsOnly = Value) and Options.UseStrikeoutWordsOnly then
    Exit;
  SetStrikeoutWordsOnlyCore(Info, Value);
  Options.UseStrikeoutWordsOnly := True;
end;

procedure TdxCharacterFormattingBase.SetUnderlineColorCore(const AInfo: TdxCharacterFormattingInfo; const AValue: TdxAlphaColor);
begin
  AInfo.UnderlineColor := AValue;
end;

procedure TdxCharacterFormattingBase.SetUnderlineColor(const Value: TdxAlphaColor);
begin
  if not SupportsFormatting or (Info.UnderlineColor = Value) and Options.UseUnderlineColor then
    Exit;
  SetUnderlineColorCore(Info, Value);
  Options.UseUnderlineColor := True;
end;

procedure TdxCharacterFormattingBase.SetUnderlineWordsOnlyCore(const AInfo: TdxCharacterFormattingInfo; const AValue: Boolean);
begin
  AInfo.UnderlineWordsOnly := AValue;
end;

procedure TdxCharacterFormattingBase.SetUnderlineWordsOnly(const Value: Boolean);
begin
  if not SupportsFormatting or (Info.UnderlineWordsOnly = Value) and Options.UseUnderlineWordsOnly then
    Exit;
  SetUnderlineWordsOnlyCore(Info, Value);
  Options.UseUnderlineWordsOnly := True;
end;

{ TdxCharacterProperties }

constructor TdxCharacterProperties.Create(const AOwner: IdxCharacterPropertiesContainer);
begin
  Assert(AOwner <> nil);
  inherited Create(AOwner.PieceTable);
  FOwner := AOwner;
end;

class procedure TdxCharacterProperties.ApplyPropertiesDiff(ATarget: TdxCharacterProperties; ATargetMergedInfo,
  ASourceMergedInfo: TdxCharacterFormattingInfo);
begin
  if ATargetMergedInfo.AllCaps <> ASourceMergedInfo.AllCaps then
    ATarget.AllCaps := ASourceMergedInfo.AllCaps;

  if ATargetMergedInfo.BackColor <> ASourceMergedInfo.BackColor then
    ATarget.BackColor := ASourceMergedInfo.BackColor;

  if ATargetMergedInfo.FontBold <> ASourceMergedInfo.FontBold then
    ATarget.FontBold := ASourceMergedInfo.FontBold;

  if ATargetMergedInfo.FontItalic <> ASourceMergedInfo.FontItalic then
    ATarget.FontItalic := ASourceMergedInfo.FontItalic;

  if ATargetMergedInfo.FontName <> ASourceMergedInfo.FontName then
    ATarget.FontName := ASourceMergedInfo.FontName;

  if ATargetMergedInfo.DoubleFontSize <> ASourceMergedInfo.DoubleFontSize then
    ATarget.DoubleFontSize := ASourceMergedInfo.DoubleFontSize;

  if ATargetMergedInfo.FontStrikeoutType <> ASourceMergedInfo.FontStrikeoutType then
    ATarget.FontStrikeoutType := ASourceMergedInfo.FontStrikeoutType;

  if ATargetMergedInfo.FontUnderlineType <> ASourceMergedInfo.FontUnderlineType then
    ATarget.FontUnderlineType := ASourceMergedInfo.FontUnderlineType;

  if ATargetMergedInfo.ForeColor <> ASourceMergedInfo.ForeColor then
    ATarget.ForeColor := ASourceMergedInfo.ForeColor;

  if ATargetMergedInfo.Hidden <> ASourceMergedInfo.Hidden then
    ATarget.Hidden := ASourceMergedInfo.Hidden;

  if ATargetMergedInfo.Script <> ASourceMergedInfo.Script then
    ATarget.Script := ASourceMergedInfo.Script;

  if ATargetMergedInfo.StrikeoutColor <> ASourceMergedInfo.StrikeoutColor then
    ATarget.StrikeoutColor := ASourceMergedInfo.StrikeoutColor;

  if ATargetMergedInfo.StrikeoutWordsOnly <> ASourceMergedInfo.StrikeoutWordsOnly then
    ATarget.StrikeoutWordsOnly := ASourceMergedInfo.StrikeoutWordsOnly;

  if ATargetMergedInfo.UnderlineColor <> ASourceMergedInfo.UnderlineColor then
    ATarget.UnderlineColor := ASourceMergedInfo.UnderlineColor;

  if ATargetMergedInfo.UnderlineWordsOnly <> ASourceMergedInfo.UnderlineWordsOnly then
    ATarget.UnderlineWordsOnly := ASourceMergedInfo.UnderlineWordsOnly;

  if ATargetMergedInfo.NoProof <> ASourceMergedInfo.NoProof then
    ATarget.NoProof := ASourceMergedInfo.NoProof;
end;

procedure TdxCharacterProperties.CopyFrom(const AProperties: TdxMergedCharacterProperties);
var
  AInfo: TdxCharacterFormattingBase;
  AIsDeferredInfo: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferredInfo);
  AInfo.CopyFrom(AProperties.Info, AProperties.Options);
  ReplaceInfo(AInfo, BatchUpdateChangeActions);
  if not AIsDeferredInfo then AInfo.Free;
end;

{$IFNDEF DELPHI17}
procedure TdxCharacterProperties.CopyFrom(
  const Source: TdxUndoableIndexBasedObject<TdxCharacterFormattingBase>);
begin
  inherited CopyFrom(Source);
end;

procedure TdxCharacterProperties.CopyFrom(const Source: TdxCharacterFormattingBase);
begin
  inherited CopyFrom(Source);
end;
{$ENDIF}

procedure TdxCharacterProperties.Merge(const AProperties: TdxCharacterProperties);
var
  AMergedProperties: TdxMergedCharacterProperties;
begin
  AMergedProperties := TdxMergedCharacterProperties.Create(Self);
  try
    AMergedProperties.Merge(AProperties);
    CopyFrom(AMergedProperties);
  finally
    AMergedProperties.Free;
  end;
end;

procedure TdxCharacterProperties.Reset;
var
  AInfo: TdxCharacterFormattingBase;
  AEmptyInfo: TdxCharacterFormattingBase;
  AIsDeferredInfo: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferredInfo);
  AEmptyInfo := GetCache(DocumentModel)[TdxCharacterFormattingCache.EmptyCharacterFormattingIndex];
  AInfo.ReplaceInfo(AEmptyInfo.Info, AEmptyInfo.Options);
  ReplaceInfo(AInfo, BatchUpdateChangeActions);
  if not AIsDeferredInfo then AInfo.Free;
end;

procedure TdxCharacterProperties.ResetAllUse;
var
  AInfo: TdxCharacterFormattingBase;
  AFormattingInfo: TdxCharacterFormattingInfo;
  AIsDeferredInfo: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferredInfo);
  AFormattingInfo := AInfo.Info;
  AInfo.ReplaceInfo(AFormattingInfo, TdxCharacterFormattingOptions.EmptyCharacterFormattingOption);
  ReplaceInfo(AInfo, BatchUpdateChangeActions);
  if not AIsDeferredInfo then AInfo.Free;
end;

procedure TdxCharacterProperties.ResetUse(const AResetOption: TdxUsedCharacterFormattingOption);
begin
  ResetUse([AResetOption]);
end;

procedure TdxCharacterProperties.ResetUse(const AResetOptions: TdxUsedCharacterFormattingOptions);
var
  AInfo: TdxCharacterFormattingBase;
  AOptions: TdxCharacterFormattingOptions;
  AFormattingInfo: TdxCharacterFormattingInfo;
  AIsDeferredInfo: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferredInfo);

  AOptions := AInfo.Options;
  AOptions.ResetUse(AResetOptions);

  AFormattingInfo := AInfo.Info;
  AInfo.ReplaceInfo(AFormattingInfo, AOptions);

  ReplaceInfo(AInfo, BatchUpdateChangeActions);
  if not AIsDeferredInfo then AInfo.Free;
end;

function TdxCharacterProperties.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(TdxCharacterFormattingChangeType.BatchUpdate);
end;

function TdxCharacterProperties.GetObtainAffectedRangeListener: IdxObtainAffectedRangeListener;
begin
  if not Supports(FOwner, IdxObtainAffectedRangeListener, Result) then
    Result := nil;
end;

procedure TdxCharacterProperties.CopyFrom(const ACharacterProperties: TdxMergedProperties<TdxCharacterFormattingInfo, TdxCharacterFormattingOptions>);
var
  AInfo: TdxCharacterFormattingBase;
  AIsDeferredInfo: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferredInfo);
  AInfo.CopyFromCore(ACharacterProperties.Info, ACharacterProperties.Options);
  if not AIsDeferredInfo then AInfo.Free;
end;

function TdxCharacterProperties.CreateIndexChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := FOwner.CreateCharacterPropertiesChangedHistoryItem;
end;

function TdxCharacterProperties.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxCharacterFormattingBase>;
begin
  Result := TdxSimpleDocumentCache(ADocumentModel.Cache).CharacterFormattingCache;
end;

procedure TdxCharacterProperties.OnIndexChanged;
begin
  Owner.OnCharacterPropertiesChanged;
end;

function TdxCharacterProperties.GetUseFontName: Boolean;
begin
  Result := Info.Options.UseFontName;
end;

function TdxCharacterProperties.GetUseDoubleFontSize: Boolean;
begin
  Result := Info.Options.UseDoubleFontSize;
end;

function TdxCharacterProperties.GetUseFontBold: Boolean;
begin
  Result := Info.Options.UseFontBold;
end;

function TdxCharacterProperties.GetUseFontItalic: Boolean;
begin
  Result := Info.Options.UseFontItalic;
end;

function TdxCharacterProperties.GetUseFontStrikeoutType: Boolean;
begin
  Result := Info.Options.UseFontStrikeoutType;
end;

function TdxCharacterProperties.GetUseFontUnderlineType: Boolean;
begin
  Result := Info.Options.UseFontUnderlineType;
end;

function TdxCharacterProperties.GetUseAllCaps: Boolean;
begin
  Result := Info.Options.UseAllCaps;
end;

function TdxCharacterProperties.GetUseForeColor: Boolean;
begin
  Result := Info.Options.UseForeColor;
end;

function TdxCharacterProperties.GetUseBackColor: Boolean;
begin
  Result := Info.Options.UseBackColor;
end;

function TdxCharacterProperties.GetUseUnderlineColor: Boolean;
begin
  Result := Info.Options.UseUnderlineColor;
end;

function TdxCharacterProperties.GetUseStrikeoutColor: Boolean;
begin
  Result := Info.Options.UseStrikeoutColor;
end;

function TdxCharacterProperties.GetUseUnderlineWordsOnly: Boolean;
begin
  Result := Info.Options.UseUnderlineWordsOnly;
end;

function TdxCharacterProperties.GetUseStrikeoutWordsOnly: Boolean;
begin
  Result := Info.Options.UseStrikeoutWordsOnly;
end;

function TdxCharacterProperties.GetUseScript: Boolean;
begin
  Result := Info.Options.UseScript;
end;

function TdxCharacterProperties.GetUseHidden: Boolean;
begin
  Result := Info.Options.UseHidden;
end;

function TdxCharacterProperties.GetUseNoProof: Boolean;
begin
  Result := Info.Options.UseNoProof;
end;

function TdxCharacterProperties.GetAllCaps: Boolean;
begin
  Result := Info.AllCaps;
end;

function TdxCharacterProperties.GetBackColor: TdxAlphaColor;
begin
  Result := Info.BackColor;
end;

function TdxCharacterProperties.GetFontBold: Boolean;
begin
  Result := Info.FontBold;
end;

function TdxCharacterProperties.GetFontItalic: Boolean;
begin
  Result := Info.FontItalic;
end;

function TdxCharacterProperties.GetFontName: string;
begin
  Result := Info.FontName;
end;

function TdxCharacterProperties.GetDoubleFontSize: Integer;
begin
  Result := Info.DoubleFontSize;
end;

function TdxCharacterProperties.GetFontStrikeoutType: TdxStrikeoutType;
begin
  Result := Info.FontStrikeoutType;
end;

function TdxCharacterProperties.GetFontUnderlineType: TdxUnderlineType;
begin
  Result := Info.FontUnderlineType;
end;

function TdxCharacterProperties.GetForeColor: TdxAlphaColor;
begin
  Result := Info.ForeColor;
end;

function TdxCharacterProperties.GetHidden: Boolean;
begin
  Result := Info.Hidden;
end;

function TdxCharacterProperties.GetNoProof: Boolean;
begin
  Result := Info.NoProof;
end;

function TdxCharacterProperties.GetScript: TdxCharacterFormattingScript;
begin
  Result := Info.Script;
end;

function TdxCharacterProperties.GetStrikeoutColor: TdxAlphaColor;
begin
  Result := Info.StrikeoutColor;
end;

function TdxCharacterProperties.GetStrikeoutWordsOnly: Boolean;
begin
  Result := Info.StrikeoutWordsOnly;
end;

function TdxCharacterProperties.GetUnderlineColor: TdxAlphaColor;
begin
  Result := Info.UnderlineColor;
end;

function TdxCharacterProperties.GetUnderlineWordsOnly: Boolean;
begin
  Result := Info.UnderlineWordsOnly;
end;

procedure TdxCharacterProperties.SetAllCaps(const Value: Boolean);
begin
  SetPropertyValue<Boolean>(SetAllCapsCore, Value);
end;

function TdxCharacterProperties.SetAllCapsCore(const AInfo: TdxCharacterFormattingBase; const Value: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.AllCaps := Value;
  Result := TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(TdxCharacterFormattingChangeType.AllCaps);
end;

function TdxCharacterProperties.SetBackColorCore(const AInfo: TdxCharacterFormattingBase; const Value: TdxAlphaColor): TdxDocumentModelChangeActions;
begin
  AInfo.BackColor := Value;
  Result := TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(TdxCharacterFormattingChangeType.BackColor);
end;

procedure TdxCharacterProperties.SetBackColor(const Value: TdxAlphaColor);
begin
  SetPropertyValue<TdxAlphaColor>(SetBackColorCore, Value);
end;

procedure TdxCharacterProperties.SetFontBold(const Value: Boolean);
begin
  SetPropertyValue<Boolean>(SetFontBoldCore, Value);
end;

function TdxCharacterProperties.SetFontBoldCore(const AInfo: TdxCharacterFormattingBase;
  const Value: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.FontBold := Value;
  Result := TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(TdxCharacterFormattingChangeType.FontBold);
end;

procedure TdxCharacterProperties.SetFontItalic(const Value: Boolean);
begin
  SetPropertyValue<Boolean>(SetFontItalicCore, Value);
end;

function TdxCharacterProperties.SetFontItalicCore(const AInfo: TdxCharacterFormattingBase;
  const Value: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.FontItalic := Value;
  Result := TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(TdxCharacterFormattingChangeType.FontItalic);
end;

procedure TdxCharacterProperties.SetFontName(const Value: string);
begin
  SetPropertyValue<string>(SetFontNameCore, Value);
end;

function TdxCharacterProperties.SetFontNameCore(const AInfo: TdxCharacterFormattingBase;
  const Value: string): TdxDocumentModelChangeActions;
begin
  AInfo.FontName := Value;
  Result := TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(TdxCharacterFormattingChangeType.FontName);
end;

procedure TdxCharacterProperties.SetDoubleFontSize(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowInternalException;
  SetPropertyValue<Integer>(SetDoubleFontSizeCore, Value);
end;

function TdxCharacterProperties.SetDoubleFontSizeCore(const AInfo: TdxCharacterFormattingBase;
  const Value: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.DoubleFontSize := Value;
  Result := TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(TdxCharacterFormattingChangeType.FontSize);
end;

procedure TdxCharacterProperties.SetFontStrikeoutType(const Value: TdxStrikeoutType);
begin
  SetPropertyValue<Integer>(SetFontStrikeoutTypeCore, Ord(Value));
end;

function TdxCharacterProperties.SetFontStrikeoutTypeCore(const AInfo: TdxCharacterFormattingBase;
  const Value: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.FontStrikeoutType := TdxStrikeoutType(Value);
  Result := TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(TdxCharacterFormattingChangeType.FontStrikeoutType);
end;

procedure TdxCharacterProperties.SetFontUnderlineType(const Value: TdxUnderlineType);
begin
  SetPropertyValue<Integer>(SetFontUnderlineTypeCore, Ord(Value));
end;

function TdxCharacterProperties.SetFontUnderlineTypeCore(const AInfo: TdxCharacterFormattingBase;
  const Value: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.FontUnderlineType := TdxUnderlineType(Value);
  Result := TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(TdxCharacterFormattingChangeType.FontUnderlineType);
end;

function TdxCharacterProperties.SetForeColorCore(const AInfo: TdxCharacterFormattingBase; const Value: TdxAlphaColor): TdxDocumentModelChangeActions;
begin
  AInfo.ForeColor := Value;
  Result := TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(TdxCharacterFormattingChangeType.ForeColor);
end;

procedure TdxCharacterProperties.SetForeColor(const Value: TdxAlphaColor);
begin
  if not IsUpdateLocked and Info.Options.UseForeColor and (Info.Info.ForeColor = Value) then
    Exit;
  SetPropertyValue<TdxAlphaColor>(SetForeColorCore, Value);
end;

procedure TdxCharacterProperties.SetHidden(const Value: Boolean);
begin
  SetPropertyValue<Boolean>(SetHiddenCore, Value);
end;

function TdxCharacterProperties.SetHiddenCore(const AInfo: TdxCharacterFormattingBase;
  const Value: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.Hidden := Value;
  Result := TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(TdxCharacterFormattingChangeType.Hidden);
end;

procedure TdxCharacterProperties.SetNoProof(const Value: Boolean);
begin
  SetPropertyValue<Boolean>(SetNoProofCore, Value);
end;

function TdxCharacterProperties.SetNoProofCore(const AInfo: TdxCharacterFormattingBase;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.NoProof := AValue;
  Result := TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(TdxCharacterFormattingChangeType.NoProof);
end;

procedure TdxCharacterProperties.SetScript(const Value: TdxCharacterFormattingScript);
begin
  SetPropertyValue<Integer>(SetScriptCore, Ord(Value));
end;

function TdxCharacterProperties.SetScriptCore(const AInfo: TdxCharacterFormattingBase;
  const Value: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.Script := TdxCharacterFormattingScript(Value);
  Result := TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(TdxCharacterFormattingChangeType.Script);
end;

function TdxCharacterProperties.SetStrikeoutColorCore(const AInfo: TdxCharacterFormattingBase;
  const Value: TdxAlphaColor): TdxDocumentModelChangeActions;
begin
  AInfo.StrikeoutColor := Value;
  Result := TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(TdxCharacterFormattingChangeType.StrikeoutColor);
end;

procedure TdxCharacterProperties.SetStrikeoutColor(const Value: TdxAlphaColor);
begin
  SetPropertyValue<TdxAlphaColor>(SetStrikeoutColorCore, Value);
end;

procedure TdxCharacterProperties.SetStrikeoutWordsOnly(const Value: Boolean);
begin
  SetPropertyValue<Boolean>(SetStrikeoutWordsOnlyCore, Value);
end;

function TdxCharacterProperties.SetStrikeoutWordsOnlyCore(const AInfo: TdxCharacterFormattingBase;
  const Value: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.StrikeoutWordsOnly := Value;
  Result := TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(TdxCharacterFormattingChangeType.StrikeoutWordsOnly);
end;

function TdxCharacterProperties.SetUnderlineColorCore(const AInfo: TdxCharacterFormattingBase;
  const Value: TdxAlphaColor): TdxDocumentModelChangeActions;
begin
  AInfo.UnderlineColor := Value;
  Result := TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(TdxCharacterFormattingChangeType.UnderlineColor);
end;

procedure TdxCharacterProperties.SetUnderlineColor(const Value: TdxAlphaColor);
begin
  SetPropertyValue<TdxAlphaColor>(SetUnderlineColorCore, Value);
end;

procedure TdxCharacterProperties.SetUnderlineWordsOnly(const Value: Boolean);
begin
  SetPropertyValue<Boolean>(SetUnderlineWordsOnlyCore, Value);
end;

function TdxCharacterProperties.SetUnderlineWordsOnlyCore(const AInfo: TdxCharacterFormattingBase;
  const Value: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.UnderlineWordsOnly := Value;
  Result := TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(TdxCharacterFormattingChangeType.UnderlineWordsOnly);
end;

function TdxCharacterProperties.UseVal(AMask: TdxUsedCharacterFormattingOption): Boolean;
begin
  Result := AMask in Info.Options.Value;
end;

{ TdxMergedCharacterProperties }

constructor TdxMergedCharacterProperties.Create(const AProperties: TdxMergedCharacterProperties);
begin
  Create(AProperties.Info, AProperties.Options);
end;

constructor TdxMergedCharacterProperties.Create(const AProperties: TdxCharacterProperties);
var
  AInfo: TdxCharacterFormattingBase;
begin
  AInfo := AProperties.Info;
  Create(AInfo.Info, AInfo.Options);
end;

procedure TdxMergedCharacterProperties.Merge(const AProperties: TdxCharacterProperties);
var
  AInfo: TdxCharacterFormattingBase;
begin
  AInfo := AProperties.Info;
  MergeCore(AInfo.Info, AInfo.Options);
end;

procedure TdxMergedCharacterProperties.Merge(const AProperties: TdxMergedCharacterProperties);
begin
  MergeCore(AProperties.Info, AProperties.Options);
end;

procedure TdxMergedCharacterProperties.MergeCore(const AInfo: TdxCharacterFormattingInfo;
  const AOptions: TdxCharacterFormattingOptions);
begin
  if not Options.UseAllCaps and AOptions.UseAllCaps then
  begin
    Info.AllCaps := AInfo.AllCaps;
    Options.UseAllCaps := True;
  end;
  if not Options.UseBackColor and AOptions.UseBackColor then
  begin
    Info.BackColor := AInfo.BackColor;
    Options.UseBackColor := True;
  end;
  if not Options.UseFontBold and AOptions.UseFontBold then
  begin
    Info.FontBold := AInfo.FontBold;
    Options.UseFontBold := True;
  end;
  if not Options.UseFontItalic and AOptions.UseFontItalic then
  begin
    Info.FontItalic := AInfo.FontItalic;
    Options.UseFontItalic := True;
  end;
  if not Options.UseFontName and AOptions.UseFontName then
  begin
    Info.FontName := AInfo.FontName;
    Options.UseFontName := True;
  end;
  if not Options.UseDoubleFontSize and AOptions.UseDoubleFontSize then
  begin
    Info.DoubleFontSize := AInfo.DoubleFontSize;
    Options.UseDoubleFontSize := True;
  end;
  if not Options.UseFontStrikeoutType and AOptions.UseFontStrikeoutType then
  begin
    Info.FontStrikeoutType := AInfo.FontStrikeoutType;
    Options.UseFontStrikeoutType := True;
  end;
  if not Options.UseFontUnderlineType and AOptions.UseFontUnderlineType then
  begin
    Info.FontUnderlineType := AInfo.FontUnderlineType;
    Options.UseFontUnderlineType := True;
  end;
  if not Options.UseForeColor and AOptions.UseForeColor then
  begin
    Info.ForeColor := AInfo.ForeColor;
    Options.UseForeColor := True;
  end;
  if not Options.UseScript and AOptions.UseScript then
  begin
    Info.Script := AInfo.Script;
    Options.UseScript := True;
  end;
  if not Options.UseStrikeoutColor and AOptions.UseStrikeoutColor then
  begin
    Info.StrikeoutColor := AInfo.StrikeoutColor;
    Options.UseStrikeoutColor := True;
  end;
  if not Options.UseStrikeoutWordsOnly and AOptions.UseStrikeoutWordsOnly then
  begin
    Info.StrikeoutWordsOnly := AInfo.StrikeoutWordsOnly;
    Options.UseStrikeoutWordsOnly := True;
  end;
  if not Options.UseUnderlineColor and AOptions.UseUnderlineColor then
  begin
    Info.UnderlineColor := AInfo.UnderlineColor;
    Options.UseUnderlineColor := True;
  end;
  if not Options.UseUnderlineWordsOnly and AOptions.UseUnderlineWordsOnly then
  begin
    Info.UnderlineWordsOnly := AInfo.UnderlineWordsOnly;
    Options.UseUnderlineWordsOnly := True;
  end;
  if not Options.UseHidden and AOptions.UseHidden then
  begin
    Info.Hidden := AInfo.Hidden;
    Options.UseHidden := True;
  end;
  if not Options.UseNoProof and AOptions.UseNoProof then
  begin
    Info.NoProof := AInfo.NoProof;
    Options.UseNoProof := True;
  end;
end;

{ TdxCharacterFormattingInfoCache }

function TdxCharacterFormattingInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxCharacterFormattingInfo;
begin
  Result := TdxCharacterFormattingInfo.CreateDefaultItem;
end;

{ TdxCharacterFormattingCache }

constructor TdxCharacterFormattingCache.Create(const ADocumentModel: TdxCustomDocumentModel);
var
  ADefaultCharacterFormattingInfo: TdxCharacterFormattingInfo;
begin
  inherited Create(ADocumentModel.UnitConverter, ADocumentModel);
  ADefaultCharacterFormattingInfo := TdxSimpleDocumentCache(ADocumentModel.Cache).CharacterFormattingInfoCache.DefaultItem;
  AppendItem(TdxDefaultCharacterFormattingBase.Create(DocumentModel.MainPart, DocumentModel,
    ADefaultCharacterFormattingInfo,
    TdxCharacterFormattingOptions.EmptyCharacterFormattingOption));
  AppendItem(TdxDefaultCharacterFormattingBase.Create(DocumentModel.MainPart, DocumentModel,
    ADefaultCharacterFormattingInfo,
    TdxCharacterFormattingOptions.RootCharacterFormattingOption));
end;

function TdxCharacterFormattingCache.CreateDefaultItem(
  const AUnitConverter: IdxDocumentModelUnitConverter): TdxCharacterFormattingBase;
begin
  Result := nil;
end;

{ TdxCharacterFormattingChangeActionsCalculator }

class function TdxCharacterFormattingChangeActionsCalculator.CalculateChangeActions(
  const AChange: TdxCharacterFormattingChangeType): TdxDocumentModelChangeActions;
const
  CharacterFormattingChangeActionsMap: array[TdxCharacterFormattingChangeType] of TdxDocumentModelChangeActions =
    (
      [],
      [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetPrimaryLayout,
       TdxDocumentModelChangeAction.ResetSecondaryLayout, TdxDocumentModelChangeAction.ResetSelectionLayout,
       TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting, TdxDocumentModelChangeAction.SplitRunByCharset],
      [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetPrimaryLayout,
       TdxDocumentModelChangeAction.ResetSecondaryLayout, TdxDocumentModelChangeAction.ResetSelectionLayout,
       TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting],
      [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetPrimaryLayout,
       TdxDocumentModelChangeAction.ResetSecondaryLayout, TdxDocumentModelChangeAction.ResetSelectionLayout,
       TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting],
      [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetPrimaryLayout,
       TdxDocumentModelChangeAction.ResetSecondaryLayout, TdxDocumentModelChangeAction.ResetSelectionLayout,
       TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting],
      [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetPrimaryLayout,
       TdxDocumentModelChangeAction.ResetSecondaryLayout, TdxDocumentModelChangeAction.ResetSelectionLayout,
       TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting],
      [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetPrimaryLayout,
       TdxDocumentModelChangeAction.ResetSecondaryLayout, TdxDocumentModelChangeAction.ResetSelectionLayout,
       TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting],
      [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetPrimaryLayout,
       TdxDocumentModelChangeAction.ResetSecondaryLayout, TdxDocumentModelChangeAction.ResetSelectionLayout,
       TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting],
      [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting],
      [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting,
       TdxDocumentModelChangeAction.ResetSecondaryLayout],
      [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting],
      [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting],
      [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetSecondaryLayout,
       TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting],
      [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetSecondaryLayout,
       TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting],
      [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetPrimaryLayout,
       TdxDocumentModelChangeAction.ResetSecondaryLayout, TdxDocumentModelChangeAction.ResetSelectionLayout,
       TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting],
      [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetPrimaryLayout,
       TdxDocumentModelChangeAction.ResetSecondaryLayout, TdxDocumentModelChangeAction.ResetSelectionLayout,
       TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting, TdxDocumentModelChangeAction.SplitRunByCharset],
      [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetPrimaryLayout,
       TdxDocumentModelChangeAction.ResetSecondaryLayout, TdxDocumentModelChangeAction.ResetSelectionLayout,
       TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting, TdxDocumentModelChangeAction.ValidateSelectionInterval],
       [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetPrimaryLayout,
       TdxDocumentModelChangeAction.ResetSecondaryLayout, TdxDocumentModelChangeAction.ResetSelectionLayout,
       TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting, TdxDocumentModelChangeAction.ResetUncheckedIntervals,
       TdxDocumentModelChangeAction.ValidateSelectionInterval],
      [TdxDocumentModelChangeAction.Redraw, TdxDocumentModelChangeAction.ResetPrimaryLayout,
       TdxDocumentModelChangeAction.ResetSecondaryLayout, TdxDocumentModelChangeAction.ResetSelectionLayout,
       TdxDocumentModelChangeAction.ResetCaretInputPositionFormatting, TdxDocumentModelChangeAction.ValidateSelectionInterval]
    );
begin
  Result := CharacterFormattingChangeActionsMap[AChange];
end;

class procedure TdxCharacterPropertiesFontAssignmentHelper.AssignFont(const ACharacterProperties: IdxCharacterProperties; AFont: TFont);
begin
  ACharacterProperties.FontName := AFont.Name;
  ACharacterProperties.DoubleFontSize := Round(AFont.Size * 2);
  ACharacterProperties.FontBold := fsBold in AFont.Style;
  ACharacterProperties.FontItalic := fsItalic in AFont.Style;
  if fsUnderline in AFont.Style then
    ACharacterProperties.FontUnderlineType := TdxUnderlineType.Single
  else
    ACharacterProperties.FontUnderlineType := TdxUnderlineType.None;
  if fsStrikeOut in AFont.Style then
    ACharacterProperties.FontStrikeoutType := TdxStrikeoutType.Single
  else
    ACharacterProperties.FontStrikeoutType := TdxStrikeoutType.None;
end;

{ TdxParagraphMergedCharacterPropertiesCachedResult }

constructor TdxParagraphMergedCharacterPropertiesCachedResult.Create;
begin
  inherited Create;
  FParagraphStyleIndex := -1;
end;

destructor TdxParagraphMergedCharacterPropertiesCachedResult.Destroy;
begin
  FreeAndNil(FMergedCharacterProperties);
  inherited Destroy;
end;

procedure TdxParagraphMergedCharacterPropertiesCachedResult.SetMergedCharacterProperties(
  const Value: TdxMergedCharacterProperties);
begin
  if FMergedCharacterProperties <> Value then
  begin
    FMergedCharacterProperties.Free;
    FMergedCharacterProperties := Value;
  end;
end;

{ TdxRunMergedCharacterPropertiesCachedResult }

constructor TdxRunMergedCharacterPropertiesCachedResult.Create;
begin
  inherited Create;
  FCharacterStyleIndex := -1;
  FCharacterPropertiesIndex := -1;
end;

end.
