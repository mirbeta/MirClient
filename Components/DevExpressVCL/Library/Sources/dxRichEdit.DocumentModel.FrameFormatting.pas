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

unit dxRichEdit.DocumentModel.FrameFormatting;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, Classes, SysUtils, Graphics,
  dxCore, dxCoreClasses, dxCoreGraphics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.MergedProperties;

type
  TdxParagraphFrameHorizontalRule = (Auto, AtLeast, Exact);

  TdxParagraphFrameTextWrapType = (
    Auto,
    Around,
    None,
    NotBeside,
    Through,
    Tight);

  TdxParagraphFrameHorizontalPositionType = (
    Page,
    Column,
    Margin);

  TdxParagraphFrameHorizontalPositionAlignment = (
    None,
    Left,
    Center,
    Right,
    Inside,
    Outside);

  TdxParagraphFrameVerticalPositionType = (
    Page,
    Paragraph,
    Margin);

  TdxParagraphFrameVerticalPositionAlignment = (
    None,
    Top,
    Center,
    Bottom,
    &Inline,
    Inside,
    Outside);

  TdxDropCapLocation = (None, Drop, Margin);

  IdxParagraphFrameLocation = interface
  ['{E59C88D2-EAA4-4E9E-834D-31F5D282B42D}']
    function GetX: Integer;
    function GetY: Integer;
    function GetHorizontalPositionAlignment: TdxParagraphFrameHorizontalPositionAlignment;
    function GetVerticalPositionAlignment: TdxParagraphFrameVerticalPositionAlignment;
    function GetHorizontalPositionType: TdxParagraphFrameHorizontalPositionType;
    function GetVerticalPositionType: TdxParagraphFrameVerticalPositionType;
    function GetWidth: Integer;
    function GetHeight: Integer;

    property X: Integer read GetX;
    property Y: Integer read GetY;
    property HorizontalPositionAlignment: TdxParagraphFrameHorizontalPositionAlignment read GetHorizontalPositionAlignment;
    property VerticalPositionAlignment: TdxParagraphFrameVerticalPositionAlignment read GetVerticalPositionAlignment;
    property HorizontalPositionType: TdxParagraphFrameHorizontalPositionType read GetHorizontalPositionType;
    property VerticalPositionType: TdxParagraphFrameVerticalPositionType read GetVerticalPositionType;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

  { TdxParagraphFrameFormattingInfo }

  TdxParagraphFrameFormattingInfo = class(TdxCloneable)
  private
    const
      MaskHorizontalRule = $00000003;
      MaskWrapType = $0000001C;
      MaskHorizontalPositionType = $00000060;
      MaskVerticalPositionType = $00000180;
      MaskHorizontalPositionAlignment = $00000E00;
      MaskVerticalPositionAlignment = $0000F000;
      MaskDropCap = $00030000;
      MaskLockFrameAnchorToParagraph = $00040000;
  strict private
    FPackedValues: Integer;
    FVerticalPosition: Integer;
    FHorizontalPosition: Integer;
    FHorizontalPadding: Integer;
    FVerticalPadding: Integer;
    FHeight: Integer;
    FWidth: Integer;
    FX: Integer;
    FY: Integer;
    FDropCapVerticalHeightInLines: Integer;
    function GetHorizontalRule: TdxParagraphFrameHorizontalRule;
    procedure SetHorizontalRule(const AValue: TdxParagraphFrameHorizontalRule);
    function GetTextWrapType: TdxParagraphFrameTextWrapType;
    procedure SetTextWrapType(const AValue: TdxParagraphFrameTextWrapType);
    function GetHorizontalPositionAlignment: TdxParagraphFrameHorizontalPositionAlignment;
    procedure SetHorizontalPositionAlignment(const AValue: TdxParagraphFrameHorizontalPositionAlignment);
    function GetVerticalPositionAlignment: TdxParagraphFrameVerticalPositionAlignment;
    procedure SetVerticalPositionAlignment(const AValue: TdxParagraphFrameVerticalPositionAlignment);
    function GetHorizontalPositionType: TdxParagraphFrameHorizontalPositionType;
    procedure SetHorizontalPositionType(const AValue: TdxParagraphFrameHorizontalPositionType);
    function GetVerticalPositionType: TdxParagraphFrameVerticalPositionType;
    procedure SetVerticalPositionType(const AValue: TdxParagraphFrameVerticalPositionType);
    function GetDropCap: TdxDropCapLocation;
    procedure SetDropCap(const AValue: TdxDropCapLocation);
    function GetLockFrameAnchorToParagraph: Boolean;
    procedure SetLockFrameAnchorToParagraph(const AValue: Boolean);
  public
    constructor Create; override;
    procedure SetBooleanValue(AMask: Integer; ABitVal: Boolean);
    function GetBooleanValue(AMask: Integer): Boolean;
    procedure CopyFrom(Source: TdxCloneable); override;
    function Clone: TdxParagraphFrameFormattingInfo; reintroduce; inline;
    function Equals(AObj: TObject): Boolean; override; final;
    function GetHashCode: Integer; override; final;

    property HorizontalPosition: Integer read FHorizontalPosition write FHorizontalPosition;
    property VerticalPosition: Integer read FVerticalPosition write FVerticalPosition;
    property HorizontalPadding: Integer read FHorizontalPadding write FHorizontalPadding;
    property VerticalPadding: Integer read FVerticalPadding write FVerticalPadding;
    property HorizontalRule: TdxParagraphFrameHorizontalRule read GetHorizontalRule write SetHorizontalRule;
    property TextWrapType: TdxParagraphFrameTextWrapType read GetTextWrapType write SetTextWrapType;
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
    property HorizontalPositionAlignment: TdxParagraphFrameHorizontalPositionAlignment read GetHorizontalPositionAlignment write SetHorizontalPositionAlignment;
    property VerticalPositionAlignment: TdxParagraphFrameVerticalPositionAlignment read GetVerticalPositionAlignment write SetVerticalPositionAlignment;
    property HorizontalPositionType: TdxParagraphFrameHorizontalPositionType read GetHorizontalPositionType write SetHorizontalPositionType;
    property VerticalPositionType: TdxParagraphFrameVerticalPositionType read GetVerticalPositionType write SetVerticalPositionType;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property DropCap: TdxDropCapLocation read GetDropCap write SetDropCap;
    property DropCapVerticalHeightInLines: Integer read FDropCapVerticalHeightInLines write FDropCapVerticalHeightInLines;
    property LockFrameAnchorToParagraph: Boolean read GetLockFrameAnchorToParagraph write SetLockFrameAnchorToParagraph;
  end;

  { TdxParagraphFrameFormattingOptions }

  TdxParagraphFrameFormattingOptions = record
  public
    type TMask = (
      UseHorizontalPosition,
      UseVerticalPosition,
      UseHorizontalPadding,
      UseVerticalPadding,
      UseHorizontalRule,
      UseTextWrapType,
      UseX,
      UseY,
      UseWidth,
      UseHeight,
      UseHorizontalPositionAlignment,
      UseVerticalPositionAlignment,
      UseHorizontalPositionType,
      UseVerticalPositionType,
      UseDropCap,
      UseDropCapVerticalHeightInLines,
      UseLockFrameAnchorToParagraph
    );
    TMasks = set of TMask;
  const
    MaskUseNone = [];
    MaskUseAll = [Low(TMask)..High(TMask)];
  strict private
    FValue: TMasks;
    procedure SetValue(AMask: TMask; AValue: Boolean);
    function GetValue(AMask: TMask): Boolean;
  private
    class function GetEmptyParagraphFrameFormattingOption: TdxParagraphFrameFormattingOptions; static;
    class function GetRootParagraphFrameFormattingOption: TdxParagraphFrameFormattingOptions; static;
  public
    class function Create: TdxParagraphFrameFormattingOptions; overload; static;
    class property RootParagraphFrameFormattingOption: TdxParagraphFrameFormattingOptions read GetRootParagraphFrameFormattingOption;
    class property EmptyParagraphFrameFormattingOption: TdxParagraphFrameFormattingOptions read GetEmptyParagraphFrameFormattingOption;
    constructor Create(AValue: TMasks); overload;

    property Value: TMasks read FValue write FValue;

    property UseHorizontalPosition: Boolean index TMask.UseHorizontalPosition read GetValue write SetValue;
    property UseVerticalPosition: Boolean index TMask.UseVerticalPosition read GetValue write SetValue;
    property UseHorizontalPadding: Boolean index TMask.UseHorizontalPadding read GetValue write SetValue;
    property UseVerticalPadding: Boolean index TMask.UseVerticalPadding read GetValue write SetValue;
    property UseHorizontalRule: Boolean index TMask.UseHorizontalRule read GetValue write SetValue;
    property UseTextWrapType: Boolean index TMask.UseTextWrapType read GetValue write SetValue;
    property UseX: Boolean index TMask.UseX read GetValue write SetValue;
    property UseY: Boolean index TMask.UseY read GetValue write SetValue;
    property UseWidth: Boolean index TMask.UseWidth read GetValue write SetValue;
    property UseHeight: Boolean index TMask.UseHeight read GetValue write SetValue;
    property UseHorizontalPositionAlignment: Boolean index TMask.UseHorizontalPositionAlignment read GetValue write SetValue;
    property UseVerticalPositionAlignment: Boolean index TMask.UseVerticalPositionAlignment read GetValue write SetValue;
    property UseHorizontalPositionType: Boolean index TMask.UseHorizontalPositionType read GetValue write SetValue;
    property UseVerticalPositionType: Boolean index TMask.UseVerticalPositionType read GetValue write SetValue;
    property UseDropCap: Boolean index TMask.UseDropCap read GetValue write SetValue;
    property UseDropCapVerticalHeightInLines: Boolean index TMask.UseDropCapVerticalHeightInLines read GetValue write SetValue;
    property UseLockFrameAnchorToParagraph: Boolean index TMask.UseLockFrameAnchorToParagraph read GetValue write SetValue;
  end;

  { TdxParagraphFrameFormattingBase }

  TdxParagraphFrameFormattingBase = class(TdxIndexBasedObjectB<TdxParagraphFrameFormattingInfo, TdxParagraphFrameFormattingOptions>,
    IdxParagraphFrameLocation)
  strict private
    function GetDropCap: TdxDropCapLocation;
    function GetDropCapVerticalHeightInLines: Integer;
    function GetHeight: Integer;
    function GetHorizontalPadding: Integer;
    function GetHorizontalPosition: Integer;
    function GetHorizontalPositionAlignment: TdxParagraphFrameHorizontalPositionAlignment;
    function GetHorizontalPositionType: TdxParagraphFrameHorizontalPositionType;
    function GetHorizontalRule: TdxParagraphFrameHorizontalRule;
    function GetLockFrameAnchorToParagraph: Boolean;
    function GetTextWrapType: TdxParagraphFrameTextWrapType;
    function GetUseValue: TdxParagraphFrameFormattingOptions.TMasks;
    function GetVerticalPadding: Integer;
    function GetVerticalPosition: Integer;
    function GetVerticalPositionAlignment: TdxParagraphFrameVerticalPositionAlignment;
    function GetVerticalPositionType: TdxParagraphFrameVerticalPositionType;
    function GetWidth: Integer;
    function GetX: Integer;
    function GetY: Integer;
    procedure SetDropCap(const AValue: TdxDropCapLocation);
    procedure SetDropCapVerticalHeightInLines(const AValue: Integer);
    procedure SetHeight(const AValue: Integer);
    procedure SetHorizontalPadding(const AValue: Integer);
    procedure SetHorizontalPosition(const AValue: Integer);
    procedure SetHorizontalPositionAlignment(const AValue: TdxParagraphFrameHorizontalPositionAlignment);
    procedure SetHorizontalPositionType(const AValue: TdxParagraphFrameHorizontalPositionType);
    procedure SetHorizontalRule(const AValue: TdxParagraphFrameHorizontalRule);
    procedure SetLockFrameAnchorToParagraph(const AValue: Boolean);
    procedure SetTextWrapType(const AValue: TdxParagraphFrameTextWrapType);
    procedure SetUseValue(const AValue: TdxParagraphFrameFormattingOptions.TMasks);
    procedure SetVerticalPadding(const AValue: Integer);
    procedure SetVerticalPosition(const AValue: Integer);
    procedure SetVerticalPositionAlignment(const AValue: TdxParagraphFrameVerticalPositionAlignment);
    procedure SetVerticalPositionType(const AValue: TdxParagraphFrameVerticalPositionType);
    procedure SetWidth(const AValue: Integer);
    procedure SetX(const AValue: Integer);
    procedure SetY(const AValue: Integer);
  protected
    function CanSetPropertyValue: Boolean; override;
    function PropertyEquals(const AOther: TdxIndexBasedObject<TdxParagraphFrameFormattingInfo, TdxParagraphFrameFormattingOptions>): Boolean; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; const ADocumentModel: TdxCustomDocumentModel;
      const AFormattingInfo: TdxParagraphFrameFormattingInfo; const AFormattingOptions: TdxParagraphFrameFormattingOptions); reintroduce;

    procedure SetHorizontalPositionCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: Integer);
    procedure SetVerticalPositionCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: Integer);
    procedure SetHorizontalPaddingCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: Integer);
    procedure SetVerticalPaddingCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: Integer);
    procedure SetHorizontalRuleCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: TdxParagraphFrameHorizontalRule);
    procedure SetTextWrapTypeCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: TdxParagraphFrameTextWrapType);
    procedure SetXCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: Integer);
    procedure SetYCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: Integer);
    procedure SetHorizontalPositionAlignmentCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: TdxParagraphFrameHorizontalPositionAlignment);
    procedure SetVerticalPositionAlignmentCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: TdxParagraphFrameVerticalPositionAlignment);
    procedure SetHorizontalPositionTypeCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: TdxParagraphFrameHorizontalPositionType);
    procedure SetVerticalPositionTypeCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: TdxParagraphFrameVerticalPositionType);
    procedure SetWidthCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: Integer);
    procedure SetHeightCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: Integer);
    procedure SetDropCapCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: TdxDropCapLocation);
    procedure SetDropCapVerticalHeightInLinesCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: Integer);
    procedure SetLockFrameAnchorToParagraphCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: Boolean);

    procedure CopyFrom(Source: TdxCloneable); overload; override;
    procedure CopyFrom(const AInfo: TdxParagraphFrameFormattingInfo; const AOptions: TdxParagraphFrameFormattingOptions); reintroduce; overload; inline;
    function Clone: TdxCloneable; override;

    property HorizontalPosition: Integer read GetHorizontalPosition write SetHorizontalPosition;
    property VerticalPosition: Integer read GetVerticalPosition write SetVerticalPosition;
    property HorizontalPadding: Integer read GetHorizontalPadding write SetHorizontalPadding;
    property VerticalPadding: Integer read GetVerticalPadding write SetVerticalPadding;
    property HorizontalRule: TdxParagraphFrameHorizontalRule read GetHorizontalRule write SetHorizontalRule;
    property TextWrapType: TdxParagraphFrameTextWrapType read GetTextWrapType write SetTextWrapType;
    property X: Integer read GetX write SetX;
    property Y: Integer read GetY write SetY;
    property HorizontalPositionAlignment: TdxParagraphFrameHorizontalPositionAlignment read GetHorizontalPositionAlignment write SetHorizontalPositionAlignment;
    property VerticalPositionAlignment: TdxParagraphFrameVerticalPositionAlignment read GetVerticalPositionAlignment write SetVerticalPositionAlignment;
    property HorizontalPositionType: TdxParagraphFrameHorizontalPositionType read GetHorizontalPositionType write SetHorizontalPositionType;
    property VerticalPositionType: TdxParagraphFrameVerticalPositionType read GetVerticalPositionType write SetVerticalPositionType;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property DropCap: TdxDropCapLocation read GetDropCap write SetDropCap;
    property DropCapVerticalHeightInLines: Integer read GetDropCapVerticalHeightInLines write SetDropCapVerticalHeightInLines;
    property LockFrameAnchorToParagraph: Boolean read GetLockFrameAnchorToParagraph write SetLockFrameAnchorToParagraph;
    property UseValue: TdxParagraphFrameFormattingOptions.TMasks read GetUseValue write SetUseValue;
  end;

  { TdxParagraphFrameFormattingInfoCache }

  TdxParagraphFrameFormattingInfoCache = class(TdxUniqueItemsCache<TdxParagraphFrameFormattingInfo>)
  strict private
    FDefaultItem: TdxParagraphFrameFormattingInfo;
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxParagraphFrameFormattingInfo; override;
    procedure InitItems(const AUnitConverter: IdxDocumentModelUnitConverter); override;
  public
    class function CreateDefaultInfo: TdxParagraphFrameFormattingInfo; static;
    property DefaultItem: TdxParagraphFrameFormattingInfo read FDefaultItem;
  end;

  { TdxParagraphFrameFormattingCache }

  TdxParagraphFrameFormattingCache = class(TdxUniqueItemsCache<TdxParagraphFrameFormattingBase>)
  public const
    EmptyParagraphFrameFormattingIndex = 0;
    RootParagraphFrameFormattingIndex = 1;
    DefaultParagraphFrameFormattingIndex = 2;
  strict private
    FDocumentModel: TdxCustomDocumentModel;
    FEmptyParagraphFrameFormattingInfo: TdxParagraphFrameFormattingInfo;
    FDefaultParagraphFrameFormattingInfo: TdxParagraphFrameFormattingInfo;
  protected
    function CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxParagraphFrameFormattingBase; override;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel);
    destructor Destroy; override;

    property DocumentModel: TdxCustomDocumentModel read FDocumentModel;
    property DefaultParagraphFrameFormattingInfo: TdxParagraphFrameFormattingInfo read FDefaultParagraphFrameFormattingInfo;
    property EmptyParagraphFrameFormattingInfo: TdxParagraphFrameFormattingInfo read FEmptyParagraphFrameFormattingInfo;
  end;

  { TdxParagraphFrameFormattingChangeActionsCalculator }

  TdxParagraphFrameFormattingChangeType = (
    None,
    HorizontalPosition,
    VerticalPosition,
    HorizontalPadding,
    VerticalPadding,
    HorizontalRule,
    TextWrapType,
    X,
    Y,
    Width,
    Height,
    HorizontalPositionAlignment,
    VerticalPositionAlignment,
    HorizontalPositionType,
    VerticalPositionType,
    DropCap,
    DropCapVerticalHeightInLines,
    LockFrameAnchorToParagraph,
    BatchUpdate
  );

  TdxParagraphFrameFormattingChangeActionsCalculator = class
    class function CalculateChangeActions(AChange: TdxParagraphFrameFormattingChangeType): TdxDocumentModelChangeActions; static;
  end;

  { TdxFrameProperties }

  TdxFrameProperties = class(TdxRichEditIndexBasedObject<TdxParagraphFrameFormattingBase>, IdxParagraphFrameLocation)
  strict private
    FOwner: IdxParagraphPropertiesContainer;
    function GetVerticalPosition: Integer;
    procedure SetVerticalPosition(const AValue: Integer);
    function GetUseVerticalPosition: Boolean;
    function GetHorizontalPosition: Integer;
    procedure SetHorizontalPosition(const AValue: Integer);
    function GetUseHorizontalPosition: Boolean;
    function GetHorizontalPadding: Integer;
    procedure SetHorizontalPadding(const AValue: Integer);
    function GetUseHorizontalPadding: Boolean;
    function GetVerticalPadding: Integer;
    procedure SetVerticalPadding(const AValue: Integer);
    function GetUseVerticalPadding: Boolean;
    function GetHorizontalRule: TdxParagraphFrameHorizontalRule;
    procedure SetHorizontalRule(const AValue: TdxParagraphFrameHorizontalRule);
    function GetUseHorizontalRule: Boolean;
    function GetTextWrapType: TdxParagraphFrameTextWrapType;
    procedure SetTextWrapType(const AValue: TdxParagraphFrameTextWrapType);
    function GetUseTextWrapType: Boolean;
    function GetX: Integer;
    procedure SetX(const AValue: Integer);
    function GetUseX: Boolean;
    function GetY: Integer;
    procedure SetY(const AValue: Integer);
    function GetUseY: Boolean;
    function GetHorizontalPositionAlignment: TdxParagraphFrameHorizontalPositionAlignment;
    procedure SetHorizontalPositionAlignment(const AValue: TdxParagraphFrameHorizontalPositionAlignment);
    function GetUseHorizontalPositionAlignment: Boolean;
    function GetVerticalPositionAlignment: TdxParagraphFrameVerticalPositionAlignment;
    procedure SetVerticalPositionAlignment(const AValue: TdxParagraphFrameVerticalPositionAlignment);
    function GetUseVerticalPositionAlignment: Boolean;
    function GetHorizontalPositionType: TdxParagraphFrameHorizontalPositionType;
    procedure SetHorizontalPositionType(const AValue: TdxParagraphFrameHorizontalPositionType);
    function GetUseHorizontalPositionType: Boolean;
    function GetVerticalPositionType: TdxParagraphFrameVerticalPositionType;
    procedure SetVerticalPositionType(const AValue: TdxParagraphFrameVerticalPositionType);
    function GetUseVerticalPositionType: Boolean;
    function GetWidth: Integer;
    procedure SetWidth(const AValue: Integer);
    function GetUseWidth: Boolean;
    function GetHeight: Integer;
    procedure SetHeight(const AValue: Integer);
    function GetUseHeight: Boolean;
    function GetLockFrameAnchorToParagraph: Boolean;
    procedure SetLockFrameAnchorToParagraph(const AValue: Boolean);
    function GetUseLockFrameAnchorToParagraph: Boolean;
    function GetDropCap: TdxDropCapLocation;
    procedure SetDropCap(const AValue: TdxDropCapLocation);
    function GetUseDropCap: Boolean;
    function GetDropCapVerticalHeightInLines: Integer;
    procedure SetDropCapVerticalHeightInLines(const AValue: Integer);
    function GetUseDropCapVerticalHeightInLines: Boolean;
  strict protected
    function SetVerticalPositionCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetHorizontalPositionCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetHorizontalPaddingCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetVerticalPaddingCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetHorizontalRuleCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: TdxParagraphFrameHorizontalRule): TdxDocumentModelChangeActions; virtual;
    function SetTextWrapTypeCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: TdxParagraphFrameTextWrapType): TdxDocumentModelChangeActions; virtual;
    function SetXCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetYCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetHorizontalPositionAlignmentCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: TdxParagraphFrameHorizontalPositionAlignment): TdxDocumentModelChangeActions; virtual;
    function SetVerticalPositionAlignmentCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: TdxParagraphFrameVerticalPositionAlignment): TdxDocumentModelChangeActions; virtual;
    function SetHorizontalPositionTypeCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: TdxParagraphFrameHorizontalPositionType): TdxDocumentModelChangeActions; virtual;
    function SetVerticalPositionTypeCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: TdxParagraphFrameVerticalPositionType): TdxDocumentModelChangeActions; virtual;
    function SetWidthCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetHeightCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: Integer): TdxDocumentModelChangeActions; virtual;
    function SetLockFrameAnchorToParagraphCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: Boolean): TdxDocumentModelChangeActions; virtual;
    function SetDropCapCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: TdxDropCapLocation): TdxDocumentModelChangeActions; virtual;
    function SetDropCapVerticalHeightInLinesCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: Integer): TdxDocumentModelChangeActions; virtual;

    function GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxParagraphFrameFormattingBase>; override;
    function UseVal(AMask: TdxParagraphFrameFormattingOptions.TMask): Boolean;
    function GetBatchUpdateChangeActions: TdxDocumentModelChangeActions; override;
    procedure OnIndexChanged; override;
    function CreateIndexChangedHistoryItem: TdxIndexChangedHistoryItemCore; override;
    function GetObtainAffectedRangeListener: IdxObtainAffectedRangeListener; override;
  public
    constructor Create(const AOwner: IdxParagraphPropertiesContainer); reintroduce; overload;
    class procedure ApplyPropertiesDiff(ATarget: TdxFrameProperties; ATargetMergedInfo: TdxParagraphFrameFormattingInfo;
      ASourceMergedInfo: TdxParagraphFrameFormattingInfo); static;
    procedure Reset;
    function Equals(AObj: TObject): Boolean; override;
    procedure ResetUse(const AMask: TdxParagraphFrameFormattingOptions.TMasks);
    procedure ResetAllUse;
    function GetHashCode: Integer; override;
    procedure MakeDefault;
    function CanMerge(AProperties: TdxFrameProperties): Boolean;
    procedure Merge(AProperties: TdxFrameProperties); virtual;
    procedure CopyFrom(AFrameProperties: TdxMergedProperties<TdxParagraphFrameFormattingInfo, TdxParagraphFrameFormattingOptions>); overload;
  {$IFNDEF DELPHI17}
    procedure CopyFrom(const Source: TdxParagraphFrameFormattingBase); override;
    procedure CopyFrom(const Source: TdxUndoableIndexBasedObject<TdxParagraphFrameFormattingBase>); override;
  {$ENDIF}

    property VerticalPosition: Integer read GetVerticalPosition write SetVerticalPosition;
    property UseVerticalPosition: Boolean read GetUseVerticalPosition;
    property HorizontalPosition: Integer read GetHorizontalPosition write SetHorizontalPosition;
    property UseHorizontalPosition: Boolean read GetUseHorizontalPosition;
    property HorizontalPadding: Integer read GetHorizontalPadding write SetHorizontalPadding;
    property UseHorizontalPadding: Boolean read GetUseHorizontalPadding;
    property VerticalPadding: Integer read GetVerticalPadding write SetVerticalPadding;
    property UseVerticalPadding: Boolean read GetUseVerticalPadding;
    property HorizontalRule: TdxParagraphFrameHorizontalRule read GetHorizontalRule write SetHorizontalRule;
    property UseHorizontalRule: Boolean read GetUseHorizontalRule;
    property TextWrapType: TdxParagraphFrameTextWrapType read GetTextWrapType write SetTextWrapType;
    property UseTextWrapType: Boolean read GetUseTextWrapType;
    property X: Integer read GetX write SetX;
    property UseX: Boolean read GetUseX;
    property Y: Integer read GetY write SetY;
    property UseY: Boolean read GetUseY;
    property HorizontalPositionAlignment: TdxParagraphFrameHorizontalPositionAlignment read GetHorizontalPositionAlignment write SetHorizontalPositionAlignment;
    property UseHorizontalPositionAlignment: Boolean read GetUseHorizontalPositionAlignment;
    property VerticalPositionAlignment: TdxParagraphFrameVerticalPositionAlignment read GetVerticalPositionAlignment write SetVerticalPositionAlignment;
    property UseVerticalPositionAlignment: Boolean read GetUseVerticalPositionAlignment;
    property HorizontalPositionType: TdxParagraphFrameHorizontalPositionType read GetHorizontalPositionType write SetHorizontalPositionType;
    property UseHorizontalPositionType: Boolean read GetUseHorizontalPositionType;
    property VerticalPositionType: TdxParagraphFrameVerticalPositionType read GetVerticalPositionType write SetVerticalPositionType;
    property UseVerticalPositionType: Boolean read GetUseVerticalPositionType;
    property Width: Integer read GetWidth write SetWidth;
    property UseWidth: Boolean read GetUseWidth;
    property Height: Integer read GetHeight write SetHeight;
    property UseHeight: Boolean read GetUseHeight;
    property LockFrameAnchorToParagraph: Boolean read GetLockFrameAnchorToParagraph write SetLockFrameAnchorToParagraph;
    property UseLockFrameAnchorToParagraph: Boolean read GetUseLockFrameAnchorToParagraph;
    property DropCap: TdxDropCapLocation read GetDropCap write SetDropCap;
    property UseDropCap: Boolean read GetUseDropCap;
    property DropCapVerticalHeightInLines: Integer read GetDropCapVerticalHeightInLines write SetDropCapVerticalHeightInLines;
    property UseDropCapVerticalHeightInLines: Boolean read GetUseDropCapVerticalHeightInLines;
  end;

  { TdxMergedFrameProperties }

  TdxMergedFrameProperties = class(TdxMergedProperties<TdxParagraphFrameFormattingInfo, TdxParagraphFrameFormattingOptions>,
    IdxParagraphFrameLocation)
  strict private
    function GetX: Integer;
    function GetY: Integer;
    function GetHorizontalPositionAlignment: TdxParagraphFrameHorizontalPositionAlignment;
    function GetVerticalPositionAlignment: TdxParagraphFrameVerticalPositionAlignment;
    function GetHorizontalPositionType: TdxParagraphFrameHorizontalPositionType;
    function GetVerticalPositionType: TdxParagraphFrameVerticalPositionType;
    function GetWidth: Integer;
    function GetHeight: Integer;
  protected
    procedure MergeCore(AInfo: TdxParagraphFrameFormattingInfo; AOptions: TdxParagraphFrameFormattingOptions);
  public
    constructor Create(AInitialProperties: TdxFrameProperties); reintroduce; overload;
    constructor Create(AInitial: TdxMergedFrameProperties); reintroduce; overload;
    function CanMerge(AProperties: TdxMergedFrameProperties): Boolean;
    function Equals(AObj: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    function IsParagraphFrame: Boolean; virtual;

    procedure Merge(AProperties: TdxFrameProperties); overload;
    procedure Merge(AProperties: TdxMergedFrameProperties); overload;

    property X: Integer read GetX;
    property Y: Integer read GetY;
    property HorizontalPositionAlignment: TdxParagraphFrameHorizontalPositionAlignment read GetHorizontalPositionAlignment;
    property VerticalPositionAlignment: TdxParagraphFrameVerticalPositionAlignment read GetVerticalPositionAlignment;
    property HorizontalPositionType: TdxParagraphFrameHorizontalPositionType read GetHorizontalPositionType;
    property VerticalPositionType: TdxParagraphFrameVerticalPositionType read GetVerticalPositionType;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

implementation

uses
  dxRichEdit.DocumentModel.Cache.Simple;

{ TdxParagraphFrameFormattingInfo }

constructor TdxParagraphFrameFormattingInfo.Create;
begin
  inherited Create;
  HorizontalPositionType := TdxParagraphFrameHorizontalPositionType.Column;
  VerticalPositionType := TdxParagraphFrameVerticalPositionType.Margin;
end;

function TdxParagraphFrameFormattingInfo.GetHorizontalRule: TdxParagraphFrameHorizontalRule;
begin
  Result := TdxParagraphFrameHorizontalRule(FPackedValues and MaskHorizontalRule);
end;

procedure TdxParagraphFrameFormattingInfo.SetHorizontalRule(const AValue: TdxParagraphFrameHorizontalRule);
begin
  FPackedValues := FPackedValues and not MaskHorizontalRule;
  FPackedValues := FPackedValues or Integer(AValue) and MaskHorizontalRule;
end;

function TdxParagraphFrameFormattingInfo.GetTextWrapType: TdxParagraphFrameTextWrapType;
begin
  Result := TdxParagraphFrameTextWrapType((FPackedValues and MaskWrapType) shr 2);
end;

procedure TdxParagraphFrameFormattingInfo.SetTextWrapType(const AValue: TdxParagraphFrameTextWrapType);
begin
  FPackedValues := FPackedValues and not MaskWrapType;
  FPackedValues := FPackedValues or (Integer(AValue) shl 2) and MaskWrapType;
end;

function TdxParagraphFrameFormattingInfo.GetHorizontalPositionAlignment: TdxParagraphFrameHorizontalPositionAlignment;
begin
  Result := TdxParagraphFrameHorizontalPositionAlignment((FPackedValues and MaskHorizontalPositionAlignment) shr 9);
end;

procedure TdxParagraphFrameFormattingInfo.SetHorizontalPositionAlignment(const AValue: TdxParagraphFrameHorizontalPositionAlignment);
begin
  FPackedValues := FPackedValues and not MaskHorizontalPositionAlignment;
  FPackedValues := FPackedValues or (Integer(AValue) shl 9) and MaskHorizontalPositionAlignment;
end;

function TdxParagraphFrameFormattingInfo.GetVerticalPositionAlignment: TdxParagraphFrameVerticalPositionAlignment;
begin
  Result := TdxParagraphFrameVerticalPositionAlignment((FPackedValues and MaskVerticalPositionAlignment) shr 12);
end;

procedure TdxParagraphFrameFormattingInfo.SetVerticalPositionAlignment(const AValue: TdxParagraphFrameVerticalPositionAlignment);
begin
  FPackedValues := FPackedValues and not MaskVerticalPositionAlignment;
  FPackedValues := FPackedValues or (Integer(AValue) shl 12) and MaskVerticalPositionAlignment;
end;

function TdxParagraphFrameFormattingInfo.GetHorizontalPositionType: TdxParagraphFrameHorizontalPositionType;
begin
  Result := TdxParagraphFrameHorizontalPositionType((FPackedValues and MaskHorizontalPositionType) shr 5);
end;

procedure TdxParagraphFrameFormattingInfo.SetHorizontalPositionType(const AValue: TdxParagraphFrameHorizontalPositionType);
begin
  FPackedValues := FPackedValues and not MaskHorizontalPositionType;
  FPackedValues := FPackedValues or (Integer(AValue) shl 5) and MaskHorizontalPositionType;
end;

function TdxParagraphFrameFormattingInfo.GetVerticalPositionType: TdxParagraphFrameVerticalPositionType;
begin
  Result := TdxParagraphFrameVerticalPositionType((FPackedValues and MaskVerticalPositionType) shr 7);
end;

procedure TdxParagraphFrameFormattingInfo.SetVerticalPositionType(const AValue: TdxParagraphFrameVerticalPositionType);
begin
  FPackedValues := FPackedValues and not MaskVerticalPositionType;
  FPackedValues := FPackedValues or (Integer(AValue) shl 7) and MaskVerticalPositionType;
end;

function TdxParagraphFrameFormattingInfo.GetDropCap: TdxDropCapLocation;
begin
  Result := TdxDropCapLocation((FPackedValues and MaskDropCap) shr 16);
end;

procedure TdxParagraphFrameFormattingInfo.SetDropCap(const AValue: TdxDropCapLocation);
begin
  FPackedValues := FPackedValues and not MaskDropCap;
  FPackedValues := FPackedValues or (Integer(AValue) shl 16) and MaskDropCap;
end;

function TdxParagraphFrameFormattingInfo.GetLockFrameAnchorToParagraph: Boolean;
begin
  Result := GetBooleanValue(MaskLockFrameAnchorToParagraph);
end;

procedure TdxParagraphFrameFormattingInfo.SetLockFrameAnchorToParagraph(const AValue: Boolean);
begin
  SetBooleanValue(MaskLockFrameAnchorToParagraph, AValue);
end;

procedure TdxParagraphFrameFormattingInfo.SetBooleanValue(AMask: Integer; ABitVal: Boolean);
begin
  if ABitVal then
    FPackedValues := FPackedValues or AMask
  else
    FPackedValues := FPackedValues and not AMask;
end;

function TdxParagraphFrameFormattingInfo.GetBooleanValue(AMask: Integer): Boolean;
begin
  Result := (FPackedValues and AMask) <> 0;
end;

function TdxParagraphFrameFormattingInfo.Clone: TdxParagraphFrameFormattingInfo;
begin
  Result := TdxParagraphFrameFormattingInfo(inherited Clone);
end;

procedure TdxParagraphFrameFormattingInfo.CopyFrom(Source: TdxCloneable);
var
  ASource: TdxParagraphFrameFormattingInfo absolute Source;
begin
  FPackedValues := ASource.FPackedValues;

  VerticalPosition := ASource.VerticalPosition;
  HorizontalPosition := ASource.HorizontalPosition;
  HorizontalPadding := ASource.HorizontalPadding;
  VerticalPadding := ASource.VerticalPadding;
  Height := ASource.Height;
  Width := ASource.Width;
  X := ASource.X;
  Y := ASource.Y;
  DropCapVerticalHeightInLines := ASource.DropCapVerticalHeightInLines;
end;

function TdxParagraphFrameFormattingInfo.Equals(AObj: TObject): Boolean;
var
  AInfo: TdxParagraphFrameFormattingInfo;
begin
  if AObj = nil then
    Exit(False);
  if AObj = Self then
    Exit(True);

  AInfo := TdxParagraphFrameFormattingInfo(AObj);
  Result :=
    (FPackedValues = AInfo.FPackedValues) and
    (VerticalPosition = AInfo.VerticalPosition) and
    (HorizontalPosition = AInfo.HorizontalPosition) and
    (HorizontalPadding = AInfo.HorizontalPadding) and
    (VerticalPadding = AInfo.VerticalPadding) and
    (Height = AInfo.Height) and (Width = AInfo.Width) and
    (X = AInfo.X) and
    (Y = AInfo.Y) and
    (DropCapVerticalHeightInLines = AInfo.DropCapVerticalHeightInLines);
end;

function TdxParagraphFrameFormattingInfo.GetHashCode: Integer;
begin
  Result := FPackedValues xor FVerticalPosition xor FHorizontalPosition xor FHorizontalPadding xor
    FVerticalPadding xor FHeight xor FWidth xor FX xor FY xor FDropCapVerticalHeightInLines;
end;

{ TdxParagraphFrameFormattingInfoCache }

class function TdxParagraphFrameFormattingInfoCache.CreateDefaultInfo: TdxParagraphFrameFormattingInfo;
begin
  Result := TdxParagraphFrameFormattingInfo.Create;
  Result.Width := 0;
  Result.HorizontalRule := TdxParagraphFrameHorizontalRule.Auto;
  Result.X := 0;
  Result.Y := 0;
  Result.HorizontalPositionType := TdxParagraphFrameHorizontalPositionType.Column;
  Result.VerticalPositionType := TdxParagraphFrameVerticalPositionType.Margin;
  Result.TextWrapType := TdxParagraphFrameTextWrapType.Auto;
  Result.HorizontalPositionAlignment := TdxParagraphFrameHorizontalPositionAlignment.Left;
  Result.VerticalPositionAlignment := TdxParagraphFrameVerticalPositionAlignment.Inline;
end;

function TdxParagraphFrameFormattingInfoCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxParagraphFrameFormattingInfo;
begin
  Result := TdxParagraphFrameFormattingInfo.Create;
end;

procedure TdxParagraphFrameFormattingInfoCache.InitItems(const AUnitConverter: IdxDocumentModelUnitConverter);
begin
  inherited InitItems(AUnitConverter);
  FDefaultItem := CreateDefaultInfo;
  AppendItem(FDefaultItem);
end;

{ TdxParagraphFrameFormattingCache }

constructor TdxParagraphFrameFormattingCache.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel.UnitConverter);
  Assert(ADocumentModel <> nil, 'documentModel');
  FDocumentModel := ADocumentModel;

  FEmptyParagraphFrameFormattingInfo := TdxParagraphFrameFormattingInfo.Create;
  FDefaultParagraphFrameFormattingInfo := TdxParagraphFrameFormattingInfoCache.CreateDefaultInfo;

  AppendItem(TdxParagraphFrameFormattingBase.Create(DocumentModel.MainPart,
    DocumentModel, FDefaultParagraphFrameFormattingInfo,
    TdxParagraphFrameFormattingOptions.EmptyParagraphFrameFormattingOption));
  AppendItem(TdxParagraphFrameFormattingBase.Create(DocumentModel.MainPart,
    DocumentModel, FDefaultParagraphFrameFormattingInfo,
    TdxParagraphFrameFormattingOptions.RootParagraphFrameFormattingOption));
  AppendItem(TdxParagraphFrameFormattingBase.Create(DocumentModel.MainPart,
    DocumentModel, FDefaultParagraphFrameFormattingInfo,
    TdxParagraphFrameFormattingOptions.RootParagraphFrameFormattingOption));
end;

destructor TdxParagraphFrameFormattingCache.Destroy;
begin
  FreeAndNil(FEmptyParagraphFrameFormattingInfo);
  FreeAndNil(FDefaultParagraphFrameFormattingInfo);
  inherited Destroy;
end;

function TdxParagraphFrameFormattingCache.CreateDefaultItem(const AUnitConverter: IdxDocumentModelUnitConverter): TdxParagraphFrameFormattingBase;
begin
  Result := nil;
end;

{ TdxParagraphFrameFormattingOptions }

constructor TdxParagraphFrameFormattingOptions.Create(AValue: TMasks);
begin
  FValue := AValue;
end;

class function TdxParagraphFrameFormattingOptions.Create: TdxParagraphFrameFormattingOptions;
begin
  Result.FValue := MaskUseNone;
end;

procedure TdxParagraphFrameFormattingOptions.SetValue(AMask: TMask; AValue: Boolean);
begin
  if AValue then
    Include(FValue, AMask)
  else
    Exclude(FValue, AMask);
end;

class function TdxParagraphFrameFormattingOptions.GetEmptyParagraphFrameFormattingOption: TdxParagraphFrameFormattingOptions;
begin
  Result := TdxParagraphFrameFormattingOptions.Create;
end;

class function TdxParagraphFrameFormattingOptions.GetRootParagraphFrameFormattingOption: TdxParagraphFrameFormattingOptions;
begin
  Result := TdxParagraphFrameFormattingOptions.Create(TdxParagraphFrameFormattingOptions.MaskUseAll);
end;

function TdxParagraphFrameFormattingOptions.GetValue(AMask: TMask): Boolean;
begin
  Result := AMask in FValue;
end;

{ TdxParagraphFrameFormattingBase }

constructor TdxParagraphFrameFormattingBase.Create(APieceTable: TdxCustomPieceTable; const ADocumentModel: TdxCustomDocumentModel;
  const AFormattingInfo: TdxParagraphFrameFormattingInfo; const AFormattingOptions: TdxParagraphFrameFormattingOptions);
begin
  inherited Create(APieceTable, ADocumentModel, AFormattingInfo, AFormattingOptions);
end;

function TdxParagraphFrameFormattingBase.GetHorizontalPosition: Integer;
begin
  Result := Info.HorizontalPosition;
end;

procedure TdxParagraphFrameFormattingBase.SetHorizontalPosition(const AValue: Integer);
begin
  if (Info.HorizontalPosition = AValue) and Options.UseHorizontalPosition then
    Exit;
  SetPropertyValue<Integer>(SetHorizontalPositionCore, AValue);
end;

procedure TdxParagraphFrameFormattingBase.SetHorizontalPositionCore(const AInfo: TdxParagraphFrameFormattingInfo;
  const AValue: Integer);
begin
  AInfo.HorizontalPosition := AValue;
  Options.UseHorizontalPosition := True;
end;

function TdxParagraphFrameFormattingBase.GetVerticalPosition: Integer;
begin
  Result := Info.VerticalPosition;
end;

procedure TdxParagraphFrameFormattingBase.SetVerticalPosition(const AValue: Integer);
begin
  if (Info.VerticalPosition = AValue) and Options.UseVerticalPosition then
    Exit;
  SetPropertyValue<Integer>(SetVerticalPositionCore, AValue);
end;

procedure TdxParagraphFrameFormattingBase.SetVerticalPositionCore(const AInfo: TdxParagraphFrameFormattingInfo;
  const AValue: Integer);
begin
  AInfo.VerticalPosition := AValue;
  Options.UseVerticalPosition := True;
end;

function TdxParagraphFrameFormattingBase.GetHorizontalPadding: Integer;
begin
  Result := Info.HorizontalPadding;
end;

procedure TdxParagraphFrameFormattingBase.SetHorizontalPadding(const AValue: Integer);
begin
  if (Info.HorizontalPadding = AValue) and Options.UseHorizontalPadding then
    Exit;
  SetPropertyValue<Integer>(SetHorizontalPaddingCore, AValue);
end;

procedure TdxParagraphFrameFormattingBase.SetHorizontalPaddingCore(const AInfo: TdxParagraphFrameFormattingInfo;
  const AValue: Integer);
begin
  AInfo.HorizontalPadding := AValue;
  Options.UseHorizontalPadding := True;
end;

function TdxParagraphFrameFormattingBase.GetVerticalPadding: Integer;
begin
  Result := Info.VerticalPadding;
end;

procedure TdxParagraphFrameFormattingBase.SetVerticalPadding(const AValue: Integer);
begin
  if (Info.VerticalPadding = AValue) and Options.UseVerticalPadding then
    Exit;
  SetPropertyValue<Integer>(SetVerticalPaddingCore, AValue);
end;

procedure TdxParagraphFrameFormattingBase.SetVerticalPaddingCore(const AInfo: TdxParagraphFrameFormattingInfo;
  const AValue: Integer);
begin
  AInfo.VerticalPadding := AValue;
  Options.UseVerticalPadding := True;
end;

function TdxParagraphFrameFormattingBase.GetHorizontalRule: TdxParagraphFrameHorizontalRule;
begin
  Result := Info.HorizontalRule;
end;

procedure TdxParagraphFrameFormattingBase.SetHorizontalRule(const AValue: TdxParagraphFrameHorizontalRule);
begin
  if (Info.HorizontalRule = AValue) and Options.UseHorizontalRule then
    Exit;
  SetPropertyValue<TdxParagraphFrameHorizontalRule>(SetHorizontalRuleCore, AValue);
end;

procedure TdxParagraphFrameFormattingBase.SetHorizontalRuleCore(const AInfo: TdxParagraphFrameFormattingInfo;
  const AValue: TdxParagraphFrameHorizontalRule);
begin
  AInfo.HorizontalRule := AValue;
  Options.UseHorizontalRule := True;
end;

function TdxParagraphFrameFormattingBase.GetTextWrapType: TdxParagraphFrameTextWrapType;
begin
  Result := Info.TextWrapType;
end;

procedure TdxParagraphFrameFormattingBase.SetTextWrapType(const AValue: TdxParagraphFrameTextWrapType);
begin
  if (Info.TextWrapType = AValue) and Options.UseTextWrapType then
    Exit;
  SetPropertyValue<TdxParagraphFrameTextWrapType>(SetTextWrapTypeCore, AValue);
end;

procedure TdxParagraphFrameFormattingBase.SetTextWrapTypeCore(const AInfo: TdxParagraphFrameFormattingInfo;
  const AValue: TdxParagraphFrameTextWrapType);
begin
  AInfo.TextWrapType := AValue;
  Options.UseTextWrapType := True;
end;

function TdxParagraphFrameFormattingBase.GetX: Integer;
begin
  Result := Info.X;
end;

procedure TdxParagraphFrameFormattingBase.SetX(const AValue: Integer);
begin
  if (Info.X = AValue) and Options.UseX then
    Exit;
  SetPropertyValue<Integer>(SetXCore, AValue);
end;

procedure TdxParagraphFrameFormattingBase.SetXCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: Integer);
begin
  AInfo.X := AValue;
  Options.UseX := True;
end;

function TdxParagraphFrameFormattingBase.GetY: Integer;
begin
  Result := Info.Y;
end;

procedure TdxParagraphFrameFormattingBase.SetY(const AValue: Integer);
begin
  if (Info.Y = AValue) and Options.UseY then
    Exit;
  SetPropertyValue<Integer>(SetYCore, AValue);
end;

procedure TdxParagraphFrameFormattingBase.SetYCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: Integer);
begin
  AInfo.Y := AValue;
  Options.UseY := True;
end;

function TdxParagraphFrameFormattingBase.GetHorizontalPositionAlignment: TdxParagraphFrameHorizontalPositionAlignment;
begin
  Result := Info.HorizontalPositionAlignment;
end;

procedure TdxParagraphFrameFormattingBase.SetHorizontalPositionAlignment(const AValue: TdxParagraphFrameHorizontalPositionAlignment);
begin
  if (Info.HorizontalPositionAlignment = AValue) and Options.UseHorizontalPositionAlignment then
    Exit;
  SetPropertyValue<TdxParagraphFrameHorizontalPositionAlignment>(SetHorizontalPositionAlignmentCore, AValue);
end;

procedure TdxParagraphFrameFormattingBase.SetHorizontalPositionAlignmentCore(const AInfo: TdxParagraphFrameFormattingInfo;
  const AValue: TdxParagraphFrameHorizontalPositionAlignment);
begin
  AInfo.HorizontalPositionAlignment := AValue;
  Options.UseHorizontalPositionAlignment := True;
end;

function TdxParagraphFrameFormattingBase.GetVerticalPositionAlignment: TdxParagraphFrameVerticalPositionAlignment;
begin
  Result := Info.VerticalPositionAlignment;
end;

procedure TdxParagraphFrameFormattingBase.SetVerticalPositionAlignment(const AValue: TdxParagraphFrameVerticalPositionAlignment);
begin
  if (Info.VerticalPositionAlignment = AValue) and Options.UseVerticalPositionAlignment then
    Exit;
  SetPropertyValue<TdxParagraphFrameVerticalPositionAlignment>(SetVerticalPositionAlignmentCore, AValue);
end;

procedure TdxParagraphFrameFormattingBase.SetVerticalPositionAlignmentCore(const AInfo: TdxParagraphFrameFormattingInfo;
  const AValue: TdxParagraphFrameVerticalPositionAlignment);
begin
  AInfo.VerticalPositionAlignment := AValue;
  Options.UseVerticalPositionAlignment := True;
end;

function TdxParagraphFrameFormattingBase.GetHorizontalPositionType: TdxParagraphFrameHorizontalPositionType;
begin
  Result := Info.HorizontalPositionType;
end;

procedure TdxParagraphFrameFormattingBase.SetHorizontalPositionType(const AValue: TdxParagraphFrameHorizontalPositionType);
begin
  if (Info.HorizontalPositionType = AValue) and Options.UseHorizontalPositionType then
    Exit;
  SetPropertyValue<TdxParagraphFrameHorizontalPositionType>(SetHorizontalPositionTypeCore, AValue);
end;

procedure TdxParagraphFrameFormattingBase.SetHorizontalPositionTypeCore(const AInfo: TdxParagraphFrameFormattingInfo;
  const AValue: TdxParagraphFrameHorizontalPositionType);
begin
  AInfo.HorizontalPositionType := AValue;
  Options.UseHorizontalPositionType := True;
end;

function TdxParagraphFrameFormattingBase.GetVerticalPositionType: TdxParagraphFrameVerticalPositionType;
begin
  Result := Info.VerticalPositionType;
end;

procedure TdxParagraphFrameFormattingBase.SetVerticalPositionType(const AValue: TdxParagraphFrameVerticalPositionType);
begin
  if (Info.VerticalPositionType = AValue) and Options.UseVerticalPositionType then
    Exit;
  SetPropertyValue<TdxParagraphFrameVerticalPositionType>(SetVerticalPositionTypeCore, AValue);
end;

procedure TdxParagraphFrameFormattingBase.SetVerticalPositionTypeCore(const AInfo: TdxParagraphFrameFormattingInfo;
  const AValue: TdxParagraphFrameVerticalPositionType);
begin
  AInfo.VerticalPositionType := AValue;
  Options.UseVerticalPositionType := True;
end;

function TdxParagraphFrameFormattingBase.GetWidth: Integer;
begin
  Result := Info.Width;
end;

procedure TdxParagraphFrameFormattingBase.SetWidth(const AValue: Integer);
begin
  if (Info.Width = AValue) and Options.UseWidth then
    Exit;
  SetPropertyValue<Integer>(SetWidthCore, AValue);
end;

procedure TdxParagraphFrameFormattingBase.SetWidthCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: Integer);
begin
  AInfo.Width := AValue;
  Options.UseWidth := True;
end;

function TdxParagraphFrameFormattingBase.GetHeight: Integer;
begin
  Result := Info.Height;
end;

procedure TdxParagraphFrameFormattingBase.SetHeight(const AValue: Integer);
begin
  if (Info.Height = AValue) and Options.UseHeight then
    Exit;
  SetPropertyValue<Integer>(SetHeightCore, AValue);
end;

procedure TdxParagraphFrameFormattingBase.SetHeightCore(const AInfo: TdxParagraphFrameFormattingInfo; const AValue: Integer);
begin
  AInfo.Height := AValue;
  Options.UseHeight := True;
end;

function TdxParagraphFrameFormattingBase.GetDropCap: TdxDropCapLocation;
begin
  Result := Info.DropCap;
end;

procedure TdxParagraphFrameFormattingBase.SetDropCap(const AValue: TdxDropCapLocation);
begin
  if (Info.DropCap = AValue) and Options.UseDropCap then
    Exit;
  SetPropertyValue<TdxDropCapLocation>(SetDropCapCore, AValue);
end;

procedure TdxParagraphFrameFormattingBase.SetDropCapCore(const AInfo: TdxParagraphFrameFormattingInfo;
  const AValue: TdxDropCapLocation);
begin
  AInfo.DropCap := AValue;
  Options.UseDropCap := True;
end;

function TdxParagraphFrameFormattingBase.GetDropCapVerticalHeightInLines: Integer;
begin
  Result := Info.DropCapVerticalHeightInLines;
end;

procedure TdxParagraphFrameFormattingBase.SetDropCapVerticalHeightInLines(const AValue: Integer);
begin
  if (Info.DropCapVerticalHeightInLines = AValue) and Options.UseDropCapVerticalHeightInLines then
    Exit;
  SetPropertyValue<Integer>(SetDropCapVerticalHeightInLinesCore, AValue);
end;

procedure TdxParagraphFrameFormattingBase.SetDropCapVerticalHeightInLinesCore(const AInfo: TdxParagraphFrameFormattingInfo;
  const AValue: Integer);
begin
  AInfo.DropCapVerticalHeightInLines := AValue;
  Options.UseDropCapVerticalHeightInLines := True;
end;

function TdxParagraphFrameFormattingBase.GetLockFrameAnchorToParagraph: Boolean;
begin
  Result := Info.LockFrameAnchorToParagraph;
end;

procedure TdxParagraphFrameFormattingBase.SetLockFrameAnchorToParagraph(const AValue: Boolean);
begin
  if (Info.LockFrameAnchorToParagraph = AValue) and Options.UseLockFrameAnchorToParagraph then
    Exit;
  SetPropertyValue<Boolean>(SetLockFrameAnchorToParagraphCore, AValue);
end;

procedure TdxParagraphFrameFormattingBase.SetLockFrameAnchorToParagraphCore(const AInfo: TdxParagraphFrameFormattingInfo;
  const AValue: Boolean);
begin
  AInfo.LockFrameAnchorToParagraph := AValue;
  Options.UseLockFrameAnchorToParagraph := True;
end;

function TdxParagraphFrameFormattingBase.GetUseValue: TdxParagraphFrameFormattingOptions.TMasks;
begin
  Result := Options.Value;
end;

procedure TdxParagraphFrameFormattingBase.SetUseValue(const AValue: TdxParagraphFrameFormattingOptions.TMasks);
begin
  Options := TdxParagraphFrameFormattingOptions.Create(AValue);
end;

function TdxParagraphFrameFormattingBase.Clone: TdxCloneable;
begin
  Result := TdxParagraphFrameFormattingBase.Create(PieceTable, DocumentModel,
    Info, Options);
end;

procedure TdxParagraphFrameFormattingBase.CopyFrom(const AInfo: TdxParagraphFrameFormattingInfo;
  const AOptions: TdxParagraphFrameFormattingOptions);
begin
  CopyFromCore(AInfo, AOptions);
end;

procedure TdxParagraphFrameFormattingBase.CopyFrom(Source: TdxCloneable);
var
  AParagraphFormatting: TdxParagraphFrameFormattingBase absolute Source;
begin
  CopyFromCore(AParagraphFormatting.Info, AParagraphFormatting.Options);
end;

function TdxParagraphFrameFormattingBase.PropertyEquals(
  const AOther: TdxIndexBasedObject<TdxParagraphFrameFormattingInfo, TdxParagraphFrameFormattingOptions>): Boolean;
begin
  Assert(AOther <> nil, 'other');
  Result := (Options.Value = AOther.Options.Value) and Info.Equals(AOther.Info);
end;

function TdxParagraphFrameFormattingBase.CanSetPropertyValue: Boolean;
begin
  Result := DocumentModel.DocumentCapabilities.ParagraphFormattingAllowed;
end;

{ TdxParagraphFrameFormattingChangeActionsCalculator }

class function TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(
  AChange: TdxParagraphFrameFormattingChangeType): TdxDocumentModelChangeActions;
const
  ParagraphFrameFormattingChangeActionsMap: array[TdxParagraphFrameFormattingChangeType] of TdxDocumentModelChangeActions =
    (
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetRuler],
     [TdxDocumentModelChangeAction.Redraw,
      TdxDocumentModelChangeAction.ResetPrimaryLayout,
      TdxDocumentModelChangeAction.ResetSecondaryLayout,
      TdxDocumentModelChangeAction.ResetSelectionLayout,
      TdxDocumentModelChangeAction.ResetRuler],
     [TdxDocumentModelChangeAction.Redraw,
      TdxDocumentModelChangeAction.ResetPrimaryLayout,
      TdxDocumentModelChangeAction.ResetSecondaryLayout,
      TdxDocumentModelChangeAction.ResetSelectionLayout,
      TdxDocumentModelChangeAction.ResetRuler],
     [TdxDocumentModelChangeAction.Redraw,
      TdxDocumentModelChangeAction.ResetPrimaryLayout,
      TdxDocumentModelChangeAction.ResetSecondaryLayout,
      TdxDocumentModelChangeAction.ResetSelectionLayout,
      TdxDocumentModelChangeAction.ResetRuler],
     [TdxDocumentModelChangeAction.Redraw,
      TdxDocumentModelChangeAction.ResetPrimaryLayout,
      TdxDocumentModelChangeAction.ResetSecondaryLayout,
      TdxDocumentModelChangeAction.ResetSelectionLayout,
      TdxDocumentModelChangeAction.ResetRuler],
     [TdxDocumentModelChangeAction.Redraw,
      TdxDocumentModelChangeAction.ResetPrimaryLayout,
      TdxDocumentModelChangeAction.ResetSecondaryLayout,
      TdxDocumentModelChangeAction.ResetSelectionLayout,
      TdxDocumentModelChangeAction.ResetRuler],
     [TdxDocumentModelChangeAction.Redraw,
      TdxDocumentModelChangeAction.ResetPrimaryLayout,
      TdxDocumentModelChangeAction.ResetSecondaryLayout,
      TdxDocumentModelChangeAction.ResetSelectionLayout,
      TdxDocumentModelChangeAction.ResetRuler],
     [TdxDocumentModelChangeAction.Redraw,
      TdxDocumentModelChangeAction.ResetPrimaryLayout,
      TdxDocumentModelChangeAction.ResetSecondaryLayout,
      TdxDocumentModelChangeAction.ResetSelectionLayout,
      TdxDocumentModelChangeAction.ResetRuler],
     [TdxDocumentModelChangeAction.Redraw,
      TdxDocumentModelChangeAction.ResetPrimaryLayout,
      TdxDocumentModelChangeAction.ResetSecondaryLayout,
      TdxDocumentModelChangeAction.ResetSelectionLayout,
      TdxDocumentModelChangeAction.ResetRuler],
     [TdxDocumentModelChangeAction.Redraw,
      TdxDocumentModelChangeAction.ResetPrimaryLayout,
      TdxDocumentModelChangeAction.ResetSecondaryLayout,
      TdxDocumentModelChangeAction.ResetSelectionLayout,
      TdxDocumentModelChangeAction.ResetRuler],
     [TdxDocumentModelChangeAction.Redraw,
      TdxDocumentModelChangeAction.ResetPrimaryLayout,
      TdxDocumentModelChangeAction.ResetSecondaryLayout,
      TdxDocumentModelChangeAction.ResetSelectionLayout,
      TdxDocumentModelChangeAction.ResetRuler],
     [TdxDocumentModelChangeAction.Redraw,
      TdxDocumentModelChangeAction.ResetPrimaryLayout,
      TdxDocumentModelChangeAction.ResetSecondaryLayout,
      TdxDocumentModelChangeAction.ResetSelectionLayout,
      TdxDocumentModelChangeAction.ResetRuler],
     [TdxDocumentModelChangeAction.Redraw,
      TdxDocumentModelChangeAction.ResetPrimaryLayout,
      TdxDocumentModelChangeAction.ResetSecondaryLayout,
      TdxDocumentModelChangeAction.ResetSelectionLayout,
      TdxDocumentModelChangeAction.ResetRuler],
     [TdxDocumentModelChangeAction.Redraw,
      TdxDocumentModelChangeAction.ResetPrimaryLayout,
      TdxDocumentModelChangeAction.ResetSecondaryLayout,
      TdxDocumentModelChangeAction.ResetSelectionLayout,
      TdxDocumentModelChangeAction.ResetRuler]
    );
begin
  Result := ParagraphFrameFormattingChangeActionsMap[AChange];
end;

{ TdxMergedFrameProperties }

constructor TdxMergedFrameProperties.Create(AInitialProperties: TdxFrameProperties);
var
  AInfo: TdxParagraphFrameFormattingBase;
begin
  AInfo := AInitialProperties.Info;
  inherited Create(AInfo.Info, AInfo.Options);
end;

constructor TdxMergedFrameProperties.Create(AInitial: TdxMergedFrameProperties);
begin
  inherited Create(AInitial.Info, AInitial.Options);
end;

function TdxMergedFrameProperties.GetX: Integer;
begin
  Result := Info.X;
end;

function TdxMergedFrameProperties.GetY: Integer;
begin
  Result := Info.Y;
end;

procedure TdxMergedFrameProperties.Merge(AProperties: TdxFrameProperties);
var
  AInfo: TdxParagraphFrameFormattingBase;
begin
  if AProperties = nil then
    Exit;
  AInfo := AProperties.Info;
  MergeCore(AInfo.Info, AInfo.Options);
end;

procedure TdxMergedFrameProperties.Merge(AProperties: TdxMergedFrameProperties);
begin
  if AProperties = nil then
    Exit;
  MergeCore(AProperties.Info, AProperties.Options);
end;

procedure TdxMergedFrameProperties.MergeCore(AInfo: TdxParagraphFrameFormattingInfo;
  AOptions: TdxParagraphFrameFormattingOptions);
begin
  if not Options.UseHorizontalPosition and AOptions.UseHorizontalPosition then
  begin
    Info.HorizontalPosition := AInfo.HorizontalPosition;
    Options.UseHorizontalPosition := True;
  end;
  if not Options.UseVerticalPosition and AOptions.UseVerticalPosition then
  begin
    Info.VerticalPosition := AInfo.VerticalPosition;
    Options.UseVerticalPosition := True;
  end;
  if not Options.UseHorizontalPadding and AOptions.UseHorizontalPadding then
  begin
    Info.HorizontalPadding := AInfo.HorizontalPadding;
    Options.UseHorizontalPadding := True;
  end;
  if not Options.UseVerticalPadding and AOptions.UseVerticalPadding then
  begin
    Info.VerticalPadding := AInfo.VerticalPadding;
    Options.UseVerticalPadding := True;
  end;
  if not Options.UseHorizontalRule and AOptions.UseHorizontalRule then
  begin
    Info.HorizontalRule := AInfo.HorizontalRule;
    Options.UseHorizontalRule := True;
  end;
  if not Options.UseTextWrapType and AOptions.UseTextWrapType then
  begin
    Info.TextWrapType := AInfo.TextWrapType;
    Options.UseTextWrapType := True;
  end;
  if not Options.UseX and AOptions.UseX then
  begin
    Info.X := AInfo.X;
    Options.UseX := True;
  end;
  if not Options.UseY and AOptions.UseY then
  begin
    Info.Y := AInfo.Y;
    Options.UseY := True;
  end;
  if not Options.UseWidth and AOptions.UseWidth then
  begin
    Info.Width := AInfo.Width;
    Options.UseWidth := True;
  end;
  if not Options.UseHeight and AOptions.UseHeight then
  begin
    Info.Height := AInfo.Height;
    Options.UseHeight := True;
  end;
  if not Options.UseHorizontalPositionAlignment and AOptions.UseHorizontalPositionAlignment then
  begin
    Info.HorizontalPositionAlignment := AInfo.HorizontalPositionAlignment;
    Options.UseHorizontalPositionAlignment := True;
  end;
  if not Options.UseVerticalPositionAlignment and AOptions.UseVerticalPositionAlignment then
  begin
    Info.VerticalPositionAlignment := AInfo.VerticalPositionAlignment;
    Options.UseVerticalPositionAlignment := True;
  end;
  if not Options.UseHorizontalPositionType and AOptions.UseHorizontalPositionType then
  begin
    Info.HorizontalPositionType := AInfo.HorizontalPositionType;
    Options.UseHorizontalPositionType := True;
  end;
  if not Options.UseVerticalPositionType and AOptions.UseVerticalPositionType then
  begin
    Info.VerticalPositionType := AInfo.VerticalPositionType;
    Options.UseVerticalPositionType := True;
  end;
  if not Options.UseDropCap and AOptions.UseDropCap then
  begin
    Info.DropCap := AInfo.DropCap;
    Options.UseDropCap := True;
  end;
  if not Options.UseDropCapVerticalHeightInLines and AOptions.UseDropCapVerticalHeightInLines then
  begin
    Info.DropCapVerticalHeightInLines := AInfo.DropCapVerticalHeightInLines;
    Options.UseDropCapVerticalHeightInLines := True;
  end;
  if not Options.UseLockFrameAnchorToParagraph and AOptions.UseLockFrameAnchorToParagraph then
  begin
    Info.LockFrameAnchorToParagraph := AInfo.LockFrameAnchorToParagraph;
    Options.UseLockFrameAnchorToParagraph := True;
  end;
end;

function TdxMergedFrameProperties.GetHorizontalPositionAlignment: TdxParagraphFrameHorizontalPositionAlignment;
begin
  Result := Info.HorizontalPositionAlignment;
end;

function TdxMergedFrameProperties.GetVerticalPositionAlignment: TdxParagraphFrameVerticalPositionAlignment;
begin
  Result := Info.VerticalPositionAlignment;
end;

function TdxMergedFrameProperties.GetHorizontalPositionType: TdxParagraphFrameHorizontalPositionType;
begin
  Result := Info.HorizontalPositionType;
end;

function TdxMergedFrameProperties.GetVerticalPositionType: TdxParagraphFrameVerticalPositionType;
begin
  Result := Info.VerticalPositionType;
end;

function TdxMergedFrameProperties.GetWidth: Integer;
begin
  Result := Info.Width;
end;

function TdxMergedFrameProperties.GetHeight: Integer;
begin
  Result := Info.Height;
end;

function TdxMergedFrameProperties.CanMerge(AProperties: TdxMergedFrameProperties): Boolean;
begin
  Result :=
    (Info.TextWrapType = AProperties.Info.TextWrapType) and
    (HorizontalPositionType = AProperties.HorizontalPositionType) and
    (VerticalPositionType = AProperties.VerticalPositionType) and
    (Info.VerticalPosition = AProperties.Info.VerticalPosition) and
    (Info.HorizontalPosition = AProperties.Info.HorizontalPosition) and
    (Info.HorizontalPadding = AProperties.Info.HorizontalPadding) and
    (Info.VerticalPadding = AProperties.Info.VerticalPadding) and
    (Width = AProperties.Width) and
    (X = AProperties.X) and (Y = AProperties.Y);

  if Info.HorizontalRule = AProperties.Info.HorizontalRule then
  begin
    if Info.HorizontalRule <> TdxParagraphFrameHorizontalRule.Auto then
      Result := Result and (Height = AProperties.Height);
  end
  else
    Exit(False);

  if not Options.UseX and not AProperties.Options.UseX then
    Result := Result and (HorizontalPositionAlignment = AProperties.HorizontalPositionAlignment);

  if not Options.UseY and not AProperties.Options.UseY then
    Result := Result and (VerticalPositionAlignment = AProperties.VerticalPositionAlignment);
end;

function TdxMergedFrameProperties.Equals(AObj: TObject): Boolean;
var
  AOther: TdxMergedFrameProperties;
begin
  AOther := Safe<TdxMergedFrameProperties>.Cast(AObj);
  if AObj = nil then
    Result := False
  else
    Result := Info.Equals(AOther.Info);
end;

function TdxMergedFrameProperties.GetHashCode: Integer;
begin
  Result := Info.GetHashCode;
end;

function TdxMergedFrameProperties.IsParagraphFrame: Boolean;
begin
  Result := (Width <> 0) or
    ((Height <> 0) and (Info.HorizontalRule <> TdxParagraphFrameHorizontalRule.Auto)) or
    (X <> 0) or (Y <> 0) or (HorizontalPositionType <> TdxParagraphFrameHorizontalPositionType.Column) or
    (VerticalPositionType = TdxParagraphFrameVerticalPositionType.Paragraph) or
    (Options.UseHorizontalPositionAlignment and (HorizontalPositionAlignment <> TdxParagraphFrameHorizontalPositionAlignment.Left)) or
    (Options.UseVerticalPositionAlignment and (VerticalPositionAlignment <> TdxParagraphFrameVerticalPositionAlignment.Inline)) or
    (Info.TextWrapType <> TdxParagraphFrameTextWrapType.Auto);
end;

{ TdxFrameProperties }

constructor TdxFrameProperties.Create(const AOwner: IdxParagraphPropertiesContainer);
begin
  Assert(AOwner <> nil);
  inherited Create(AOwner.PieceTable);
  FOwner := AOwner;
end;

class procedure TdxFrameProperties.ApplyPropertiesDiff(ATarget: TdxFrameProperties;
  ATargetMergedInfo: TdxParagraphFrameFormattingInfo; ASourceMergedInfo: TdxParagraphFrameFormattingInfo);
begin
  if ATargetMergedInfo.HorizontalPosition <> ASourceMergedInfo.HorizontalPosition then
    ATarget.HorizontalPosition := ASourceMergedInfo.HorizontalPosition;

  if ATargetMergedInfo.VerticalPosition <> ASourceMergedInfo.VerticalPosition then
    ATarget.VerticalPosition := ASourceMergedInfo.VerticalPosition;

  if ATargetMergedInfo.HorizontalPadding <> ASourceMergedInfo.HorizontalPadding then
    ATarget.HorizontalPadding := ASourceMergedInfo.HorizontalPadding;

  if ATargetMergedInfo.VerticalPadding <> ASourceMergedInfo.VerticalPadding then
    ATarget.VerticalPadding := ASourceMergedInfo.VerticalPadding;

  if ATargetMergedInfo.HorizontalRule <> ASourceMergedInfo.HorizontalRule then
    ATarget.HorizontalRule := ASourceMergedInfo.HorizontalRule;

  if ATargetMergedInfo.TextWrapType <> ASourceMergedInfo.TextWrapType then
    ATarget.TextWrapType := ASourceMergedInfo.TextWrapType;

  if ATargetMergedInfo.X <> ASourceMergedInfo.X then
    ATarget.X := ASourceMergedInfo.X;

  if ATargetMergedInfo.Y <> ASourceMergedInfo.Y then
    ATarget.Y := ASourceMergedInfo.Y;

  if ATargetMergedInfo.Width <> ASourceMergedInfo.Width then
    ATarget.Width := ASourceMergedInfo.Width;

  if ATargetMergedInfo.Height <> ASourceMergedInfo.Height then
    ATarget.Height := ASourceMergedInfo.Height;

  if ATargetMergedInfo.HorizontalPositionAlignment <> ASourceMergedInfo.HorizontalPositionAlignment then
    ATarget.HorizontalPositionAlignment := ASourceMergedInfo.HorizontalPositionAlignment;

  if ATargetMergedInfo.VerticalPositionAlignment <> ASourceMergedInfo.VerticalPositionAlignment then
    ATarget.VerticalPositionAlignment := ASourceMergedInfo.VerticalPositionAlignment;

  if ATargetMergedInfo.HorizontalPositionType <> ASourceMergedInfo.HorizontalPositionType then
    ATarget.HorizontalPositionType := ASourceMergedInfo.HorizontalPositionType;

  if ATargetMergedInfo.VerticalPositionType <> ASourceMergedInfo.VerticalPositionType then
    ATarget.VerticalPositionType := ASourceMergedInfo.VerticalPositionType;

  if ATargetMergedInfo.DropCap <> ASourceMergedInfo.DropCap then
    ATarget.DropCap := ASourceMergedInfo.DropCap;

  if ATargetMergedInfo.DropCapVerticalHeightInLines <> ASourceMergedInfo.DropCapVerticalHeightInLines then
    ATarget.DropCapVerticalHeightInLines := ASourceMergedInfo.DropCapVerticalHeightInLines;

  if ATargetMergedInfo.LockFrameAnchorToParagraph <> ASourceMergedInfo.LockFrameAnchorToParagraph then
    ATarget.LockFrameAnchorToParagraph := ASourceMergedInfo.LockFrameAnchorToParagraph;
end;

function TdxFrameProperties.GetVerticalPosition: Integer;
begin
  Result := Info.VerticalPosition;
end;

procedure TdxFrameProperties.SetVerticalPosition(const AValue: Integer);
begin
  if (Info.VerticalPosition = AValue) and UseVerticalPosition then
    Exit;
  SetPropertyValue<Integer>(SetVerticalPositionCore, AValue);
end;

function TdxFrameProperties.GetUseVerticalPosition: Boolean;
begin
  Result := Info.Options.UseVerticalPosition;
end;

function TdxFrameProperties.SetVerticalPositionCore(const AInfo: TdxParagraphFrameFormattingBase;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.VerticalPosition := AValue;
  Result := TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFrameFormattingChangeType.VerticalPosition);
end;

function TdxFrameProperties.GetHorizontalPosition: Integer;
begin
  Result := Info.HorizontalPosition;
end;

procedure TdxFrameProperties.SetHorizontalPosition(const AValue: Integer);
begin
  if (Info.HorizontalPosition = AValue) and UseHorizontalPosition then
    Exit;
  SetPropertyValue<Integer>(SetHorizontalPositionCore, AValue);
end;

function TdxFrameProperties.GetUseHorizontalPosition: Boolean;
begin
  Result := Info.Options.UseHorizontalPosition;
end;

function TdxFrameProperties.SetHorizontalPositionCore(const AInfo: TdxParagraphFrameFormattingBase;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.HorizontalPosition := AValue;
  Result := TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFrameFormattingChangeType.HorizontalPosition);
end;

function TdxFrameProperties.GetHorizontalPadding: Integer;
begin
  Result := Info.HorizontalPadding;
end;

procedure TdxFrameProperties.SetHorizontalPadding(const AValue: Integer);
begin
  if (Info.HorizontalPadding = AValue) and UseHorizontalPadding then
    Exit;
  SetPropertyValue<Integer>(SetHorizontalPaddingCore, AValue);
end;

function TdxFrameProperties.GetUseHorizontalPadding: Boolean;
begin
  Result := Info.Options.UseHorizontalPadding;
end;

function TdxFrameProperties.SetHorizontalPaddingCore(const AInfo: TdxParagraphFrameFormattingBase;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.HorizontalPadding := AValue;
  Result := TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFrameFormattingChangeType.HorizontalPadding);
end;

function TdxFrameProperties.GetVerticalPadding: Integer;
begin
  Result := Info.VerticalPadding;
end;

procedure TdxFrameProperties.SetVerticalPadding(const AValue: Integer);
begin
  if (Info.VerticalPadding = AValue) and UseVerticalPadding then
    Exit;
  SetPropertyValue<Integer>(SetVerticalPaddingCore, AValue);
end;

function TdxFrameProperties.GetUseVerticalPadding: Boolean;
begin
  Result := Info.Options.UseVerticalPadding;
end;

function TdxFrameProperties.SetVerticalPaddingCore(const AInfo: TdxParagraphFrameFormattingBase;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.VerticalPadding := AValue;
  Result := TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFrameFormattingChangeType.VerticalPadding);
end;

function TdxFrameProperties.GetHorizontalRule: TdxParagraphFrameHorizontalRule;
begin
  Result := Info.HorizontalRule;
end;

procedure TdxFrameProperties.SetHorizontalRule(const AValue: TdxParagraphFrameHorizontalRule);
begin
  if (Info.HorizontalRule = AValue) and UseHorizontalRule then
    Exit;
  SetPropertyValue<TdxParagraphFrameHorizontalRule>(SetHorizontalRuleCore, AValue);
end;

function TdxFrameProperties.GetUseHorizontalRule: Boolean;
begin
  Result := Info.Options.UseHorizontalRule;
end;

function TdxFrameProperties.SetHorizontalRuleCore(const AInfo: TdxParagraphFrameFormattingBase;
  const AValue: TdxParagraphFrameHorizontalRule): TdxDocumentModelChangeActions;
begin
  AInfo.HorizontalRule := AValue;
  Result := TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFrameFormattingChangeType.HorizontalRule);
end;

function TdxFrameProperties.GetTextWrapType: TdxParagraphFrameTextWrapType;
begin
  Result := Info.TextWrapType;
end;

procedure TdxFrameProperties.SetTextWrapType(const AValue: TdxParagraphFrameTextWrapType);
begin
  if (Info.TextWrapType = AValue) and UseTextWrapType then
    Exit;
  SetPropertyValue<TdxParagraphFrameTextWrapType>(SetTextWrapTypeCore, AValue);
end;

function TdxFrameProperties.GetUseTextWrapType: Boolean;
begin
  Result := Info.Options.UseTextWrapType;
end;

function TdxFrameProperties.SetTextWrapTypeCore(const AInfo: TdxParagraphFrameFormattingBase;
  const AValue: TdxParagraphFrameTextWrapType): TdxDocumentModelChangeActions;
begin
  AInfo.TextWrapType := AValue;
  Result := TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFrameFormattingChangeType.TextWrapType);
end;

function TdxFrameProperties.GetX: Integer;
begin
  Result := Info.X;
end;

procedure TdxFrameProperties.SetX(const AValue: Integer);
begin
  if (Info.X = AValue) and UseX then
    Exit;
  SetPropertyValue<Integer>(SetXCore, AValue);
end;

function TdxFrameProperties.GetUseX: Boolean;
begin
  Result := Info.Options.UseX;
end;

function TdxFrameProperties.SetXCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.X := AValue;
  Result := TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFrameFormattingChangeType.X);
end;

function TdxFrameProperties.GetY: Integer;
begin
  Result := Info.Y;
end;

procedure TdxFrameProperties.SetY(const AValue: Integer);
begin
  if (Info.Y = AValue) and UseY then
    Exit;
  SetPropertyValue<Integer>(SetYCore, AValue);
end;

function TdxFrameProperties.GetUseY: Boolean;
begin
  Result := Info.Options.UseY;
end;

function TdxFrameProperties.SetYCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.Y := AValue;
  Result := TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFrameFormattingChangeType.Y);
end;

function TdxFrameProperties.GetHorizontalPositionAlignment: TdxParagraphFrameHorizontalPositionAlignment;
begin
  Result := Info.HorizontalPositionAlignment;
end;

procedure TdxFrameProperties.SetHorizontalPositionAlignment(const AValue: TdxParagraphFrameHorizontalPositionAlignment);
begin
  if (Info.HorizontalPositionAlignment = AValue) and UseHorizontalPositionAlignment then
    Exit;
  SetPropertyValue<TdxParagraphFrameHorizontalPositionAlignment>(SetHorizontalPositionAlignmentCore, AValue);
end;

function TdxFrameProperties.GetUseHorizontalPositionAlignment: Boolean;
begin
  Result := Info.Options.UseHorizontalPositionAlignment;
end;

function TdxFrameProperties.SetHorizontalPositionAlignmentCore(const AInfo: TdxParagraphFrameFormattingBase;
  const AValue: TdxParagraphFrameHorizontalPositionAlignment): TdxDocumentModelChangeActions;
begin
  AInfo.HorizontalPositionAlignment := AValue;
  Result := TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFrameFormattingChangeType.HorizontalPositionAlignment);
end;

function TdxFrameProperties.GetVerticalPositionAlignment: TdxParagraphFrameVerticalPositionAlignment;
begin
  Result := Info.VerticalPositionAlignment;
end;

procedure TdxFrameProperties.SetVerticalPositionAlignment(const AValue: TdxParagraphFrameVerticalPositionAlignment);
begin
  if (Info.VerticalPositionAlignment = AValue) and UseVerticalPositionAlignment then
    Exit;
  SetPropertyValue<TdxParagraphFrameVerticalPositionAlignment>(SetVerticalPositionAlignmentCore, AValue);
end;

function TdxFrameProperties.GetUseVerticalPositionAlignment: Boolean;
begin
  Result := Info.Options.UseVerticalPositionAlignment;
end;

function TdxFrameProperties.SetVerticalPositionAlignmentCore(const AInfo: TdxParagraphFrameFormattingBase;
  const AValue: TdxParagraphFrameVerticalPositionAlignment): TdxDocumentModelChangeActions;
begin
  AInfo.VerticalPositionAlignment := AValue;
  Result := TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFrameFormattingChangeType.VerticalPositionAlignment);
end;

function TdxFrameProperties.GetHorizontalPositionType: TdxParagraphFrameHorizontalPositionType;
begin
  Result := Info.HorizontalPositionType;
end;

procedure TdxFrameProperties.SetHorizontalPositionType(const AValue: TdxParagraphFrameHorizontalPositionType);
begin
  if (Info.HorizontalPositionType = AValue) and UseHorizontalPositionType then
    Exit;
  SetPropertyValue<TdxParagraphFrameHorizontalPositionType>(SetHorizontalPositionTypeCore, AValue);
end;

function TdxFrameProperties.GetUseHorizontalPositionType: Boolean;
begin
  Result := Info.Options.UseHorizontalPositionType;
end;

function TdxFrameProperties.SetHorizontalPositionTypeCore(const AInfo: TdxParagraphFrameFormattingBase;
  const AValue: TdxParagraphFrameHorizontalPositionType): TdxDocumentModelChangeActions;
begin
  AInfo.HorizontalPositionType := AValue;
  Result := TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFrameFormattingChangeType.HorizontalPositionType);
end;

function TdxFrameProperties.GetVerticalPositionType: TdxParagraphFrameVerticalPositionType;
begin
  Result := Info.VerticalPositionType;
end;

procedure TdxFrameProperties.SetVerticalPositionType(const AValue: TdxParagraphFrameVerticalPositionType);
begin
  if (Info.VerticalPositionType = AValue) and UseVerticalPositionType then
    Exit;
  SetPropertyValue<TdxParagraphFrameVerticalPositionType>(SetVerticalPositionTypeCore, AValue);
end;

function TdxFrameProperties.GetUseVerticalPositionType: Boolean;
begin
  Result := Info.Options.UseVerticalPositionType;
end;

function TdxFrameProperties.SetVerticalPositionTypeCore(const AInfo: TdxParagraphFrameFormattingBase;
  const AValue: TdxParagraphFrameVerticalPositionType): TdxDocumentModelChangeActions;
begin
  AInfo.VerticalPositionType := AValue;
  Result := TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFrameFormattingChangeType.VerticalPositionType);
end;

function TdxFrameProperties.GetWidth: Integer;
begin
  Result := Info.Width;
end;

procedure TdxFrameProperties.SetWidth(const AValue: Integer);
begin
  if (Info.Width = AValue) and UseWidth then
    Exit;
  SetPropertyValue<Integer>(SetWidthCore, AValue);
end;

function TdxFrameProperties.GetUseWidth: Boolean;
begin
  Result := Info.Options.UseWidth;
end;

function TdxFrameProperties.SetWidthCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.Width := AValue;
  Result := TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFrameFormattingChangeType.Width);
end;

function TdxFrameProperties.GetHeight: Integer;
begin
  Result := Info.Height;
end;

procedure TdxFrameProperties.SetHeight(const AValue: Integer);
begin
  if (Info.Height = AValue) and UseHeight then
    Exit;
  SetPropertyValue<Integer>(SetHeightCore, AValue);
end;

function TdxFrameProperties.GetUseHeight: Boolean;
begin
  Result := Info.Options.UseHeight;
end;

function TdxFrameProperties.SetHeightCore(const AInfo: TdxParagraphFrameFormattingBase; const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.Height := AValue;
  Result := TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFrameFormattingChangeType.Height);
end;

function TdxFrameProperties.GetLockFrameAnchorToParagraph: Boolean;
begin
  Result := Info.LockFrameAnchorToParagraph;
end;

procedure TdxFrameProperties.SetLockFrameAnchorToParagraph(const AValue: Boolean);
begin
  if (Info.LockFrameAnchorToParagraph = AValue) and UseLockFrameAnchorToParagraph then
    Exit;
  SetPropertyValue<Boolean>(SetLockFrameAnchorToParagraphCore, AValue);
end;

function TdxFrameProperties.GetUseLockFrameAnchorToParagraph: Boolean;
begin
  Result := Info.Options.UseLockFrameAnchorToParagraph;
end;

function TdxFrameProperties.SetLockFrameAnchorToParagraphCore(const AInfo: TdxParagraphFrameFormattingBase;
  const AValue: Boolean): TdxDocumentModelChangeActions;
begin
  AInfo.LockFrameAnchorToParagraph := AValue;
  Result := TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFrameFormattingChangeType.LockFrameAnchorToParagraph);
end;

function TdxFrameProperties.GetDropCap: TdxDropCapLocation;
begin
  Result := Info.DropCap;
end;

procedure TdxFrameProperties.SetDropCap(const AValue: TdxDropCapLocation);
begin
  if (Info.DropCap = AValue) and UseDropCap then
    Exit;
  SetPropertyValue<TdxDropCapLocation>(SetDropCapCore, AValue);
end;

function TdxFrameProperties.GetUseDropCap: Boolean;
begin
  Result := Info.Options.UseDropCap;
end;

function TdxFrameProperties.SetDropCapCore(const AInfo: TdxParagraphFrameFormattingBase;
  const AValue: TdxDropCapLocation): TdxDocumentModelChangeActions;
begin
  AInfo.DropCap := AValue;
  Result := TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFrameFormattingChangeType.DropCap);
end;

function TdxFrameProperties.GetDropCapVerticalHeightInLines: Integer;
begin
  Result := Info.DropCapVerticalHeightInLines;
end;

procedure TdxFrameProperties.SetDropCapVerticalHeightInLines(const AValue: Integer);
begin
  if (Info.DropCapVerticalHeightInLines = AValue) and UseDropCapVerticalHeightInLines then
    Exit;
  SetPropertyValue<Integer>(SetDropCapVerticalHeightInLinesCore, AValue);
end;

function TdxFrameProperties.GetUseDropCapVerticalHeightInLines: Boolean;
begin
  Result := Info.Options.UseDropCapVerticalHeightInLines;
end;

function TdxFrameProperties.SetDropCapVerticalHeightInLinesCore(const AInfo: TdxParagraphFrameFormattingBase;
  const AValue: Integer): TdxDocumentModelChangeActions;
begin
  AInfo.DropCapVerticalHeightInLines := AValue;
  Result := TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFrameFormattingChangeType.DropCapVerticalHeightInLines);
end;

function TdxFrameProperties.GetCache(const ADocumentModel: TdxCustomDocumentModel): TdxUniqueItemsCache<TdxParagraphFrameFormattingBase>;
begin
  Result := TdxSimpleDocumentCache(ADocumentModel.Cache).ParagraphFrameFormattingCache;
end;

function TdxFrameProperties.UseVal(AMask: TdxParagraphFrameFormattingOptions.TMask): Boolean;
begin
  Result := AMask in Info.Options.Value;
end;

procedure TdxFrameProperties.Reset;
var
  AInfo, AEmptyInfo: TdxParagraphFrameFormattingBase;
  AIsDeferred: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferred);
  AEmptyInfo := GetCache(DocumentModel)[TdxParagraphFrameFormattingCache.EmptyParagraphFrameFormattingIndex];
  AInfo.ReplaceInfo(AEmptyInfo.Info, AEmptyInfo.Options);
  ReplaceInfo(AInfo, GetBatchUpdateChangeActions);
  if not AIsDeferred then
    AInfo.Free;
end;

function TdxFrameProperties.Equals(AObj: TObject): Boolean;
var
  AOther: TdxFrameProperties;
begin
  AOther := Safe<TdxFrameProperties>.Cast(AObj);
  if AObj = nil then
    Exit(False);
  if DocumentModel = AOther.DocumentModel then
    Result := Index = AOther.Index
  else
    Result := Info.Equals(AOther.Info);
end;

procedure TdxFrameProperties.ResetUse(const AMask: TdxParagraphFrameFormattingOptions.TMasks);
var
  AInfo: TdxParagraphFrameFormattingBase;
  AOptions: TdxParagraphFrameFormattingOptions;
  AIsDeferred: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferred);
  AOptions := AInfo.Options;
  AOptions.Value := AOptions.Value - AMask;
  AInfo.Options := AOptions;
  ReplaceInfo(AInfo, GetBatchUpdateChangeActions);
  if not AIsDeferred then
    AInfo.Free;
end;

procedure TdxFrameProperties.ResetAllUse;
var
  AInfo: TdxParagraphFrameFormattingBase;
  AIsDeferred: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferred);
  AInfo.Options := TdxParagraphFrameFormattingOptions.Create;
  ReplaceInfo(AInfo, GetBatchUpdateChangeActions);
  if not AIsDeferred then
    AInfo.Free;
end;

function TdxFrameProperties.GetHashCode: Integer;
begin
  Result := Index;
end;

procedure TdxFrameProperties.MakeDefault;
begin
  ChangeIndexCore(TdxParagraphFrameFormattingCache.DefaultParagraphFrameFormattingIndex, GetBatchUpdateChangeActions);
end;

function TdxFrameProperties.CanMerge(AProperties: TdxFrameProperties): Boolean;
begin
  Result :=
    (TextWrapType = AProperties.TextWrapType) and
    (HorizontalPositionType = AProperties.HorizontalPositionType) and
    (VerticalPositionType = AProperties.VerticalPositionType) and
    (VerticalPosition = AProperties.VerticalPosition) and
    (HorizontalPosition = AProperties.HorizontalPosition) and
    (HorizontalPadding = AProperties.HorizontalPadding) and
    (VerticalPadding = AProperties.VerticalPadding) and
    (Width = AProperties.Width) and
    (X = AProperties.X) and (Y = AProperties.Y);

  if HorizontalRule = AProperties.HorizontalRule then
  begin
    if HorizontalRule <> TdxParagraphFrameHorizontalRule.Auto then
      Result := Result and (Height = AProperties.Height);
  end
  else
    Exit(False);

  if not UseX and not AProperties.UseX then
    Result := Result and (HorizontalPositionAlignment = AProperties.HorizontalPositionAlignment);

  if not UseY and not AProperties.UseY then
    Result := Result and (VerticalPositionAlignment = AProperties.VerticalPositionAlignment);
end;

procedure TdxFrameProperties.Merge(AProperties: TdxFrameProperties);
var
  AMergedFrameProperties: TdxMergedFrameProperties;
begin
 AMergedFrameProperties := TdxMergedFrameProperties.Create(Self);
 try
   AMergedFrameProperties.Merge(AProperties);
   CopyFrom(AMergedFrameProperties);
 finally
   AMergedFrameProperties.Free;
 end;
end;

function TdxFrameProperties.GetBatchUpdateChangeActions: TdxDocumentModelChangeActions;
begin
  Result := TdxParagraphFrameFormattingChangeActionsCalculator.CalculateChangeActions(TdxParagraphFrameFormattingChangeType.BatchUpdate);
end;

procedure TdxFrameProperties.OnIndexChanged;
begin
  inherited OnIndexChanged;
  FOwner.OnParagraphPropertiesChanged;
end;

function TdxFrameProperties.CreateIndexChangedHistoryItem: TdxIndexChangedHistoryItemCore;
begin
  Result := TdxIndexChangedHistoryItem.Create(DocumentModel.MainPart, Self);
end;

procedure TdxFrameProperties.CopyFrom(AFrameProperties: TdxMergedProperties<TdxParagraphFrameFormattingInfo, TdxParagraphFrameFormattingOptions>);
var
  AInfo: TdxParagraphFrameFormattingBase;
  AIsDeferred: Boolean;
begin
  AInfo := GetInfoForModification(AIsDeferred);
  AInfo.CopyFromCore(AFrameProperties.Info, AFrameProperties.Options);
  ReplaceInfo(AInfo, GetBatchUpdateChangeActions);
  if not AIsDeferred then
    AInfo.Free;
end;

{$IFNDEF DELPHI17}
procedure TdxFrameProperties.CopyFrom(const Source: TdxParagraphFrameFormattingBase);
begin
  inherited CopyFrom(Source);
end;

procedure TdxFrameProperties.CopyFrom(
  const Source: TdxUndoableIndexBasedObject<TdxParagraphFrameFormattingBase>);
begin
  inherited CopyFrom(Source);
end;
{$ENDIF}

function TdxFrameProperties.GetObtainAffectedRangeListener: IdxObtainAffectedRangeListener;
begin
  if not Supports(FOwner, IdxObtainAffectedRangeListener, Result) then
    Result := nil;
end;

end.
