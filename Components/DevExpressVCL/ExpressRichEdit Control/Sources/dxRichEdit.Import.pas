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

unit dxRichEdit.Import;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, dxCoreClasses, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreGraphics,

  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.ProtectionFormatting,
  dxRichEdit.DocumentModel.Simple,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.ProgressIndication,
  dxRichEdit.Options;

type
  { TdxImportBookmarkInfoCore }

  TdxImportBookmarkInfoCore = class
  private
    FEnd: Integer;
    FStart: Integer;
  public
    constructor Create;
    function Validate(APieceTable: TdxPieceTable): Boolean; virtual;

    property &End: Integer read FEnd write FEnd;
    property Start: Integer read FStart write FStart;
  end;

  { TdxImportBookmarkInfo }

  TdxImportBookmarkInfo = class(TdxImportBookmarkInfoCore)
  strict private
    FName: string;
  public
    function Validate(APieceTable: TdxPieceTable): Boolean; override;

    property Name: string read FName write FName;
  end;

  TdxImportBookmarkInfos = TdxNamedObjectDictionary<TdxImportBookmarkInfo>;

  { ImportRangePermissionInfo }

  TdxImportRangePermissionInfo = class(TdxImportBookmarkInfoCore)
  strict private
    FPermissionInfo: TdxRangePermissionInfo;
  public
    constructor Create;
    destructor Destroy; override;
    function Validate(APieceTable: TdxPieceTable): Boolean; override;

    property PermissionInfo: TdxRangePermissionInfo read FPermissionInfo;
  end;

  TdxImportRangePermissionInfos = TdxNamedObjectDictionary<TdxImportRangePermissionInfo>;

  { TdxImportCommentInfo }

  TdxImportCommentInfo = class(TdxImportBookmarkInfoCore);

  { TdxImportPieceTableInfoBase }

  TdxImportPieceTableInfoBase = class
  private
    FPieceTable: TdxSimplePieceTable;
    FBookmarks: TdxImportBookmarkInfos;
    FRangePermissions: TdxImportRangePermissionInfos;
  public
    constructor Create(APieceTable: TdxSimplePieceTable); overload; virtual;
    destructor Destroy; override;

    property PieceTable: TdxSimplePieceTable read FPieceTable;
    property Bookmarks: TdxImportBookmarkInfos read FBookmarks;
    property RangePermissions: TdxImportRangePermissionInfos read FRangePermissions;
  end;

  { TdxImportInputPosition }

  TdxImportInputPosition = class(TdxInputPosition)
  strict private
    FIsContainsParagraphFrame: Boolean;
    FParagraphMarkCharacterFormatting: TdxCharacterFormattingBase;
    FParagraphFormatting: TdxParagraphFormattingBase;
    FParagraphFrameFormatting: TdxParagraphFrameFormattingBase;
    FParagraphTabs: TdxTabFormattingInfo;
    FParagraphStyleIndex: Integer;
    FParagraphMarkCharacterStyleIndex: Integer;
    FTableCellStyleIndex: Integer;
  public
    constructor Create(APieceTable: TdxSimplePieceTable); override;
    destructor Destroy; override;

    property IsContainsParagraphFrame: Boolean read FIsContainsParagraphFrame write FIsContainsParagraphFrame;
    property ParagraphMarkCharacterFormatting: TdxCharacterFormattingBase read FParagraphMarkCharacterFormatting;
    property ParagraphFormatting: TdxParagraphFormattingBase read FParagraphFormatting;
    property ParagraphFrameFormatting: TdxParagraphFrameFormattingBase read FParagraphFrameFormatting;
    property ParagraphTabs: TdxTabFormattingInfo read FParagraphTabs;
    property ParagraphMarkCharacterStyleIndex: Integer read FParagraphMarkCharacterStyleIndex write FParagraphMarkCharacterStyleIndex;
    property ParagraphStyleIndex: Integer read FParagraphStyleIndex write FParagraphStyleIndex;
    property TableCellStyleIndex: Integer read FTableCellStyleIndex write FTableCellStyleIndex;
  end;

  { TdxImportFieldInfo }

  TdxImportFieldInfo = class
  strict private
    FCodeEndIndex: TdxRunIndex;
    FCodeStartIndex: TdxRunIndex;
    FDisableUpdate: Boolean;
    FField: TdxField;
    FHideByParent: Boolean;
    FIsFieldUsed: Boolean;
    FLocked: Boolean;
    FResultEndIndex: TdxRunIndex;
    procedure SetField(const Value: TdxField);
  public
    constructor Create(APieceTable: TdxCustomPieceTable); virtual;
    destructor Destroy; override;

    property CodeEndIndex: TdxRunIndex read FCodeEndIndex write FCodeEndIndex;
    property CodeStartIndex: TdxRunIndex read FCodeStartIndex write FCodeStartIndex;
    property DisableUpdate: Boolean read FDisableUpdate write FDisableUpdate;
    property Field: TdxField read FField write SetField;
    property HideByParent: Boolean read FHideByParent write FHideByParent;
    property IsFieldUsed: Boolean read FIsFieldUsed write FIsFieldUsed;
    property Locked: Boolean read FLocked write FLocked;
    property ResultEndIndex: TdxRunIndex read FResultEndIndex write FResultEndIndex;
  end;

  { TdxImportPieceTableInfo }

  TdxImportPieceTableInfo = class(TdxImportPieceTableInfoBase)
  strict private
    FTableStack: TdxObjectStack<TdxTable>;
    FFieldInfoStack: TdxObjectStack<TdxImportFieldInfo>;
    FPosition: TdxImportInputPosition;
  public
    constructor Create(APieceTable: TdxSimplePieceTable); override;
    destructor Destroy; override;

    property Position: TdxImportInputPosition read FPosition;
    property FieldInfoStack: TdxObjectStack<TdxImportFieldInfo> read FFieldInfoStack;
    property TableStack: TdxObjectStack<TdxTable> read FTableStack;
  end;

  { TdxShadingHelper }

  TdxShadingHelper = class
  strict private
    class var
      FPatternMultipliers: TdxEnumeratedDictionary<TdxShadingPattern, Integer>;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    class function CreatePatternMultiplierDictionary: TdxEnumeratedDictionary<TdxShadingPattern, Integer>; static;
  public
    class function GetActualBackColor(AFill: TdxAlphaColor; APatternColor: TdxAlphaColor; APattern: TdxShadingPattern): TdxAlphaColor; static;
  end;

implementation

uses
  RTLConsts, Math,
  dxRichEdit.Strs,
  dxRichEdit.Utils.Exceptions;

{ TdxImportBookmarkInfoCore }

constructor TdxImportBookmarkInfoCore.Create;
begin
  inherited Create;
  FStart := -1;
  FEnd := -1;
end;

function TdxImportBookmarkInfoCore.Validate(APieceTable: TdxPieceTable): Boolean;
var
  ATemp, AMax: Integer;
begin
  Result := (Start >= 0) and (&End >= 0);
  if Result then
  begin
    if Start > &End then
    begin
      ATemp := Start;
      FStart := FEnd;
      FEnd := ATemp;
    end;
    AMax := APieceTable.DocumentEndLogPosition + 1;
    Start := Min(Start, AMax);
    &End := Min(&End, AMax);
  end;
end;

{ TdxImportRangePermissionInfo }

constructor TdxImportRangePermissionInfo.Create;
begin
  inherited Create;
  FPermissionInfo := TdxRangePermissionInfo.Create;
end;

destructor TdxImportRangePermissionInfo.Destroy;
begin
  FPermissionInfo.Free;
  inherited Destroy;
end;

function TdxImportRangePermissionInfo.Validate(APieceTable: TdxPieceTable): Boolean;
begin
  if (PermissionInfo.UserName = '') and (PermissionInfo.Group = '') then
    Exit(False);
  if &End - Start <= 0 then
    Exit(False);
  Result := inherited Validate(APieceTable);
end;

{ TdxImportBookmarkInfo }

function TdxImportBookmarkInfo.Validate(APieceTable: TdxPieceTable): Boolean;
begin
  Result := (Name <> '') and (APieceTable.Bookmarks.FindByName(Name) = nil) and inherited Validate(APieceTable);
end;

{ TdxImportPieceTableInfoBase }

constructor TdxImportPieceTableInfoBase.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create;
  FPieceTable := APieceTable;
  FBookmarks := TdxImportBookmarkInfos.Create(True);
  FRangePermissions := TdxImportRangePermissionInfos.Create(True);
end;

destructor TdxImportPieceTableInfoBase.Destroy;
begin
  FreeAndNil(FRangePermissions);
  FreeAndNil(FBookmarks);
  inherited Destroy;
end;

{ TdxImportInputPosition }

constructor TdxImportInputPosition.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create(APieceTable);
  FParagraphMarkCharacterFormatting := APieceTable.DocumentModel.CreateEmptyCharacterFormatting;
  FParagraphFormatting := APieceTable.DocumentModel.CreateEmptyParagraphFormatting;
  FParagraphFrameFormatting := TdxParagraphFrameFormattingBase.Create(APieceTable, APieceTable.DocumentModel,
    TdxDocumentModel(APieceTable.DocumentModel).Cache.ParagraphFrameFormattingCache.EmptyParagraphFrameFormattingInfo,
    TdxParagraphFrameFormattingOptions.EmptyParagraphFrameFormattingOption);
  FParagraphTabs := TdxTabFormattingInfo.Create;
end;

destructor TdxImportInputPosition.Destroy;
begin
  FParagraphTabs.Free;
  FParagraphMarkCharacterFormatting.Free;
  FParagraphFormatting.Free;
  FParagraphFrameFormatting.Free;
  inherited Destroy;
end;

{ TdxImportFieldInfo }

constructor TdxImportFieldInfo.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create;
  FField := TdxField.Create(APieceTable);
end;

destructor TdxImportFieldInfo.Destroy;
begin
  if not FIsFieldUsed then
    FreeAndNil(FField);
  inherited Destroy;
end;

procedure TdxImportFieldInfo.SetField(const Value: TdxField);
begin
  if Field = Value then
    Exit;
  if not FIsFieldUsed then
    FreeAndNil(FField);
  FIsFieldUsed := True;
  FField := Value;
end;

{ TdxImportPieceTableInfo }

constructor TdxImportPieceTableInfo.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create(APieceTable);
  FPosition := TdxImportInputPosition.Create(APieceTable);
  FFieldInfoStack := TdxObjectStack<TdxImportFieldInfo>.Create(True);
  FTableStack := TdxObjectStack<TdxTable>.Create(False);
end;

destructor TdxImportPieceTableInfo.Destroy;
begin
  FTableStack.Free;
  FFieldInfoStack.Free;
  FPosition.Free;
  inherited Destroy;
end;

{ TdxShadingHelper }

class constructor TdxShadingHelper.Initialize;
begin
  FPatternMultipliers := CreatePatternMultiplierDictionary;
end;

class destructor TdxShadingHelper.Finalize;
begin
  FreeAndNil(FPatternMultipliers);
end;

class function TdxShadingHelper.CreatePatternMultiplierDictionary: TdxEnumeratedDictionary<TdxShadingPattern, Integer>;
begin
  Result := TdxEnumeratedDictionary<TdxShadingPattern, Integer>.Create;
  Result.Add(TdxShadingPattern.Clear, 0);
  Result.Add(TdxShadingPattern.Pct5, 50);
  Result.Add(TdxShadingPattern.Pct10, 100);
  Result.Add(TdxShadingPattern.Pct12, 125);
  Result.Add(TdxShadingPattern.Pct15, 150);
  Result.Add(TdxShadingPattern.Pct20, 200);
  Result.Add(TdxShadingPattern.Pct25, 250);
  Result.Add(TdxShadingPattern.Pct30, 300);
  Result.Add(TdxShadingPattern.Pct35, 350);
  Result.Add(TdxShadingPattern.Pct37, 375);
  Result.Add(TdxShadingPattern.Pct40, 400);
  Result.Add(TdxShadingPattern.Pct45, 450);
  Result.Add(TdxShadingPattern.Pct50, 500);
  Result.Add(TdxShadingPattern.Pct55, 550);
  Result.Add(TdxShadingPattern.Pct60, 600);
  Result.Add(TdxShadingPattern.Pct62, 625);
  Result.Add(TdxShadingPattern.Pct65, 650);
  Result.Add(TdxShadingPattern.Pct70, 700);
  Result.Add(TdxShadingPattern.Pct75, 750);
  Result.Add(TdxShadingPattern.Pct80, 800);
  Result.Add(TdxShadingPattern.Pct85, 850);
  Result.Add(TdxShadingPattern.Pct87, 875);
  Result.Add(TdxShadingPattern.Pct90, 900);
  Result.Add(TdxShadingPattern.Pct95, 950);
  Result.Add(TdxShadingPattern.Solid, 1000);
end;

class function TdxShadingHelper.GetActualBackColor(AFill: TdxAlphaColor; APatternColor: TdxAlphaColor; APattern: TdxShadingPattern): TdxAlphaColor;
var
  AMultiplier, AIntensity: Integer;
begin
  if (APattern = TdxShadingPattern.Clear) or (APattern = TdxShadingPattern.&Nil) then
    Exit(AFill);
  if (TdxAlphaColors.IsTransparentOrEmpty(AFill) or (AFill = TdxAlphaColors.White)) and
    TdxAlphaColors.IsTransparentOrEmpty(APatternColor) then
  begin
    if FPatternMultipliers.TryGetValue(APattern, AMultiplier) then
    begin
      AIntensity := 255 * (1000 - AMultiplier) div 1000;
      Exit(TdxAlphaColors.FromArgb(AIntensity, AIntensity, AIntensity));
    end;
  end;
  if APattern <> TdxShadingPattern.Solid then
    Result := AFill
  else
    Result := APatternColor;
end;

end.
