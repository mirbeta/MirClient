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

unit dxRichEdit.Import.Rtf.ParagraphFormatting;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Utils.Types;

type
  { TdxRtfParagraphFormattingInfo }

  TdxRtfParagraphFormattingInfo = class(TdxParagraphFormattingInfo)
  public const
    DefaultAtListSpacingInTwips = 240;
  private
    FInTableParagraph: Boolean;
    FListLevelIndex: Integer;
    FNestingLevel: Integer;
    FNextStyle: Integer;
    FNumberingListIndex: TdxNumberingListIndex;
    FParentStyleIndex: Integer;
    FProcessedBorder: TdxBorderInfo;
    FRtfLineSpacingMultiplier: Integer;
    FRtfLineSpacingType: Integer;
    FRtfTableStyleIndexForRowOrCell: Integer;
    FStyleIndex: Integer;
    FStyleLink: Integer;
    FTabAlignment: TdxTabAlignmentType;
    FTabLeader: TdxTabLeaderType;
    FTabs: TdxTabFormattingInfo;
    FUseLineSpacingMultiplier: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure CopyFrom(Source: TdxCloneable); override;

    function CalcLineSpacing(AUnitConverter: TdxDocumentModelUnitConverter): Single;
    function CalcLineSpacingType: TdxParagraphLineSpacing;

    property InTableParagraph: Boolean read FInTableParagraph write FInTableParagraph;
    property ListLevelIndex: Integer read FListLevelIndex write FListLevelIndex;
    property NestingLevel: Integer read FNestingLevel write FNestingLevel;
    property NextStyle: Integer read FNextStyle write FNextStyle;
    property NumberingListIndex: TdxNumberingListIndex read FNumberingListIndex write FNumberingListIndex;
    property ParentStyleIndex: Integer read FParentStyleIndex write FParentStyleIndex;
    property ProcessedBorder: TdxBorderInfo read FProcessedBorder write FProcessedBorder;
    property RtfLineSpacingMultiplier: Integer read FRtfLineSpacingMultiplier write FRtfLineSpacingMultiplier;
    property RtfLineSpacingType: Integer read FRtfLineSpacingType write FRtfLineSpacingType;
    property RtfTableStyleIndexForRowOrCell: Integer read FRtfTableStyleIndexForRowOrCell write FRtfTableStyleIndexForRowOrCell;
    property StyleIndex: Integer read FStyleIndex write FStyleIndex;
    property StyleLink: Integer read FStyleLink write FStyleLink;
    property TabAlignment: TdxTabAlignmentType read FTabAlignment write FTabAlignment;
    property TabLeader: TdxTabLeaderType read FTabLeader write FTabLeader;
    property Tabs: TdxTabFormattingInfo read FTabs;
    property UseLineSpacingMultiplier: Boolean read FUseLineSpacingMultiplier write FUseLineSpacingMultiplier;
  end;

implementation

uses
  Math;

{ TdxRtfParagraphFormattingInfo }

constructor TdxRtfParagraphFormattingInfo.Create;
begin
  inherited Create;
  FTabs := TdxTabFormattingInfo.Create;
  FRtfLineSpacingMultiplier := 1;
  FStyleLink := -1;
  FNextStyle := -1;
  FNumberingListIndex := NumberingListIndexListIndexNotSetted;
  WidowOrphanControl := True;
end;

destructor TdxRtfParagraphFormattingInfo.Destroy;
begin
  FreeAndNil(FTabs);
  inherited Destroy;
end;

procedure TdxRtfParagraphFormattingInfo.CopyFrom(Source: TdxCloneable);
var
  ASource: TdxRtfParagraphFormattingInfo;
begin
  if Source is TdxRtfParagraphFormattingInfo then
  begin
    ASource := TdxRtfParagraphFormattingInfo(Source);
    InTableParagraph := ASource.InTableParagraph;
    ListLevelIndex := ASource.ListLevelIndex;
    NestingLevel := ASource.NestingLevel;
    NextStyle := ASource.NextStyle;
    NumberingListIndex := ASource.NumberingListIndex;
    RtfLineSpacingType := ASource.RtfLineSpacingType;
    RtfLineSpacingMultiplier := ASource.RtfLineSpacingMultiplier;
    UseLineSpacingMultiplier := ASource.UseLineSpacingMultiplier;
    RtfTableStyleIndexForRowOrCell := ASource.RtfTableStyleIndexForRowOrCell;
    ParentStyleIndex := ASource.ParentStyleIndex;
    StyleIndex := ASource.StyleIndex;
    StyleLink := ASource.StyleLink;
    TabAlignment := ASource.TabAlignment;
    TabLeader := ASource.TabLeader;
    Tabs.CopyFrom(ASource.Tabs);
  end;
  inherited CopyFrom(Source);
end;

function TdxRtfParagraphFormattingInfo.CalcLineSpacing(AUnitConverter: TdxDocumentModelUnitConverter): Single;
begin
  if RtfLineSpacingMultiplier = 0 then
  begin
    if (RtfLineSpacingType = 0) and FUseLineSpacingMultiplier then
      Result := AUnitConverter.TwipsToModelUnits(DefaultAtListSpacingInTwips)
    else
      if RtfLineSpacingType <> 0 then
        Result := Max(AUnitConverter.TwipsToModelUnits(Abs(RtfLineSpacingType)), 1)
      else
        Result := 0;
  end
  else
  begin
    if RtfLineSpacingType < 0 then
      Result := Max(AUnitConverter.TwipsToModelUnits(Abs(RtfLineSpacingType)), 1)
    else
      Result := RtfLineSpacingType / 240.0;
  end;
end;

function TdxRtfParagraphFormattingInfo.CalcLineSpacingType: TdxParagraphLineSpacing;
begin
  if FRtfLineSpacingType < 0 then
    Exit(TdxParagraphLineSpacing.Exactly);

  if FRtfLineSpacingType > 0 then
  begin
    if FRtfLineSpacingMultiplier = 0 then
      Exit(TdxParagraphLineSpacing.AtLeast)
    else
    begin
      case RtfLineSpacingType of
        240: Result := TdxParagraphLineSpacing.Single;
        360: Result := TdxParagraphLineSpacing.Sesquialteral;
        480: Result := TdxParagraphLineSpacing.Double;
      else
        if RtfLineSpacingType <= 0 then
          Result := TdxParagraphLineSpacing.Single
        else
          Result := TdxParagraphLineSpacing.Multiple;
      end;
    end;
  end
  else
  begin
    if (FRtfLineSpacingMultiplier = 0) and FUseLineSpacingMultiplier then
      Result := TdxParagraphLineSpacing.AtLeast
    else
      Result := TdxParagraphLineSpacing.Single;
  end;
end;

end.
