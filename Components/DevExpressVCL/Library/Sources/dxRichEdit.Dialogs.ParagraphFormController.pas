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

unit dxRichEdit.Dialogs.ParagraphFormController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes, SysUtils, Variants, Generics.Defaults, Generics.Collections,
  dxCoreClasses,

  dxRichEdit.NativeApi,
  dxRichEdit.View.Core,
  dxRichEdit.Utils.Types,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Core;

type
  TdxNullableParagraphAlignment = TdxNullableValue<TdxParagraphAlignment>;
  TdxNullableParagraphFirstLineIndent = TdxNullableValue<TdxParagraphFirstLineIndent>;
  TdxNullableParagraphLineSpacing = TdxNullableValue<TdxParagraphLineSpacing>;

  { TdxParagraphFormControllerParameters }

  TdxParagraphFormControllerParameters = class(TdxFormControllerParameters)
  private
    FParagraphProperties: TdxMergedParagraphProperties;
    FUnitConverter: TdxDocumentModelUnitConverter;
  protected
    property UnitConverter: TdxDocumentModelUnitConverter read FUnitConverter;
  public
    constructor Create(const AControl: IdxRichEditControl; AParagraphProperties: TdxMergedParagraphProperties;
      AUnitConverter: TdxDocumentModelUnitConverter);
    property ParagraphProperties: TdxMergedParagraphProperties read FParagraphProperties;
  end;

  { TdxParagraphFormController }

  TdxParagraphFormController = class(TdxFormController)
  private
    FSourceProperties: TdxMergedParagraphProperties;
    FAlignment: TdxNullableParagraphAlignment;
    FFirstLineIndent: TdxNullableInteger;
    FFirstLineIndentType: TdxNullableParagraphFirstLineIndent;
    FLeftIndent: TdxNullableInteger;
    FRightIndent: TdxNullableInteger;
    FSpacingAfter: TdxNullableInteger;
    FSpacingBefore: TdxNullableInteger;
    FLineSpacing: TdxNullableSingle;
    FLineSpacingType: TdxNullableParagraphLineSpacing;
    FSuppressHyphenation: TdxNullableBoolean;
    FSuppressLineNumbers: TdxNullableBoolean;

    FOutlineLevel: TdxNullableInteger;
    FUnitConverter: TdxDocumentModelUnitConverter;

    FKeepLinesTogether: TdxNullableBoolean;
    FPageBreakBefore: TdxNullableBoolean;
    FContextualSpacing: TdxNullableBoolean;
    FCanEditTabs: Boolean;
    function GetSourceOptions: PdxParagraphFormattingOptions; inline;
    function GetSourceInfo: TdxParagraphFormattingInfo;
  protected
    procedure InitializeController; virtual;
    procedure ApplyRightIndent; virtual;
    procedure ApplyLeftIndent; virtual;
    procedure ApplyFirstLineIndent; virtual;
    procedure ApplyAlignment; virtual;
    procedure ApplySpacingAfter; virtual;
    procedure ApplySpacingBefore; virtual;
    procedure ApplyContextualSpacing; virtual;
    procedure ApplyLineSpacing; virtual;
    procedure ApplySuppressHyphenation; virtual;
    procedure ApplySuppressLineNumbers; virtual;
    procedure ApplyOutlineLevel; virtual;
    procedure ApplyKeepLinesTogether; virtual;
    procedure ApplyPageBreakBefore; virtual;

    property SourceOptions: PdxParagraphFormattingOptions read GetSourceOptions;
    property SourceInfo: TdxParagraphFormattingInfo read GetSourceInfo;
  public
    constructor Create(AControllerParameters: TdxParagraphFormControllerParameters);
    procedure ApplyChanges; override;

    property UnitConverter: TdxDocumentModelUnitConverter read FUnitConverter;
    property Alignment: TdxNullableParagraphAlignment read FAlignment write FAlignment;
    property FirstLineIndent: TdxNullableInteger read FFirstLineIndent write FFirstLineIndent;
    property FirstLineIndentType: TdxNullableParagraphFirstLineIndent read FFirstLineIndentType write FFirstLineIndentType;
    property LeftIndent: TdxNullableInteger read FLeftIndent write FLeftIndent;
    property RightIndent: TdxNullableInteger read FRightIndent write FRightIndent;
    property SpacingAfter: TdxNullableInteger read FSpacingAfter write FSpacingAfter;
    property SpacingBefore: TdxNullableInteger read FSpacingBefore write FSpacingBefore;
    property LineSpacing: TdxNullableSingle read FLineSpacing write FLineSpacing;
    property LineSpacingType: TdxNullableParagraphLineSpacing read FLineSpacingType write FLineSpacingType;
    property SuppressHyphenation: TdxNullableBoolean read FSuppressHyphenation write FSuppressHyphenation;
    property SuppressLineNumbers: TdxNullableBoolean read FSuppressLineNumbers write FSuppressLineNumbers;
    property OutlineLevel: TdxNullableInteger read FOutlineLevel write FOutlineLevel;
    property KeepLinesTogether: TdxNullableBoolean read FKeepLinesTogether write FKeepLinesTogether;
    property PageBreakBefore: TdxNullableBoolean read FPageBreakBefore write FPageBreakBefore;
    property ContextualSpacing: TdxNullableBoolean read FContextualSpacing write FContextualSpacing;
    property CanEditTabs: Boolean read FCanEditTabs;

    property SourceProperties: TdxMergedParagraphProperties read FSourceProperties;
  end;

implementation

uses
  dxRichEdit.Options,
  dxRichEdit.Utils.Exceptions;

{ TdxParagraphFormControllerParameters }

constructor TdxParagraphFormControllerParameters.Create(const AControl: IdxRichEditControl;
  AParagraphProperties: TdxMergedParagraphProperties; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  inherited Create(AControl);
  Assert(AParagraphProperties <> nil);
  Assert(AUnitConverter <> nil);
  FParagraphProperties := AParagraphProperties;
  FUnitConverter := AUnitConverter;
end;

{ TdxParagraphFormController }

function TdxParagraphFormController.GetSourceOptions: PdxParagraphFormattingOptions;
begin
  Result := @SourceProperties.Options;
end;

procedure TdxParagraphFormController.ApplyAlignment;
begin
  SourceOptions.UseAlignment := not Alignment.IsNull and
    ((SourceInfo.Alignment <> Alignment.Value) or not SourceOptions.UseAlignment);
  if SourceOptions.UseAlignment then
    SourceInfo.Alignment := Alignment.Value;
end;

procedure TdxParagraphFormController.ApplyChanges;
begin
  ApplyLeftIndent;
  ApplyRightIndent;
  ApplyFirstLineIndent;
  ApplyAlignment;
  ApplySpacingAfter;
  ApplySpacingBefore;
  ApplyLineSpacing;
  ApplySuppressHyphenation;
  ApplySuppressLineNumbers;
  ApplyOutlineLevel;
  ApplyKeepLinesTogether;
  ApplyPageBreakBefore;
  ApplyContextualSpacing;
end;

procedure TdxParagraphFormController.ApplyContextualSpacing;
begin
  SourceOptions.UseContextualSpacing := not ContextualSpacing.IsNull and
    ((SourceInfo.ContextualSpacing <> ContextualSpacing.Value) or not SourceOptions.UseContextualSpacing);
  if SourceOptions.UseContextualSpacing then
    SourceInfo.ContextualSpacing := ContextualSpacing.Value;
end;

procedure TdxParagraphFormController.ApplyFirstLineIndent;
begin
  SourceOptions.UseFirstLineIndent := not FirstLineIndentType.IsNull and
    (not FirstLineIndent.IsNull or (FirstLineIndentType.Value = TdxParagraphFirstLineIndent.None)) and
    ((FirstLineIndent.IsNull or (SourceInfo.FirstLineIndent <> FirstLineIndent.Value)) or
      (SourceInfo.FirstLineIndentType <> FirstLineIndentType.Value) or not SourceOptions.UseLeftIndent);
  if SourceOptions.UseFirstLineIndent then
  begin
    if not FirstLineIndent.IsNull then
      SourceInfo.FirstLineIndent := FirstLineIndent.Value;
    SourceInfo.FirstLineIndentType := FirstLineIndentType.Value;
  end;
end;

procedure TdxParagraphFormController.ApplyKeepLinesTogether;
begin
  SourceOptions.UseKeepLinesTogether := not KeepLinesTogether.IsNull and
    ((SourceInfo.KeepLinesTogether <> KeepLinesTogether.Value) or not SourceOptions.UseKeepLinesTogether);
  if SourceOptions.UseKeepLinesTogether then
    SourceInfo.KeepLinesTogether := KeepLinesTogether.Value;
end;

procedure TdxParagraphFormController.ApplyLeftIndent;
begin
  SourceOptions.UseLeftIndent := not LeftIndent.IsNull and
    ((SourceInfo.LeftIndent <> LeftIndent.Value) or not SourceOptions.UseLeftIndent);
  if SourceOptions.UseLeftIndent then
    SourceInfo.LeftIndent := LeftIndent.Value;
end;

procedure TdxParagraphFormController.ApplyLineSpacing;
begin
  SourceOptions.UseLineSpacing := not LineSpacingType.IsNull and (LineSpacing.IsNull or
    (SourceInfo.LineSpacing <> LineSpacing.Value) or (SourceInfo.LineSpacingType <> LineSpacingType.Value) or
      not SourceOptions.UseLeftIndent);
  if SourceOptions.UseLineSpacing then
  begin
    if not LineSpacing.IsNull then
      SourceInfo.LineSpacing := LineSpacing.Value;
    SourceInfo.LineSpacingType := LineSpacingType.Value;
  end;
end;

procedure TdxParagraphFormController.ApplyOutlineLevel;
begin
  SourceOptions.UseOutlineLevel := not OutlineLevel.IsNull and
    ((SourceInfo.OutlineLevel <> OutlineLevel.Value) or not SourceOptions.UseOutlineLevel);
  if SourceOptions.UseOutlineLevel then
    SourceInfo.OutlineLevel := OutlineLevel.Value;
end;

procedure TdxParagraphFormController.ApplyPageBreakBefore;
begin
  SourceOptions.UsePageBreakBefore := not PageBreakBefore.IsNull and
    ((SourceInfo.PageBreakBefore <> PageBreakBefore.Value) or not SourceOptions.UsePageBreakBefore);
  if SourceOptions.UsePageBreakBefore then
    SourceInfo.PageBreakBefore := PageBreakBefore.Value;
end;

procedure TdxParagraphFormController.ApplyRightIndent;
begin
  SourceOptions.UseRightIndent := not RightIndent.IsNull and
    ((SourceInfo.RightIndent <> RightIndent.Value) or not SourceOptions.UseRightIndent);
  if SourceOptions.UseRightIndent then
    SourceInfo.RightIndent := RightIndent.Value;
end;

procedure TdxParagraphFormController.ApplySpacingAfter;
begin
  SourceOptions.UseSpacingAfter := not SpacingAfter.IsNull and
    ((SourceInfo.SpacingAfter <> SpacingAfter.Value) or not SourceOptions.UseSpacingAfter);
  if SourceOptions.UseSpacingAfter then
    SourceInfo.SpacingAfter := SpacingAfter.Value;
end;

procedure TdxParagraphFormController.ApplySpacingBefore;
begin
  SourceOptions.UseSpacingBefore := not SpacingBefore.IsNull and
    ((SourceInfo.SpacingBefore <> SpacingBefore.Value) or not SourceOptions.UseSpacingBefore);
  if SourceOptions.UseSpacingBefore then
    SourceInfo.SpacingBefore := SpacingBefore.Value;
end;

procedure TdxParagraphFormController.ApplySuppressHyphenation;
begin
  SourceOptions.UseSuppressHyphenation := not SuppressHyphenation.IsNull and
    ((SourceInfo.SuppressHyphenation <> SuppressHyphenation.Value) or not SourceOptions.UseSuppressHyphenation);
  if SourceOptions.UseSuppressHyphenation then
    SourceInfo.SuppressHyphenation := SuppressHyphenation.Value;
end;

procedure TdxParagraphFormController.ApplySuppressLineNumbers;
begin
  SourceOptions.UseSuppressLineNumbers := not SuppressLineNumbers.IsNull and
    ((SourceInfo.SuppressLineNumbers <> SuppressLineNumbers.Value) or not SourceOptions.UseSuppressLineNumbers);
  if SourceOptions.UseSuppressLineNumbers then
    SourceInfo.SuppressLineNumbers := SuppressLineNumbers.Value;
end;

constructor TdxParagraphFormController.Create(AControllerParameters: TdxParagraphFormControllerParameters);
var
  ACapabilityOptions: TdxDocumentCapabilitiesOptions;
begin
  inherited Create;
  Assert(AControllerParameters <> nil);
  FSourceProperties := AControllerParameters.ParagraphProperties;
  FUnitConverter := AControllerParameters.UnitConverter;
  if AControllerParameters.Control.InnerControl <> nil then
  begin
    ACapabilityOptions := AControllerParameters.Control.InnerControl.DocumentModel.DocumentCapabilities;
    FCanEditTabs := ACapabilityOptions.ParagraphFormattingAllowed and ACapabilityOptions.ParagraphTabsAllowed;
  end
  else
    FCanEditTabs := True;
  InitializeController;
end;

function TdxParagraphFormController.GetSourceInfo: TdxParagraphFormattingInfo;
begin
  Result := SourceProperties.Info;
end;

procedure TdxParagraphFormController.InitializeController;
begin
  LeftIndent := TdxNullableValue<Integer>.Create(SourceOptions.UseLeftIndent, SourceInfo.LeftIndent);
  RightIndent := TdxNullableValue<Integer>.Create(SourceOptions.UseRightIndent, SourceInfo.RightIndent);
  FirstLineIndent := TdxNullableValue<Integer>.Create(SourceOptions.UseFirstLineIndent, SourceInfo.FirstLineIndent);
  FirstLineIndentType := TdxNullableValue<TdxParagraphFirstLineIndent>.Create(SourceOptions.UseFirstLineIndent, SourceInfo.FirstLineIndentType);
  Alignment := TdxNullableValue<TdxParagraphAlignment>.Create(SourceOptions.UseAlignment, SourceInfo.Alignment);
  SpacingAfter := TdxNullableValue<Integer>.Create(SourceOptions.UseSpacingAfter, SourceInfo.SpacingAfter);
  SpacingBefore := TdxNullableValue<Integer>.Create(SourceOptions.UseSpacingBefore, SourceInfo.SpacingBefore);
  LineSpacing := TdxNullableValue<Single>.Create(SourceOptions.UseLineSpacing, SourceInfo.LineSpacing);
  LineSpacingType := TdxNullableValue<TdxParagraphLineSpacing>.Create(SourceOptions.UseLineSpacing, SourceInfo.LineSpacingType);
  SuppressHyphenation := TdxNullableValue<Boolean>.Create(SourceOptions.UseSuppressHyphenation, SourceInfo.SuppressHyphenation);
  SuppressLineNumbers := TdxNullableValue<Boolean>.Create(SourceOptions.UseSuppressLineNumbers, SourceInfo.SuppressLineNumbers);
  OutlineLevel := TdxNullableValue<Integer>.Create(SourceOptions.UseOutlineLevel, SourceInfo.OutlineLevel);
  KeepLinesTogether := TdxNullableValue<Boolean>.Create(SourceOptions.UseKeepLinesTogether, SourceInfo.KeepLinesTogether);
  PageBreakBefore := TdxNullableValue<Boolean>.Create(SourceOptions.UsePageBreakBefore, SourceInfo.PageBreakBefore);
  ContextualSpacing := TdxNullableValue<Boolean>.Create(SourceOptions.UseContextualSpacing, SourceInfo.ContextualSpacing);
end;

end.
