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

unit dxRichEdit.Options.Simple;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes,
  dxRichEdit.Utils.Types,
  dxRichEdit.Options.Core;

type
  { TdxSimpleDocumentCapabilitiesOptions }

  TdxSimpleDocumentCapabilitiesOptions = class(TdxCustomDocumentCapabilitiesOptions)
  public type
    TAction = class sealed
    public const
      CharacterFormatting = TdxRichEditOptionsAction.CharacterFormatting;
      InlinePictures      = TdxRichEditOptionsAction.InlinePictures;
      Hyperlinks          = TdxRichEditOptionsAction.Hyperlinks;
      ParagraphFormatting = TdxRichEditOptionsAction.ParagraphFormatting;
    end;
  strict private
    FCharacterFormatting: TdxDocumentCapability;
    FInlinePictures: TdxDocumentCapability;
    FHyperlinks: TdxDocumentCapability;
    FParagraphFormatting: TdxDocumentCapability;
    procedure SetCharacterFormatting(const Value: TdxDocumentCapability);
    procedure SetHyperlinks(const Value: TdxDocumentCapability);
    procedure SetInlinePictures(const Value: TdxDocumentCapability);
    procedure SetParagraphFormatting(const Value: TdxDocumentCapability);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;

    function CharacterFormattingAllowed: Boolean; override;
    function HyperlinksAllowed: Boolean; override;
    function InlinePicturesAllowed: Boolean; override;
    function ParagraphFormattingAllowed: Boolean; override;
  published
    property CharacterFormatting: TdxDocumentCapability read FCharacterFormatting write SetCharacterFormatting default TdxDocumentCapability.Default;
    property Hyperlinks: TdxDocumentCapability read FHyperlinks write SetHyperlinks default TdxDocumentCapability.Default;
    property InlinePictures: TdxDocumentCapability read FInlinePictures write SetInlinePictures default TdxDocumentCapability.Default;
    property ParagraphFormatting: TdxDocumentCapability read FParagraphFormatting write SetParagraphFormatting default TdxDocumentCapability.Default;
  end;

  { TdxSimpleFormattingMarkVisibilityOptions }

  TdxSimpleFormattingMarkVisibilityOptions = class(TdxRichEditNotificationOptions)
  public type
    TAction = class sealed
    public const
      HiddenText      = TdxRichEditOptionsAction.HiddenText;
      ParagraphMark   = TdxRichEditOptionsAction.ParagraphMark;
      ShowHiddenText  = TdxRichEditOptionsAction.ShowHiddenText;
      Space           = TdxRichEditOptionsAction.Space;
    end;
  protected const
    DefaultParagraphMarkVisibility = TdxRichEditFormattingMarkVisibility.Auto;
    DefaultSpaceVisibility = TdxRichEditFormattingMarkVisibility.Auto;
    DefaultHiddenTextVisibility = TdxRichEditFormattingMarkVisibility.Auto;
  private
    FParagraphMark: TdxRichEditFormattingMarkVisibility;
    FSpace: TdxRichEditFormattingMarkVisibility;
    FHiddenText: TdxRichEditFormattingMarkVisibility;
    FShowHiddenText: Boolean;
    procedure SetShowHiddenText(const Value: Boolean);
    procedure SetParagraphMark(const Value: TdxRichEditFormattingMarkVisibility);
    procedure SetSpace(const Value: TdxRichEditFormattingMarkVisibility);
    procedure SetHiddenText(const Value: TdxRichEditFormattingMarkVisibility);
  protected
    procedure DoReset; override;
  public
    //for internal use
    property ShowHiddenText: Boolean read FShowHiddenText write SetShowHiddenText;
  published
    property ParagraphMark: TdxRichEditFormattingMarkVisibility read FParagraphMark write SetParagraphMark default DefaultParagraphMarkVisibility;
    property Space: TdxRichEditFormattingMarkVisibility read FSpace write SetSpace default DefaultSpaceVisibility;
    property HiddenText: TdxRichEditFormattingMarkVisibility read FHiddenText write SetHiddenText default DefaultHiddenTextVisibility;
  end;

  { TdxFieldUpdateOnLoadOptions }

  TdxFieldUpdateOnLoadOptions = class
  private
    FUpdateDateField: Boolean;
    FUpdateTimeField: Boolean;
  public
    constructor Create(AUpdateDateField, AUpdateTimeField: Boolean);

    property UpdateDateField: Boolean read FUpdateDateField;
    property UpdateTimeField: Boolean read FUpdateTimeField;
  end;

implementation

{ TdxSimpleDocumentCapabilitiesOptions }

procedure TdxSimpleDocumentCapabilitiesOptions.Assign(Source: TPersistent);
var
  ASource: TdxSimpleDocumentCapabilitiesOptions;
begin
  BeginUpdate;
  try
    if Source is TdxSimpleDocumentCapabilitiesOptions then
    begin
      ASource := TdxSimpleDocumentCapabilitiesOptions(Source);
      CharacterFormatting := ASource.CharacterFormatting;
      Hyperlinks := ASource.Hyperlinks;
      InlinePictures := ASource.InlinePictures;
      ParagraphFormatting := ASource.ParagraphFormatting;
    end;
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

procedure TdxSimpleDocumentCapabilitiesOptions.DoReset;
begin
  inherited DoReset;
  CharacterFormatting := TdxDocumentCapability.Default;
  Hyperlinks := TdxDocumentCapability.Default;
  InlinePictures := TdxDocumentCapability.Default;
  ParagraphFormatting := TdxDocumentCapability.Default;
end;

function TdxSimpleDocumentCapabilitiesOptions.CharacterFormattingAllowed: Boolean;
begin
  Result := IsAllowed(CharacterFormatting);
end;

function TdxSimpleDocumentCapabilitiesOptions.HyperlinksAllowed: Boolean;
begin
  Result := IsAllowed(Hyperlinks);
end;

function TdxSimpleDocumentCapabilitiesOptions.InlinePicturesAllowed: Boolean;
begin
  Result := IsAllowed(InlinePictures);
end;

function TdxSimpleDocumentCapabilitiesOptions.ParagraphFormattingAllowed: Boolean;
begin
  Result := IsAllowed(ParagraphFormatting);
end;

procedure TdxSimpleDocumentCapabilitiesOptions.SetCharacterFormatting(const Value: TdxDocumentCapability);
begin
  if FCharacterFormatting <> Value then
  begin
    FCharacterFormatting := Value;
    DoChanged(TAction.CharacterFormatting);
  end;
end;

procedure TdxSimpleDocumentCapabilitiesOptions.SetHyperlinks(const Value: TdxDocumentCapability);
begin
  if FHyperlinks <> Value then
  begin
    FHyperlinks := Value;
    DoChanged(TAction.Hyperlinks);
  end;
end;

procedure TdxSimpleDocumentCapabilitiesOptions.SetInlinePictures(const Value: TdxDocumentCapability);
begin
  if FInlinePictures <> Value then
  begin
    FInlinePictures := Value;
    DoChanged(TAction.InlinePictures);
  end;
end;

procedure TdxSimpleDocumentCapabilitiesOptions.SetParagraphFormatting(const Value: TdxDocumentCapability);
begin
  if FParagraphFormatting <> Value then
  begin
    FParagraphFormatting := Value;
    DoChanged(TAction.ParagraphFormatting);
  end;
end;

{ TdxSimpleFormattingMarkVisibilityOptions }

procedure TdxSimpleFormattingMarkVisibilityOptions.DoReset;
begin
  inherited DoReset;
  ParagraphMark := DefaultParagraphMarkVisibility;
  Space := DefaultSpaceVisibility;
  HiddenText := DefaultHiddenTextVisibility;
  ShowHiddenText := False;
end;

procedure TdxSimpleFormattingMarkVisibilityOptions.SetHiddenText(const Value: TdxRichEditFormattingMarkVisibility);
begin
  if HiddenText <> Value then
  begin
    FHiddenText := Value;
    DoChanged(TAction.HiddenText);
  end;
end;

procedure TdxSimpleFormattingMarkVisibilityOptions.SetParagraphMark(const Value: TdxRichEditFormattingMarkVisibility);
begin
  if ParagraphMark <> Value then
  begin
    FParagraphMark := Value;
    DoChanged(TAction.ParagraphMark);
  end;
end;

procedure TdxSimpleFormattingMarkVisibilityOptions.SetShowHiddenText(const Value: Boolean);
begin
  if ShowHiddenText <> Value then
  begin
    FShowHiddenText := Value;
    DoChanged(TAction.ShowHiddenText);
  end;
end;

procedure TdxSimpleFormattingMarkVisibilityOptions.SetSpace(const Value: TdxRichEditFormattingMarkVisibility);
begin
  if Space <> Value then
  begin
    FSpace := Value;
    DoChanged(TAction.Space);
  end;
end;

{ TdxFieldUpdateOnLoadOptions }

constructor TdxFieldUpdateOnLoadOptions.Create(AUpdateDateField, AUpdateTimeField: Boolean);
begin
  inherited Create;
  FUpdateDateField := AUpdateDateField;
  FUpdateTimeField := AUpdateTimeField;
end;

end.
