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

unit dxRichEdit.Import.Html.FormTags;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxRichEdit.Import.Html.TagBase,
  dxRichEdit.Import.Html.DocumentTags,
  dxRichEdit.DocumentModel.ParagraphFormatting;

type

  { TdxCustomFormTag }

  TdxCustomFormTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
    function ApplyCssProperties: TdxParagraphFormattingOptions; override;
    procedure OpenTagProcessCore; override;
  end;

  { TdxButtonTag }

  TdxButtonTag = class(TdxCustomFormTag);

  { TdxFormTag }

  TdxFormTag = class(TdxCustomFormTag);

  { TdxFieldsetTag }

  TdxFieldsetTag = class(TdxCustomFormTag);

  { TdxInputTag }

  TdxInputTag = class(TdxCustomFormTag);

  { TdxLegendTag }

  TdxLegendTag = class(TdxCustomFormTag);

  { TdxLabelTag }

  TdxLabelTag = class(TdxCustomFormTag);

  { TdxTextAreaTag }

  TdxTextAreaTag = class(TdxCustomFormTag);

  { TdxSelectTag }

  TdxSelectTag = class(TdxCustomFormTag);

  { TdxOptGroupTag }

  TdxOptGroupTag = class(TdxCustomFormTag);

  { TdxOptionTag }

  TdxOptionTag = class(TdxCustomFormTag);

implementation

uses
  dxRichEdit.Import.Html;

type
  TdxCustomFormTagHelper = class helper for TdxCustomFormTag
  private
    function GetImporter: TdxHtmlImporter; inline;
  public
    property Importer: TdxHtmlImporter read GetImporter;
  end;

{ TdxCustomFormTagHelper }

function TdxCustomFormTagHelper.GetImporter: TdxHtmlImporter;
begin
  Result := TdxHtmlImporter(inherited Importer);
end;

{ TdxCustomFormTag }

procedure TdxCustomFormTag.ApplyTagProperties;
begin
end;

function TdxCustomFormTag.ApplyCssProperties: TdxParagraphFormattingOptions;
begin
  Result := TdxParagraphFormattingOptions.EmptyParagraphFormattingOption;
end;

procedure TdxCustomFormTag.OpenTagProcessCore;
begin
  inherited OpenTagProcessCore;
  Importer.Position.CopyFrom(Importer.TagsStack[Importer.TagsStack.Count - 1].OldPosition);
end;

end.
