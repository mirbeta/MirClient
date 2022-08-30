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

unit dxRichEdit.Import.Html.ObjectTags;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxRichEdit.Import.Html.TagBase;

type
  { TdxObjectTag }

  TdxObjectTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxParamTag }

  TdxParamTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxNoembedTag }

  TdxNoembedTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TdxEmbedTag }

  TdxEmbedTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;


implementation

{ TdxObjectTag }

procedure TdxObjectTag.ApplyTagProperties;
begin
end;

{ TdxParamTag }

procedure TdxParamTag.ApplyTagProperties;
begin
end;

{ TdxNoembedTag }

procedure TdxNoembedTag.ApplyTagProperties;
begin
end;

{ TdxEmbedTag }

procedure TdxEmbedTag.ApplyTagProperties;
begin
end;

end.
