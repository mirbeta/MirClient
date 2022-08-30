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

unit dxRichEdit.Dialogs.Core;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.View.Core;

type
  { IdxFormOwner }

  IdxFormOwner = interface
    function GetUnitConverter: TdxDocumentModelUnitConverter;
    procedure Hide;
    procedure Show;
    procedure Close;

    property UnitConverter: TdxDocumentModelUnitConverter read GetUnitConverter;
  end;

  { TdxFormControllerParameters }

  TdxFormControllerParameters = class abstract
  private
    FControl: IdxRichEditControl;
  public
    constructor Create(const AControl: IdxRichEditControl);

    property Control: IdxRichEditControl read FControl;
  end;

  { TdxFormController }

  TdxFormController = class abstract
  public
    procedure ApplyChanges; virtual; abstract;
  end;

implementation

{ TdxFormControllerParameters }

constructor TdxFormControllerParameters.Create(const AControl: IdxRichEditControl);
begin
  inherited Create;
  Assert(AControl <> nil);
  FControl := AControl;
end;

end.
