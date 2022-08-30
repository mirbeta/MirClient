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

unit dxRichEdit.Dialogs.FormControllers;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxRichEdit.View.Core,
  dxRichEdit.Dialogs.Core;

type

  { TdxDocumentProtectionQueryNewPasswordFormControllerParameters }

  TdxDocumentProtectionSetPasswordFormControllerParameters = class(TdxFormControllerParameters)
  private
    FPassword: string;
  public
    constructor Create(const AControl: IdxRichEditControl; const APassword: string);

    property Password: string read FPassword write FPassword;
  end;

  { TdxDocumentProtectionQueryNewPasswordFormController }

  TdxDocumentProtectionQuerySetPasswordFormController = class(TdxFormController)
  private
    FPassword: string;
  public
    constructor Create(AControllerParameters: TdxDocumentProtectionSetPasswordFormControllerParameters);
    procedure ApplyChanges; override;

    property Password: string read FPassword write FPassword;
  end;

  { TdxDocumentEncryptQueryPasswordFormControllerParameters }

  TdxDocumentEncryptQueryPasswordFormControllerParameters = class(TdxDocumentProtectionSetPasswordFormControllerParameters);

  { TdxDocumentProtectionQueryPasswordFormControllerParameters }

  TdxDocumentProtectionGetPasswordFormControllerParameters = class(TdxDocumentProtectionSetPasswordFormControllerParameters);

  { TdxDocumentProtectionQueryPasswordFormController }

  TdxDocumentProtectionGetQueryPasswordFormController = class(TdxDocumentProtectionQuerySetPasswordFormController);

  { TdxInsertMergeFieldFormControllerParameters }

  TdxInsertMergeFieldFormControllerParameters = class(TdxFormControllerParameters);


implementation

{ TdxDocumentProtectionQueryNewPasswordFormControllerParameters }

constructor TdxDocumentProtectionSetPasswordFormControllerParameters.Create(const AControl: IdxRichEditControl;
  const APassword: string);
begin
  inherited Create(AControl);
  FPassword := APassword;
end;

{ TdxDocumentProtectionQueryNewPasswordFormController }

constructor TdxDocumentProtectionQuerySetPasswordFormController.Create(AControllerParameters:
  TdxDocumentProtectionSetPasswordFormControllerParameters);
begin
  Assert(AControllerParameters <> nil, 'controllerParameters');
  FPassword := AControllerParameters.Password;
end;

procedure TdxDocumentProtectionQuerySetPasswordFormController.ApplyChanges;
begin
end;

end.
