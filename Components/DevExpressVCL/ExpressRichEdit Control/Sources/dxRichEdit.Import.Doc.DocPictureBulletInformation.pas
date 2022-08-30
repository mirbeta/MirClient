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
unit dxRichEdit.Import.Doc.DocPictureBulletInformation;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

type

  { TdxDocPictureBulletInformation }

  TdxDocPictureBulletInformation = class
  strict private
    FPictureBullet: Boolean;
    FSuppressBulletResize: Boolean;
    FDefaultPicture: Boolean;
    FPictureCharacterPosition: Integer;
  public
    function Clone: TdxDocPictureBulletInformation;

    property DefaultPicture: Boolean read FDefaultPicture write FDefaultPicture;
    property PictureBullet: Boolean read FPictureBullet write FPictureBullet;
    property SuppressBulletResize: Boolean read FSuppressBulletResize write FSuppressBulletResize;
    property PictureCharacterPosition: Integer read FPictureCharacterPosition write FPictureCharacterPosition;
  end;

implementation

{ TdxDocPictureBulletInformation }

function TdxDocPictureBulletInformation.Clone: TdxDocPictureBulletInformation;
begin
  Result := TdxDocPictureBulletInformation.Create;
  Result.FDefaultPicture := FDefaultPicture;
  Result.FPictureBullet := FPictureBullet;
  Result.FPictureCharacterPosition := FPictureCharacterPosition;
  Result.FSuppressBulletResize := FSuppressBulletResize;
end;

end.
