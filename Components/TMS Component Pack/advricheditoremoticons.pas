{*************************************************************************}
{ TMS TAdvRichEditorEmoticons                                             }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2015                                              }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvRichEditorEmoticons;

interface

uses
  Classes, GDIPPictureContainer;

{$R AdvRichEditorEmoticonsS.res}
{$R AdvRichEditorEmoticonsL.res}

type
  TAdvRichEditorSmallEmoticons = class(TGDIPPictureContainer)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TAdvRichEditorLargeEmoticons = class(TGDIPPictureContainer)
  public
    constructor Create(AOwner: TComponent); override;
  end;


implementation

{ TAdvRichEditorSmallEmoticons }

constructor TAdvRichEditorSmallEmoticons.Create(AOwner: TComponent);
begin
  inherited;
  AddFromResource('tmsresangel','O:)');
  AddFromResource('tmsresbigsmile',':D');
  AddFromResource('tmsrescheerful','^_^');
  AddFromResource('tmsresconfused','O.O');
  AddFromResource('tmsrescrying',':''(');
  AddFromResource('tmsrescurlylips',':*');
  AddFromResource('tmsresfacebook','(y)');
  AddFromResource('tmsresglasses','8-)');
  AddFromResource('tmsresgrumpy','>:(');
  AddFromResource('tmsresheart','<3');
  AddFromResource('tmsreskiss',':*');
  AddFromResource('tmsrespacman',':v');
  AddFromResource('tmsrespoop',':poop:');
  AddFromResource('tmsressad',':(');
  AddFromResource('tmsressmiley',':)');
  AddFromResource('tmsressmileywithtongue',':P');
  AddFromResource('tmsressquinting','-_-');
  AddFromResource('tmsressunglasses','');
  AddFromResource('tmsresunsure',':/');
  AddFromResource('tmsreswinking',';)');
end;

{ TAdvRichEditorLargeEmoticons }

constructor TAdvRichEditorLargeEmoticons.Create(AOwner: TComponent);
begin
  inherited;
  AddFromResource('tmsrelangel','O:)');
  AddFromResource('tmsrelbigsmile',':D');
  AddFromResource('tmsrelcheerful','^_^');
  AddFromResource('tmsrelconfused','O.O');
  AddFromResource('tmsrelcrying',':''(');
  AddFromResource('tmsrelcurlylips',':*');
  AddFromResource('tmsrelfacebook','(y)');
  AddFromResource('tmsrelglasses','8-)');
  AddFromResource('tmsrelgrumpy','>:(');
  AddFromResource('tmsrelheart','<3');
  AddFromResource('tmsrelkiss',':*');
  AddFromResource('tmsrelpacman',':v');
  AddFromResource('tmsrelpoop',':poop:');
  AddFromResource('tmsrelsad',':(');
  AddFromResource('tmsrelsmiley',':)');
  AddFromResource('tmsrelsmileywithtongue',':P');
  AddFromResource('tmsrelsquinting','-_-');
  AddFromResource('tmsrelsunglasses','');
  AddFromResource('tmsrelunsure',':/');
  AddFromResource('tmsrelwinking',';)');
end;

end.
