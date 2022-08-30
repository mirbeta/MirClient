{*************************************************************************}
{ TMS ToolBars component                                                  }
{ for Delphi & C++Builder                                                 }
{ version 2.0                                                             }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2005 - 2006                                      }
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

unit AdvToolBarRegDE;

interface
{$I TMSDEFS.INC}

uses
  Classes, AdvToolBar, AdvToolBarDE, AdvToolBarStylers, AdvGlowButton, GDIPicture,
  DesignIntf, DesignEditors;

procedure Register;

implementation

procedure Register;
begin
  RegisterClasses([TAdvToolBarButton, TAdvToolBarMenuButton, TAdvToolBarSeparator,  TAdvToolBarContainer,
    TAdvPage, TAdvGlowMenuButton, TAdvQuickAccessToolBar]);

  {$IFNDEF TMS_STD}
  RegisterClasses([TDBAdvToolBarButton]);
  {$ENDIF}

  RegisterComponentEditor(TAdvToolBar,TAdvToolBarEditor);
  RegisterComponentEditor(TAdvToolBarButton,TAdvToolBarButtonEditor);
  RegisterComponentEditor(TAdvToolBarSeparator,TAdvToolBarButtonEditor);
  RegisterComponentEditor(TAdvDockPanel,TAdvDockPanelEditor);
  RegisterComponentEditor(TAdvToolBarContainer,TAdvToolBarContainerEditor);
  RegisterComponentEditor(TAdvGlowButton,TAdvGlowButtonEditor);


  RegisterComponentEditor(TAdvGlowMenuButton,TAdvGlowButtonEditor);
  RegisterComponentEditor(TAdvToolBarPager,TAdvToolBarPagerEditor);
  RegisterComponentEditor(TAdvPage,TAdvPageEditor);
  RegisterComponentEditor(TAdvQuickAccessToolBar, TAdvQuickAccessToolBarEditor);

  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvToolBar, 'OptionPicture', TGDIPATBPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvToolBar, 'OptionDisabledPicture', TGDIPATBPictureProperty);

  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvToolBar, 'CompactPicture', TGDIPATBPictureProperty);

  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvGlowMenuButton, 'Picture', TGDIPATBPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvGlowMenuButton, 'HotPicture', TGDIPATBPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvGlowMenuButton, 'DisabledPicture', TGDIPATBPictureProperty);

  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvToolBarPager, 'OptionPicture', TGDIPATBPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvToolBarPager, 'OptionDisabledPicture', TGDIPATBPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvToolBarPager, 'HelpButtonPicture', TGDIPATBPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvToolBarPager, 'BrandingPicture', TGDIPATBPictureProperty);

  {$IFNDEF TMS_STD}
  RegisterComponentEditor(TDBAdvGlowButton,TAdvGlowButtonEditor);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TDBAdvGlowButton, 'Picture', TGDIPATBPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TDBAdvGlowButton, 'DisabledPicture', TGDIPATBPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TDBAdvGlowButton, 'HotPicture', TGDIPATBPictureProperty);
  {$ENDIF}
end;



end.

