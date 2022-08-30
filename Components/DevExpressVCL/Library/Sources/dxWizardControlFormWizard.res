        ��  ��                  U  t   D X W I Z A R D T E M P L A T E S   W I Z A R D C O N T R O L C B U I L D E R H E A D E R       0           //---------------------------------------------------------------------------

#ifndef %ModuleIdent%H
#define %ModuleIdent%H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "dxCustomWizardControl.hpp"
#include "dxWizardControl.hpp"
#include "dxWizardControlForm.hpp"
//---------------------------------------------------------------------------
class T%FormIdent% : public TdxWizardControlForm
{
__published:	// IDE-managed Components
	TdxWizardControl *dxWizardControl1;
	TdxWizardControlPage *dxWizardControlPage1;
private:	// User declarations
public:	// User declarations
	__fastcall T%FormIdent%(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE T%FormIdent% *%FormIdent%;
//---------------------------------------------------------------------------
#endif
     p   D X W I Z A R D T E M P L A T E S   W I Z A R D C O N T R O L C B U I L D E R U N I T       0           //---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "%ModuleIdent%.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "dxCustomWizardControl"
#pragma link "dxWizardControl"
#pragma link "dxWizardControlForm"
#pragma resource "*.dfm"
T%FormIdent% *%FormIdent%;
//---------------------------------------------------------------------------
__fastcall T%FormIdent%::T%FormIdent%(TComponent* Owner)
	: TdxWizardControlForm(Owner)
{
}
//---------------------------------------------------------------------------
 6  l   D X W I Z A R D T E M P L A T E S   W I Z A R D C O N T R O L D E L P H I U N I T       0           unit %ModuleIdent%;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxControls, cxGraphics, cxLookAndFeelPainters, cxLookAndFeels,
  dxCustomWizardControl, dxWizardControl, dxWizardControlForm;

type
  T%FormIdent% = class(TdxWizardControlForm)
    dxWizardControl1: TdxWizardControl;
    dxWizardControlPage1: TdxWizardControlPage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  %FormIdent%: T%FormIdent%;

implementation

{$R *.dfm}

end.  �  `   D X W I Z A R D T E M P L A T E S   W I Z A R D C O N T R O L F O R M       0           object %FormIdent%: T%FormIdent%
  Caption = '%FormIdent%'
  ClientHeight = 480
  ClientWidth = 640
  object dxWizardControl1: TdxWizardControl
    Left = 0
    Top = 0
    Width = 640
    Height = 480
    ActivePage = dxWizardControlPage1
    AutoSize = True
    object dxWizardControlPage1: TdxWizardControlPage
      Left = 11
      Top = 76
      Width = 618
      Height = 344
    end
  end
end   �      �� ��     0          (       @                                	

!! ##"   	
	 ! ##"%%$(('	
	  ""!%%$''&**)	

""!$$#''&))(,,+TUT	

!! $$#&&%))(++*..-���565	
	!! ##"&&%(('++*--,00/������	
	  "#"%%$''&**)--,//.221���������	

  ""!$%$''&**),,+/.-110443������������	

!"!$$#&&%))(,,+..-10/332665���������������	

!! ##"&&%(('++*.-,00/321554887������������������	
	 ! ##"%%$(('**)--,0/.221554776::9���������������������	
	  ""!%%$''&**),,+//.210443765998<<;������������������������%&%	

""!$$#''&))(,,+..-110432665987;;:>>=���������������������������WXW!! $$#&&%))(++*..-00/332654887;:9>=<@@?/Nɱ�����������������������������!! ##"&&%(('++*--,00/221554876::9=<;@?>BBA/O�/Pˊ�����������������������������@@?  "#"%%$''&**)--,//.221543776:98<<;??>BA@EDC/O�/Q�.R�p��������������������������������))( ""!$%$''&**),,+/.-110443765998<;:?>=AA@DCBGFE/P�.Q�.S�-U�Gk�������������������������������sss!"!$$#&&%))(,,+..-10/332665987;;:>=<A@?CCBFEDIHG/P�.R�.S�-U�-W�,X҉��������������������������������\\\(('++*.-,00/321554887;:9==<@?>CBAEEDHGFKJI/Q�.R�.T�-V�-W�,Y�,Z�Fpڼ��������������������������������||{0/.221554776::9=<;??>BA@EDCGGFJIHMLK.Q�.S�-U�-V�,X�,Y�+[�+]�*^�_����������������������������������噙�PON998<<;?>=BA@DCBGFEJIHLKJONM.R�.S�-U�-W�,X�,Z�+[�+]�*_�*`�)b�z�������������������������������������ړ��YXWDCBFEDIHGLKJNMLQPO.R�.T�-V�-W�,Y�,Z�+\�+^�*_�*a�)b�)d�(f�^���������������������������������������ܣ��xwwNMLQPOSRQ.S�-T�-V�,X�,Y�+[�+]�*^�*`�)a�)c�(e�(f�(h�'i�B~撵������������������������������������������޽��.S�-U�-W�,X�,Z�+[�+]�*_�*`�)b�)c�(e�(g�'h�'j�&k�&m�%o�N����������������������������������������.T�-V�-W�,Y�,Z�+\�+^�*_�*a�)b�)d�(f�(g�'i�'j�&l�&n�%o�%q�$r�$t�?��v����������������������������-T�-V�,X�,Y�+[�+]�*^�*`�)a�)c�(e�(f�(h�'i�'k�&l�&n�%p�%q�$s�$t�#v�#w�"y�"z�/��Y�����������������-U�-W�,X�,Z�+[�+]�*_�*`�)b�)c�(e�(g�'h�'j�&k�&m�%o�%p�$r�$s�#u�#v�#x�"y�"{�!|�!~� � �� �� �� ��-V�-W�,Y�,Z�+\�+^�*_�*a�)b�)d�(f�(g�'i�'j�&l�&n�%o�%q�$r�$t�#u�#w�"x�"z�!{�!}� ~� �� �� �� �� ��-V�,X�,Y�+[�+]�*^�*`�)a�)c�)e�(f�(h�'i�'k�&l�&n�%p�%q�$s�$t�#v�#w�"y�"z�!|�!}� � �� �� �� �� ��-W�,X�,Z�+[�+]�*_�*`�)b�)c�(e�(g�'h�'j�&k�&m�%o�%p�$r�$s�#u�#v�#x�"y�"{�!|�!~� � �� �� �� �� ��   ,Y�,Z�+\�+^�*_�*a�)b�)d�(f�(g�'i�'j�&l�&m�%o�%q�$r�$t�#u�#w�"x�"z�!{�!}� ~� �� �� �� �� ��   �                                                                                                                          �  h      �� ��     0          (                                    	
	!! %&%	 $$#))(CCC	##"(('--,���565""!''&,,+110������

!! &&%++*00/654���������565	
	  %%$**)//.443:98������������FGF$$#))(..-332887>=<;Y����������������##"(('--,221776=<;BA@/P�;^ѣ��������������VVU""!''&,,+110665;;:A@?FED.Q�-T�,X�a�����������������RRQ0/.554::9@?>EDCJJI.R�-U�,Y�+\�*_؇����������������̄��JJIDCBIHGONM.S�-W�,Z�+]�*`�)c�(g�x�������������������Ц��{zy-T�,X�,[�+^�*a�)d�(h�'k�&n�@�ꄱ���������������-U�,Y�+\�*_�)b�(e�'i�&l�%o�$r�#u�"x�!{�:��T��y��-V�,Z�+]�*`�)c�(f�'j�&m�%p�$s�#v�"y�!|� � �� ��-X�,[�+^�*a�)d�(h�'k�&n�%q�$t�#w�"z�!}� �� �� ��                                                                �      �� ��     0          (   0   `                                   	

 !! ##"         		
	!! ""!$$#&&%   		
	

  ""!#$#%%$''&(('	
	

  !"!##"%%$''&(('**)	

 !! ##"$%$&&%(('**)++*	

!! "#"$$#&&%''&))(++*--,���		
	 ! ""!$$#%%$''&))(++*,,+..-���565		
	

  ""!##"%%$''&(('**),,+.-,//.������	
	

  !! ##"%%$&&%(('**)++*--,//.10/���������	

 !! ##"$$#&&%(('))(++*--,..-00/221������������		
	!! ""!$$#&&%''&))(++*,,+..-00/210332���������������		
	

  ""!#$#%%$''&(('**),,+..-//.110332543���������������ddd	
	

  !"!##"%%$''&(('**),,+--,//.110321443665������������������ddd	

 !! ##"$%$&&%(('**)++*--,/.-10/221443654776���������������������ddd	

!! "#"$$#&&%''&))(++*--,..-00/221432554776987������������������������ddd		
	 ! ""!$$#%%$''&))(++*,,+..-0/.110332554765887::9������������������������������		
	

  ""!##"%%$''&(('**),,+.-,//.110321443665887:98;;:���������������������������������	
	

  !! ##"%%$&&%(('**)++*--,//.10/221443665876998;;:=<;������������������������������������	

 !! ##"$$#&&%(('))(++*--,..-00/221432554776998;:9=<;>>=���������������������������������������676	
	!! ""!$$#&&%''&))(++*,,+..-00/210332554765987::9<<;>=<@?>~�����������������������������������������fgf  ""!#$#%%$''&(('**),,+..-//.110332543765887::9<;:>=<??>A@?0N�}��������������������������������������������  !"!##"%%$''&(('**),,+--,//.110321443665876:98;;:==<?>=A@?BBA/N�/O�c|�������������������������������������������>?> !! ##"$%$&&%(('**)++*--,/.-10/221443654776998;:9=<;>>=@@?BA@DCB/N�/P�/Q�Us����������������������������������������������!! "#"$$#&&%''&))(++*--,..-00/221432554776987::9<<;>=<@?>BA@CCBEED/O�/P�/Q�.R�;^����������������������������������������������RSR ! ""!$$#%%$''&))(++*,,+..-0/.110332554765887::9<;:>=<??>AA@CBAEDCGFE/O�/P�.Q�.R�.S�-Uϖ�����������������������������������������������++*  ""!##"%%$''&(('**),,+.-,//.110321443665887:98;;:==<??>A@?CBAEDCFFEHGF/P�/Q�.R�.S�.T�-U�-V�b����������������������������������������������񏏏//.##"%%$&&%(('**)++*--,//.10/221443665876998;;:=<;?>=A@?BBADCBFEDHGFJIH/P�/Q�.R�.S�.T�-U�-V�-W�9cբ�����������������������������������������������vvv&&%(('))(++*--,..-00/221432554776998;:9=<;>>=@@?BA@DCBFEDGFEIHGKJI/P�.Q�.R�.S�-U�-V�-W�,X�,Y�,Z�`��������������������������������������������������zzy887,,+..-00/210332554765987::9<<;>=<@?>BA@CCBEDCGFEIHGJJILKJ/Q�.R�.S�.T�-U�-V�-W�,X�,Y�,Z�+[�+\֕�������������������������������������������������򖖕<<;110332543765887::9<;:>=<??>A@?CBAEDCFFEHGFJIHLKJNML/Q�.R�.S�.T�-U�-V�-W�,Y�,Z�,[�+\�+]�+^�Esݯ�����������������������������������������������������ggf665876:98;;:==<?>=A@?BBADCBFEDHGFJIHKKJMLKONM.Q�.R�.S�-U�-V�-W�,X�,Y�,Z�+[�+\�+]�*^�*_�*`�Du������������������������������������������������������朜�TSR=<;>>=@@?BA@DCBFEDGGFIIHKJIMLKONMQPO.R�.S�.T�-U�-V�-W�,X�,Y�,Z�+[�+\�+]�*_�*`�*a�)b�)c�_���������������������������������������������������������ڔ��ZYXCCBEEDGFEIHGKJIMLKNMLPONRQP.R�.S�.T�-U�-V�-W�,X�,Z�,[�+\�+]�+^�*_�*`�*a�)b�)c�)d�(e�Cy�����������������������������������������������������������箮����UUTLKJNMLPONRQPSRQ.R�.S�-U�-V�-W�,X�,Y�,Z�+[�+\�+]�*^�*_�*`�)a�)b�)d�(e�(f�(g�'h�5r↫���������������������������������������������������������������ݲ�����ihgUTS.S�.T�-U�-V�-W�,X�,Y�,Z�+[�+\�+]�*_�*`�*a�)b�)c�)d�(e�(f�(g�'h�'i�'j�&k�\����������������������������������������������������������������������.S�.T�-U�-V�-W�,X�,Z�,[�+\�+]�+^�*_�*`�*a�)b�)c�)d�(e�(f�(g�'h�'j�'k�&l�&m�&n�%o�i�������������������������������������������������������������.S�-U�-V�-W�,X�,Y�,Z�+[�+\�+]�*^�*_�*`�)a�)b�)d�(e�(f�(g�'h�'i�'j�'k�&l�&m�&n�%o�%p�%q�$r�i����������������������������������������������������.T�-U�-V�-W�,X�,Y�,Z�+[�+\�+]�*_�*`�*a�)b�)c�)d�(e�(f�(g�'h�'i�'j�&k�&l�&m�%n�%o�%q�$r�$s�$t�#u�#v�?��v�����������������������������������������.T�-U�-V�-W�,X�,Z�,[�+\�+]�+^�*_�*`�*a�)b�)c�)d�(e�(f�(g�'h�'j�'k�&l�&m�&n�%o�%p�%q�$r�$s�$t�#u�#v�#w�"x�"y�"z�/��Y�����������������������������-U�-V�-W�,X�,Y�,Z�+[�+\�+]�*^�*_�*`�)a�)b�)c�(e�(f�(g�'h�'i�'j�'k�&l�&m�&n�%o�%p�%q�$r�$s�$t�#u�#v�#w�"x�"z�"{�!|�!}�!~� � � ��<��X�����������-U�-V�-W�,X�,Y�,Z�+[�+\�+]�*_�*`�*a�)b�)c�)d�(e�(f�(g�'h�'i�'j�&k�&l�&m�%n�%o�%q�$r�$s�$t�#u�#v�#w�#x�"y�"z�"{�!|�!}�!~� � �� �� �� �� �� �� ��-U�-V�-W�,X�,Z�,[�+\�+]�+^�*_�*`�*a�)b�)c�)d�(e�(f�(g�'h�'i�'k�&l�&m�&n�%o�%p�%q�$r�$s�$t�#u�#v�#w�"x�"y�"z�!{�!|�!}�!~� � �� �� �� �� �� �� ��-V�-W�,X�,Y�,Z�+[�+\�+]�*^�*_�*`�)a�)b�)c�(e�(f�(g�'h�'i�'j�'k�&l�&m�&n�%o�%p�%q�$r�$s�$t�#u�#v�#w�"x�"y�"{�!|�!}�!~� � � �� �� �� �� �� �� ��-V�-W�,X�,Y�,Z�+[�+\�+]�*_�*`�*a�)b�)c�)d�(e�(f�(g�'h�'i�'j�&k�&l�&m�%n�%o�%q�$r�$s�$t�$u�#v�#w�#x�"y�"z�"{�!|�!}�!~� � �� �� �� �� �� �� �� ��-V�-W�,X�,Z�,[�+\�+]�+^�*_�*`�*a�)b�)c�)d�(e�(f�(g�'h�'i�'k�&l�&m�&n�%o�%p�%q�$r�$s�$t�#u�#v�#w�"x�"y�"z�!{�!|�!}�!~� � �� �� �� �� �� �� �� ��   ,X�,Y�,Z�+[�+\�+]�*^�*_�*`�)a�)b�)c�(e�(f�(g�(h�'i�'j�'k�&l�&m�&n�%o�%p�%q�$r�$s�$t�#u�#v�#w�"x�"y�"z�!|�!}�!~� � � �� �� �� �� �� �� ��         ,Y�,Z�+[�+\�+]�*^�*`�*a�)b�)c�)d�(e�(f�(g�'h�'i�'j�&k�&l�&m�%n�%o�%p�$r�$s�$t�$u�#v�#w�#x�"y�"z�"{�!|�!}�!~� � �� �� �� �� �� �� ��      �      �                                                                                                                                                                                                                                                                                                                                                                      �      �      0   P   �� D X W I Z A R D C O N T R O L F O R M W I Z A R D       0                    �       h   00    �   