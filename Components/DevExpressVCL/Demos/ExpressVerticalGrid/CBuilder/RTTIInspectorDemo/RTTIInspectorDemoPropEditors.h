//---------------------------------------------------------------------------

#ifndef RTTIInspectorDemoPropEditorsH
#define RTTIInspectorDemoPropEditorsH
//---------------------------------------------------------------------------
#include "cxOI.hpp";
#include "cxEdit.hpp";
#include <ImgList.hpp>;
#include "cxImageComboBox.hpp";
#include <Controls.hpp>;
//---------------------------------------------------------------------------
class TcxImageIndexProperty : public TcxIntegerProperty
{
public:
  virtual void __fastcall AdjustInnerEditProperties(Cxedit::TcxCustomEditProperties* AProperties);
  virtual TCustomImageList* __fastcall GetImages(void);
  virtual TcxPropertyAttributes __fastcall GetAttributes(void);
	virtual void __fastcall SetValue(const AnsiString Value);
};
//---------------------------------------------------------------------------
#endif
