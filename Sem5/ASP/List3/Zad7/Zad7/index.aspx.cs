using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;

namespace Zad7
{
    public partial class WebForm1 : System.Web.UI.Page
    {
        protected void Page_Load(object sender, EventArgs e)
        {

        }

        protected void btnUpload_Click(object sender, EventArgs e)
        {
            if(!fileUpload.HasFile)
            {
                lblError.Text = "Nie wybrano pliku";
                return;
            }

            try
            {
                var file = fileUpload.PostedFile;

                var size = file.ContentLength;
                byte[] fileBytes;
                using(var ms = new MemoryStream())
                {
                    file.InputStream.CopyTo(ms);
                    fileBytes = ms.ToArray();
                }
                ushort checksum = (ushort)(fileBytes.Sum(b => b) % 0xFFFF);

                // create response file
                var responseContent = $"<opis>\n" +
                                      $"\t<nazwa>{Path.GetFileName(file.FileName)}</nazwa>\n" +
                                      $"\t<rozmiar>{size}</rozmiar>\n" +
                                      $"\t<sygnatura>{checksum}</sygnatura>\n" +
                                      $"</opis>";

                // response
                Response.Clear();
                Response.ContentType = "application/xml";
                Response.AddHeader("Content-Disposition", $"attachment; filename=\"file_info.xml\"");
                Response.Write(responseContent);
                Response.End();
            }
            catch(Exception ex)
            {
                lblError.Text = ex.Message;
            }
        }
    }
}