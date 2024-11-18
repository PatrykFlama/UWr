using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Web;
using System.Web.Mvc;

namespace Zad3.Controllers
{
    public class FileController : Controller
    {
        [HttpGet]
        public ActionResult Upload()
        {
            return View();
        }

        [HttpPost]
        public ActionResult Upload(HttpPostedFileBase file)
        {
            if(file != null && file.ContentLength > 0)
            {
                // Get the file content as a byte array
                using(var memoryStream = new MemoryStream())
                {
                    file.InputStream.CopyTo(memoryStream);
                    byte[] fileData = memoryStream.ToArray();

                    // Return the file as a download response
                    return File(fileData, file.ContentType, file.FileName);
                }
            } else
            {
                ViewBag.Message = "No file selected.";
                return View();
            }
        }


    }
}