import Section from "../Section";

interface IProps {
    blogPosts: {
        id: number;
        title: string;
        date: string;
        content: string;
    }[];
}

export default function BlogPosts({ blogPosts }: IProps) {
    return (
        <Section id="blog">
            <h2>Latest Blog Posts</h2>
            <div className="blog-posts">
                {blogPosts.map((post) => (
                    <div key={post.id} className="blog-post">
                        <h3>{post.title}</h3>
                        <p>{post.date}</p>
                        <p>{post.content}</p>
                        <button>Read More</button>
                    </div>
                ))}
            </div>
        </Section>
    );
}




/*
<Section id="blog">
<h2>Latest Blog Posts</h2>
<div className="blog-posts">
  {companyData.blogPosts.map((post) => (
    <div key={post.id} className="blog-post">
      <h3>{post.title}</h3>
      <p>{post.date}</p>
      <p>{post.content}</p>
      <button>Read More</button>
    </div>
  ))}
</div>
</Section>
*/