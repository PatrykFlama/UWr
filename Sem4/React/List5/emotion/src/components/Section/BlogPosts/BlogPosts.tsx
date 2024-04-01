import styled from '@emotion/styled';
import Section from "../Section";

interface IProps {
    blogPosts: {
        id: number;
        title: string;
        date: string;
        content: string;
    }[];
}

const BlogPostsContainer = styled.div`
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  grid-gap: 20px;
`;

const BlogPost = styled.div`
  border-radius: 10px;
  padding: 20px;
  text-align: left;
  background-color: ${props => props.theme.blogPostBackgroundColor};
  color: ${props => props.theme.textColor};
`;

const BlogPostTitle = styled.h3`
  margin-bottom: 10px;
`;

const BlogPostParagraph = styled.p`
  margin-bottom: 10px;
`;

const Button = styled.button`
  border: none;
  border-radius: 5px;
  cursor: pointer;
  padding: 5px 10px;
  transition: background-color 0.3s ease;
  background-color: ${props => props.theme.buttonBackgroundColor};
  color: ${props => props.theme.buttonTextColor};
  &:hover {
    background-color: ${props => props.theme.buttonHoverBackgroundColor};
  }
`;

export default function BlogPosts({ blogPosts }: IProps) {
    return (
        <Section id="blog">
            <h2>Latest Blog Posts</h2>
            <BlogPostsContainer>
                {blogPosts.map((post) => (
                    <BlogPost key={post.id}>
                        <BlogPostTitle>{post.title}</BlogPostTitle>
                        <BlogPostParagraph>{post.date}</BlogPostParagraph>
                        <BlogPostParagraph>{post.content}</BlogPostParagraph>
                        <Button>Read More</Button>
                    </BlogPost>
                ))}
            </BlogPostsContainer>
        </Section>
    );
}