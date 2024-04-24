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
            <h2 className="text-2xl font-bold mb-5">Latest Blog Posts</h2>
            <div className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 gap-4">
                {blogPosts.map((post) => (
                    <div key={post.id} className="bg-background dark:bg-gray-700 dark:text-white rounded-lg shadow-lg p-4">
                        <h3 className="text-lg font-bold mb-2">{post.title}</h3>
                        <p className="text-sm text-text mb-2">{post.date}</p>
                        <p className="text-sm mb-4">{post.content}</p>
                        <button className="px-4 py-2 rounded bg-green-500 text-white hover:bg-green-600 focus:outline-none focus:bg-green-600 transition duration-300">
                            Read More
                        </button>
                    </div>
                ))}
            </div>
        </Section>
    );
}
